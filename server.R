# Server functions for Catatonia-ID Survey Dashboard
# This file contains the server-side logic for data processing and downloads

library(shiny)
library(DT)
library(plotly)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(reactable)

# Server function
function(input, output, session) {
  
  # Reactive data loading
  survey_data <- reactive({
    # In production, this could be set up to auto-refresh from Qualtrics API
    data_file <- "mock_catatonia_id_survey_data.csv"
    
    if (file.exists(data_file)) {
      read_csv(data_file, show_col_types = FALSE) %>%
        mutate(
          RecordedDate = as.Date(RecordedDate),
          Duration_minutes = Duration / 60,
          Role_Clean = case_when(
            Q1_Main_Role == "Medical provider (like a psychiatrist or pediatrician)" ~ "Medical Provider",
            Q1_Main_Role == "Person with intellectual disability" ~ "Person with ID",
            is.na(Q1_Main_Role) ~ "Unknown",
            TRUE ~ Q1_Main_Role
          )
        )
    } else {
      # Return empty tibble if no data
      tibble(
        ResponseId = character(0),
        Progress = numeric(0),
        Finished = logical(0),
        RecordedDate = as.Date(character(0)),
        Q1_Main_Role = character(0)
      )
    }
  })
  
  # Download handlers
  output$download_raw <- downloadHandler(
    filename = function() {
      paste0("catatonia_survey_raw_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(survey_data(), file)
    }
  )
  
  output$download_clean <- downloadHandler(
    filename = function() {
      paste0("catatonia_survey_clean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Create cleaned dataset for analysis
      clean_data <- survey_data() %>%
        select(-IPAddress, -LocationLatitude, -LocationLongitude) %>%  # Remove PII
        mutate(
          # Create derived variables for analysis
          completion_status = case_when(
            Finished ~ "Complete",
            Progress >= 75 ~ "Nearly Complete",
            Progress >= 50 ~ "Partial",
            TRUE ~ "Early Dropout"
          ),
          response_quality = case_when(
            Duration_minutes < 2 & Finished ~ "Potentially rushed",
            Duration_minutes > 45 & Finished ~ "Very thorough",
            TRUE ~ "Normal"
          ),
          # Create composite scores
          content_preferences_score = rowSums(select(., starts_with("Q5_Include_")), na.rm = TRUE),
          format_preferences_score = rowSums(select(., starts_with("Q10_Format_")), na.rm = TRUE)
        )
      
      write_csv(clean_data, file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("catatonia_survey_summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data <- survey_data()
      
      # Create workbook
      wb <- createWorkbook()
      
      # Summary sheet
      addWorksheet(wb, "Summary Statistics")
      
      summary_stats <- tibble(
        Metric = c("Total Responses", "Completed Surveys", "Completion Rate (%)", 
                  "Average Completion Time (min)", "Responses Today", "Responses Last 7 Days"),
        Value = c(
          nrow(data),
          sum(data$Finished, na.rm = TRUE),
          round(mean(data$Finished, na.rm = TRUE) * 100, 1),
          round(mean(data$Duration_minutes[data$Finished], na.rm = TRUE), 1),
          sum(data$RecordedDate == Sys.Date(), na.rm = TRUE),
          sum(data$RecordedDate >= Sys.Date() - 6, na.rm = TRUE)
        )
      )
      
      writeData(wb, "Summary Statistics", summary_stats)
      
      # Demographics sheet
      addWorksheet(wb, "Demographics")
      
      demographics <- data %>%
        filter(!is.na(Role_Clean)) %>%
        count(Role_Clean, sort = TRUE) %>%
        mutate(percentage = round(n / sum(n) * 100, 1))
      
      writeData(wb, "Demographics", demographics)
      
      # Content preferences sheet
      addWorksheet(wb, "Content Preferences")
      
      content_cols <- paste0("Q5_Include_", c("What_Is", "Signs", "Warning_Signs", 
                                             "Differences_ID", "Tell_Difference", 
                                             "What_To_Do", "Treatments", "Urgency", 
                                             "Where_Help", "Advocate", "Family_Help"))
      
      content_labels <- c("What catatonia is", "Signs & symptoms", "Warning signs",
                         "Differences in ID", "Tell difference", "What to do first",
                         "Treatments", "Urgency level", "Where to get help", 
                         "How to advocate", "Family support")
      
      content_summary <- tibble(
        Content_Area = content_labels,
        Responses = map_dbl(content_cols[content_cols %in% names(data)], 
                           ~sum(data[[.]], na.rm = TRUE)),
        Percentage = round(Responses / nrow(data) * 100, 1)
      )
      
      writeData(wb, "Content Preferences", content_summary)
      
      # Concerns sheet
      addWorksheet(wb, "Key Concerns")
      
      concerns <- data %>%
        filter(!is.na(Q13_Biggest_Worry) & Q13_Biggest_Worry != "") %>%
        count(Q13_Biggest_Worry, sort = TRUE) %>%
        mutate(percentage = round(n / sum(n) * 100, 1))
      
      writeData(wb, "Key Concerns", concerns)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_codebook <- downloadHandler(
    filename = function() {
      paste0("catatonia_survey_codebook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- survey_data()
      
      codebook <- tibble(
        Variable = names(data),
        Description = case_when(
          Variable == "ResponseId" ~ "Unique response identifier from Qualtrics",
          Variable == "IPAddress" ~ "IP address of respondent",
          Variable == "Progress" ~ "Percentage of survey completed (0-100)",
          Variable == "Duration" ~ "Time spent on survey in seconds",
          Variable == "Finished" ~ "Whether survey was completed (TRUE/FALSE)",
          Variable == "RecordedDate" ~ "Date and time of response",
          Variable == "ResponseType" ~ "Type of response (Survey Preview/IP Address)",
          Variable == "LocationLatitude" ~ "Approximate latitude of respondent",
          Variable == "LocationLongitude" ~ "Approximate longitude of respondent",
          
          # Survey content
          Variable == "Q1_Main_Role" ~ "Primary role/relationship to catatonia-ID topic",
          Variable == "Q1_Other_Role" ~ "Specification when 'Other' selected for main role",
          Variable == "Q2_Heard_Of_Catatonia" ~ "Yes, No",
          Variable == "Q3_Cared_For_Someone" ~ "Yes, No, Unsure", 
          Variable == "Q7_Real_Life_Examples" ~ "Yes, No",
          Variable == "Q8_Checklist_Helpful" ~ "Yes, No",
          Variable == "Q9_Language_Type" ~ "Medical and detailed / Easy to understand / Both options",
          Variable == "Q11_Pictures_Help" ~ "Yes, No",
          Variable == "Q15_Review_Group_Interest" ~ "Yes, No",
          Variable == "Q1_Main_Role" ~ "Parent or caregiver / Person with intellectual disability / Medical provider / Behavioral therapist / Teacher or school staff / Other",
          Variable == "Role_Clean" ~ "Cleaned categories: Parent or caregiver / Person with ID / Medical Provider / Behavioral therapist / Teacher or school staff / Other / Unknown",
          str_starts(Variable, "Q5_Include_") | str_starts(Variable, "Q10_Format_") ~ "1 = Selected, 0 = Not selected",
          Variable %in% c("Progress") ~ "0-100 (percentage)",
          Variable == "Duration" ~ "Seconds (numeric)",
          Variable == "Duration_minutes" ~ "Minutes (derived from Duration)",
          str_detect(Variable, "_Text|_Email") ~ "Free text response",
          TRUE ~ "See data for possible values"
        ),
        Missing_Count = map_dbl(names(data), ~sum(is.na(data[[.]]))),
        Missing_Percent = round(Missing_Count / nrow(data) * 100, 1),
        Unique_Values = map_dbl(names(data), ~length(unique(data[[.]])))
      ) %>%
        arrange(Variable)
      
      write_csv(codebook, file)
    }
  )
  
  # Optional: Auto-refresh data every 5 minutes in production
  # observe({
  #   invalidateLater(300000, session) # 5 minutes
  #   survey_data()
  # })
}atatonia" ~ "Prior knowledge of catatonia in intellectual disability",
          Variable == "Q3_Cared_For_Someone" ~ "Experience caring for someone with catatonia",
          Variable == "Q4_What_Include_Text" ~ "Open text: What should be included in handout",
          
          # Content preferences (binary)
          str_starts(Variable, "Q5_Include_") ~ paste("Binary: Include", str_remove(Variable, "Q5_Include_"), "in handout"),
          
          Variable == "Q6_Anything_Else_Text" ~ "Open text: Additional content suggestions",
          Variable == "Q7_Real_Life_Examples" ~ "Preference for real-life examples in handout",
          Variable == "Q8_Checklist_Helpful" ~ "Preference for symptom tracking checklist",
          Variable == "Q9_Language_Type" ~ "Preferred language/complexity level for handout",
          
          # Format preferences (binary)
          str_starts(Variable, "Q10_Format_") ~ paste("Binary: Prefer", str_remove(Variable, "Q10_Format_"), "format"),
          
          Variable == "Q11_Pictures_Help" ~ "Preference for pictures/visual aids",
          Variable == "Q12_Wish_Knew_Earlier" ~ "Open text: What wish knew earlier about catatonia-ID",
          Variable == "Q13_Biggest_Worry" ~ "Open text: Biggest worry about catatonia",
          Variable == "Q14_Anything_Else" ~ "Open text: Other suggestions for handout",
          Variable == "Q15_Review_Group_Interest" ~ "Interest in participating in review group",
          Variable == "Q16_Email" ~ "Email address for review group (if interested)",
          Variable == "Duration_minutes" ~ "Survey completion time in minutes (derived)",
          Variable == "Role_Clean" ~ "Cleaned version of main role variable (derived)",
          
          TRUE ~ "Variable description not available"
        ),
        Type = case_when(
          Variable %in% c("ResponseId", "IPAddress", "ResponseType") ~ "Identifier",
          Variable %in% c("Progress", "Duration", "Duration_minutes", "LocationLatitude", "LocationLongitude") ~ "Numeric",
          Variable == "Finished" ~ "Logical",
          Variable == "RecordedDate" ~ "Date",
          str_starts(Variable, "Q5_Include_") | str_starts(Variable, "Q10_Format_") ~ "Binary (0/1)",
          Variable %in% c("Q1_Main_Role", "Q2_Heard_Of_Catatonia", "Q3_Cared_For_Someone", 
                         "Q7_Real_Life_Examples", "Q8_Checklist_Helpful", "Q9_Language_Type",
                         "Q11_Pictures_Help", "Q15_Review_Group_Interest", "Role_Clean") ~ "Categorical",
          str_detect(Variable, "_Text|_Email|Q1_Other") ~ "Open Text",
          TRUE ~ "Other"
        ),
        Values = case_when(
          Variable == "Finished" ~ "TRUE = Completed, FALSE = Incomplete",
          Variable == "Q2_Heard_Of_C