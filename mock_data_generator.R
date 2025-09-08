# Mock Qualtrics Data Generator - Catatonia in Intellectual Disability Educational Survey
# Based on actual survey items provided

library(tidyverse)
library(lubridate)

set.seed(42) # For reproducible results

# Set survey parameters
n_responses <- 120  # Expected responses for educational survey
completion_rate <- 0.78  # Realistic completion rate
n_completed <- round(n_responses * completion_rate)

# Generate timestamps (survey launched recently)
generate_timestamps <- function(n, days_ago = 10) {
  start_date <- Sys.Date() - days_ago
  end_date <- Sys.Date()
  
  # More responses in recent days
  weights <- exp(seq(0, 1.5, length.out = days_ago + 1))
  dates <- sample(seq(start_date, end_date, by = "day"), 
                  n, replace = TRUE, prob = weights[1:length(seq(start_date, end_date, by = "day"))])
  
  # Add realistic survey completion times (mostly evenings/weekends)
  times <- sample(c(seq(as.POSIXct("17:00", format = "%H:%M"), 
                        as.POSIXct("22:00", format = "%H:%M"), by = "min"),
                    seq(as.POSIXct("09:00", format = "%H:%M"), 
                        as.POSIXct("15:00", format = "%H:%M"), by = "min")), 
                  n, replace = TRUE, prob = c(rep(2, 301), rep(1, 361)))
  
  datetime <- as.POSIXct(paste(dates, format(times, "%H:%M:%S")))
  return(datetime)
}

# Create the base Qualtrics structure
mock_data <- tibble(
  ResponseId = paste0("R_", sample(1000000:9999999, n_responses)),
  IPAddress = paste(sample(1:255, n_responses, replace = TRUE),
                    sample(1:255, n_responses, replace = TRUE),
                    sample(1:255, n_responses, replace = TRUE),
                    sample(1:255, n_responses, replace = TRUE), sep = "."),
  Progress = c(rep(100, n_completed), 
               sample(15:85, n_responses - n_completed, replace = TRUE)),
  Duration = c(abs(rnorm(n_completed, 800, 200)),  # Completed: ~13 min
               abs(rnorm(n_responses - n_completed, 200, 80))), # Incomplete: ~3 min
  Finished = c(rep(TRUE, n_completed), rep(FALSE, n_responses - n_completed)),
  RecordedDate = generate_timestamps(n_responses),
  ResponseType = "Survey Preview",
  LocationLatitude = runif(n_responses, 25, 49),
  LocationLongitude = runif(n_responses, -125, -65),
  
  # Section 1: Background Questions
  Q1_Main_Role = sample(c("Parent or caregiver", 
                          "Person with intellectual disability",
                          "Medical provider (like a psychiatrist or pediatrician)",
                          "Behavioral therapist",
                          "Teacher or school staff",
                          "Other"), 
                        n_responses, replace = TRUE, 
                        prob = c(0.35, 0.08, 0.25, 0.15, 0.12, 0.05)),
  
  Q1_Other_Role = case_when(
    Q1_Main_Role == "Other" ~ sample(c("Social worker", "Residential care staff", 
                                       "Occupational therapist", "Speech therapist",
                                       "Family friend", "Advocate"), 
                                     sum(Q1_Main_Role == "Other"), replace = TRUE),
    TRUE ~ NA_character_
  ),
  
  Q2_Heard_Of_Catatonia = sample(c("Yes", "No"), n_responses, replace = TRUE, 
                                 prob = c(0.45, 0.55)),
  
  Q3_Cared_For_Someone = sample(c("Yes", "No", "Unsure"), n_responses, replace = TRUE,
                                prob = c(0.25, 0.60, 0.15)),
  
  # Section 2: Handout Content Questions
  Q4_What_Include_Text = sample(c("", 
                                  "More examples of what to look for",
                                  "Emergency contact information",
                                  "How to talk to doctors about this",
                                  "Information for schools",
                                  "Support groups or resources",
                                  "How to prevent episodes",
                                  "Medications to avoid"), 
                                n_responses, replace = TRUE,
                                prob = c(0.15, 0.15, 0.12, 0.18, 0.13, 0.10, 0.08, 0.09)),
  
  # Checklist items (multiple selection - creating binary columns)
  Q5_Include_What_Is = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.1, 0.9)),
  Q5_Include_Signs = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.05, 0.95)),
  Q5_Include_Warning_Signs = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.08, 0.92)),
  Q5_Include_Differences_ID = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.12, 0.88)),
  Q5_Include_Tell_Difference = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.15, 0.85)),
  Q5_Include_What_To_Do = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.06, 0.94)),
  Q5_Include_Treatments = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.18, 0.82)),
  Q5_Include_Urgency = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.10, 0.90)),
  Q5_Include_Where_Help = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.07, 0.93)),
  Q5_Include_Advocate = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.20, 0.80)),
  Q5_Include_Family_Help = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.14, 0.86)),
  
  Q6_Anything_Else_Text = sample(c("",
                                   "Information about insurance coverage",
                                   "Legal rights and protections",
                                   "How to document symptoms",
                                   "Training for staff",
                                   "Cultural considerations",
                                   "Age-specific information"), 
                                 n_responses, replace = TRUE,
                                 prob = c(0.35, 0.12, 0.15, 0.18, 0.08, 0.06, 0.06)),
  
  Q7_Real_Life_Examples = sample(c("Yes", "No"), n_responses, replace = TRUE, 
                                 prob = c(0.88, 0.12)),
  
  Q8_Checklist_Helpful = sample(c("Yes", "No"), n_responses, replace = TRUE,
                                prob = c(0.92, 0.08)),
  
  # Section 3: Format and Style
  Q9_Language_Type = sample(c("Medical and detailed (more like what a doctor would use)",
                              "Easy to understand, with simple words",
                              "Both: a short one-page summary plus more detailed pages after"),
                            n_responses, replace = TRUE,
                            prob = c(0.12, 0.35, 0.53)),
  
  # Format preferences (multiple selection)
  Q10_Format_Print = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.25, 0.75)),
  Q10_Format_PDF = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.15, 0.85)),
  Q10_Format_Webpage = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.35, 0.65)),
  Q10_Format_Checklist = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.45, 0.55)),
  Q10_Format_Video = sample(c(0, 1), n_responses, replace = TRUE, prob = c(0.60, 0.40)),
  
  Q11_Pictures_Help = sample(c("Yes", "No"), n_responses, replace = TRUE,
                             prob = c(0.82, 0.18)),
  
  # Section 4: Open-ended responses
  Q12_Wish_Knew_Earlier = sample(c("",
                                   "How serious this condition is",
                                   "The warning signs to look for",
                                   "That it can happen in people with ID",
                                   "How to get help quickly",
                                   "That it's treatable",
                                   "How to advocate for proper care"), 
                                 n_responses, replace = TRUE,
                                 prob = c(0.20, 0.15, 0.18, 0.12, 0.16, 0.10, 0.09)),
  
  Q13_Biggest_Worry = sample(c("",
                               "Not recognizing it in time",
                               "Doctors not taking it seriously",
                               "Getting proper treatment",
                               "Long-term effects",
                               "It happening again",
                               "My child suffering"), 
                             n_responses, replace = TRUE,
                             prob = c(0.18, 0.22, 0.18, 0.15, 0.10, 0.09, 0.08)),
  
  Q14_Anything_Else = sample(c("",
                               "Contact information for experts",
                               "Support group information",
                               "How to educate others",
                               "Research updates",
                               "Financial resources",
                               "Second opinion guidance"), 
                             n_responses, replace = TRUE,
                             prob = c(0.40, 0.12, 0.15, 0.10, 0.08, 0.08, 0.07)),
  
  Q15_Review_Group_Interest = sample(c("Yes", "No"), n_responses, replace = TRUE,
                                     prob = c(0.35, 0.65)),
  
  # Email collection (only for those interested in review group)
  Q16_Email = case_when(
    Q15_Review_Group_Interest == "Yes" ~ paste0(
      sample(c("parent", "caregiver", "teacher", "provider", "advocate"), 
             sum(Q15_Review_Group_Interest == "Yes"), replace = TRUE),
      sample(100:999, sum(Q15_Review_Group_Interest == "Yes"), replace = TRUE),
      "@", 
      sample(c("gmail.com", "yahoo.com", "outlook.com", "hotmail.com"), 
             sum(Q15_Review_Group_Interest == "Yes"), replace = TRUE)
    ),
    TRUE ~ NA_character_
  )
)

# Add missing data patterns for incomplete responses
incomplete_indices <- which(!mock_data$Finished)

# Create realistic dropout patterns
for (i in incomplete_indices) {
  progress <- mock_data$Progress[i]
  
  # Section 1 questions (Q1-Q3): dropout around 25% progress
  if (progress < 25) {
    mock_data[i, c("Q2_Heard_Of_Catatonia", "Q3_Cared_For_Someone")] <- NA
  }
  
  # Section 2 questions (Q4-Q8): dropout around 50% progress  
  if (progress < 50) {
    section2_cols <- paste0("Q", 4:8, c("_What_Include_Text", "_Include_What_Is", "_Include_Signs", 
                                        "_Include_Warning_Signs", "_Include_Differences_ID", 
                                        "_Include_Tell_Difference", "_Include_What_To_Do", 
                                        "_Include_Treatments", "_Include_Urgency", "_Include_Where_Help",
                                        "_Include_Advocate", "_Include_Family_Help", "_Anything_Else_Text",
                                        "_Real_Life_Examples", "_Checklist_Helpful"))
    available_cols <- intersect(section2_cols, names(mock_data))
    mock_data[i, available_cols] <- NA
  }
  
  # Section 3 questions (Q9-Q11): dropout around 75% progress
  if (progress < 75) {
    section3_cols <- paste0("Q", 9:11, c("_Language_Type", "_Format_Print", "_Format_PDF",
                                         "_Format_Webpage", "_Format_Checklist", "_Format_Video",
                                         "_Pictures_Help"))
    available_cols <- intersect(section3_cols, names(mock_data))
    mock_data[i, available_cols] <- NA
  }
  
  # Section 4 questions (Q12-Q16): dropout around 85% progress
  if (progress < 85) {
    section4_cols <- paste0("Q", 12:16, c("_Wish_Knew_Earlier", "_Biggest_Worry", 
                                          "_Anything_Else", "_Review_Group_Interest", "_Email"))
    available_cols <- intersect(section4_cols, names(mock_data))
    mock_data[i, available_cols] <- NA
  }
}

# Write to CSV
write_csv(mock_data, "mock_catatonia_id_survey_data.csv")

# Display summary
cat("Mock Qualtrics dataset created with", nrow(mock_data), "responses\n")
cat("Completion rate:", round(mean(mock_data$Finished) * 100, 1), "%\n")
cat("Average completion time:", round(mean(mock_data$Duration[mock_data$Finished], na.rm = TRUE) / 60, 1), "minutes\n")

# Show structure
str(mock_data)

# Preview first few rows
head(mock_data, 3)