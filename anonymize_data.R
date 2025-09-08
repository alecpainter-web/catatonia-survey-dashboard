# Data Anonymization Script for Catatonia Survey Dashboard
# This script removes all personally identifiable information (PII)
# Run this on your raw Qualtrics data before committing to Git

library(tidyverse)

anonymize_survey_data <- function(raw_data_file, output_file = "anonymized_survey_data.csv") {
  
  cat("Reading raw survey data...\n")
  raw_data <- read_csv(raw_data_file, show_col_types = FALSE)
  
  cat("Anonymizing data...\n")
  
  # Remove/anonymize all PII fields
  anonymized_data <- raw_data %>%
    
    # Remove direct identifiers
    select(-any_of(c("IPAddress", "LocationLatitude", "LocationLongitude", 
                     "Email", "Q16_Email", "email", "RecipientLastName", 
                     "RecipientFirstName", "RecipientEmail", "ExternalReference"))) %>%
    
    # Generate new anonymous response IDs
    mutate(
      ResponseId = paste0("ANON_", row_number()),
      
      # Remove timestamps that could identify individuals
      # Keep only date (not time) for analysis
      RecordedDate = if("RecordedDate" %in% names(.)) {
        as.Date(RecordedDate)
      } else {
        as.Date(Sys.Date())
      },
      
      # Remove any open text responses that might contain identifying info
      # Replace with categorical summaries where possible
      across(contains("_Text"), ~ case_when(
        is.na(.) | . == "" ~ "",
        str_detect(tolower(.), "email|phone|address|name") ~ "[REMOVED - CONTAINED PII]",
        TRUE ~ .
      ))
    ) %>%
    
    # Remove any other potentially identifying columns
    select(-any_of(c("UserLanguage", "Browser", "Version", "Operating System", 
                     "Resolution", "StartDate", "EndDate", "Status", 
                     "RecipientFirstName", "RecipientLastName")))
  
  # Check for any remaining text fields that might contain PII
  text_columns <- anonymized_data %>% 
    select(where(is.character)) %>% 
    names()
  
  cat("Text columns to review for PII:\n")
  cat(paste(text_columns, collapse = ", "), "\n")
  
  # Write anonymized data
  write_csv(anonymized_data, output_file)
  
  cat("\nAnonymization complete!\n")
  cat("Original data:", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")
  cat("Anonymized data:", nrow(anonymized_data), "rows,", ncol(anonymized_data), "columns\n")
  cat("Saved to:", output_file, "\n")
  
  # Return summary of what was removed
  removed_columns <- setdiff(names(raw_data), names(anonymized_data))
  cat("\nRemoved columns:", paste(removed_columns, collapse = ", "), "\n")
  
  return(anonymized_data)
}

# Usage example:
# anonymized_data <- anonymize_survey_data("raw_qualtrics_export.csv")

# Quick check function to scan for potential PII
check_for_pii <- function(data) {
  cat("Checking for potential PII...\n")
  
  # Check for email patterns
  email_pattern <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"
  
  # Check for phone patterns  
  phone_pattern <- "\\b\\d{3}[-.]?\\d{3}[-.]?\\d{4}\\b"
  
  text_cols <- data %>% select(where(is.character)) %>% names()
  
  for (col in text_cols) {
    if (any(str_detect(data[[col]], email_pattern, na.rm = TRUE))) {
      cat("WARNING: Potential email addresses found in column:", col, "\n")
    }
    if (any(str_detect(data[[col]], phone_pattern, na.rm = TRUE))) {
      cat("WARNING: Potential phone numbers found in column:", col, "\n")
    }
  }
  
  cat("PII check complete.\n")
}

# Example workflow:
cat("To use this script:\n")
cat("1. Place your raw Qualtrics CSV in this folder\n")
cat("2. Run: anonymized_data <- anonymize_survey_data('your_raw_file.csv')\n")
cat("3. Run: check_for_pii(anonymized_data)\n")
cat("4. Only commit the anonymized file to Git\n")