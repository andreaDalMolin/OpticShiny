library(lubridate)
library(dplyr)
library(purrr)


###################
#### IMPORTING ####
###################

setwd('C:\\Users\\EXU584\\OneDrive - INFRABEL\\Stage DAL MOLIN\\EMMA\\Code\\Optic\\Optic Alarms Dashboard')

# List all files in the directory that match the naming pattern "Optic_202x_[month_number].csv"
data_files <- list.files(path = "Data/CSV", pattern = "Optic_202[0-9]_[0-9]{2}.csv", full.names = TRUE, recursive = TRUE)

# Load the data from CSV files
data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ','))

# Combine all data frames into one data frame
data_test <- do.call(rbind, data_list)


###################
#### CLEANING #####
###################

data_test$RAISETIME <- dmy_hms(data_test$RAISETIME)
data_test$FIRSTOCCURRENCE <- dmy_hms(data_test$FIRSTOCCURRENCE)

data_sorted <- data_test %>% arrange(RAISETIME)

# ADJUST THE DATES ACCORDINGLY !
data_in_bounds <- data_sorted %>% filter(RAISETIME >= as.Date("2023-01-01") & RAISETIME <= "2024-04-02")


###################
#### EXPORTING ####
###################

# Import example
optic_2024_03 <- read.csv("Optic_2024_03.csv", header = TRUE, sep = ",")


export_unique_agents <- function(df, filename) {
  if(!"AGENT" %in% names(df)) {
    stop("The dataframe does not contain the 'AGENT' column.")
  }
  
  unique_agents <- unique(df$AGENT)
  
  unique_agents_df <- data.frame(AGENT = unique_agents)
  
  write.csv(unique_agents_df, filename, row.names = FALSE)
  
  cat("File saved as:", filename)
}

export_unique_agents(data_test, "unique_agents.csv")


