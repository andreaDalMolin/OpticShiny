library(lubridate)
library(dplyr)
library(purrr)


###################
#### IMPORTING ####
###################

# List all files in the directory that match the naming pattern "Optic_202x_[month_number].csv"
data_files <- list.files(path = "../../../../Optic Alarms 2023-2024/New", pattern = "Optic_202[0-9]_[0-9]{2}.csv", full.names = TRUE, recursive = TRUE)

# Load the data from CSV files
data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ';'))

# Combine all data frames into one data frame
data <- do.call(rbind, data_list)

###################
#### CLEANING #####
###################

data$RAISETIME <- dmy_hms(data$RAISETIME)
data$FIRSTOCCURRENCE <- dmy_hms(data$FIRSTOCCURRENCE)

data_sorted <- data %>% arrange(RAISETIME)

# ADJUST THE DATES ACCORDINGLY !
data_in_bounds <- data_sorted %>% filter(RAISETIME >= as.Date("2023-01-01") & RAISETIME <= "2024-05-30")


###################
#### EXPORTING ####
###################

export_monthly_csv(data_in_bounds)

# Import example
optic_2024_03 <- read.csv("Data/CSV/2023/Optic_2023_01.csv", header = TRUE, sep = ";")


export_monthly_csv <- function(data) {
  data <- data %>%
    mutate(YearMonth = format(RAISETIME, "%Y_%m")) %>%
    
    # Convert encoding to UTF-8 and clean data by replacing or removing carriage returns and new lines
    # This is done to avoid carriage-return problems when opening the CSV in Excel, ask me how I know :)
    mutate(across(where(is.character), ~ gsub("\r|\n", " ", iconv(.x, to = "UTF-8"))))
  
  # Split the data into a list of dataframes, each containing one month's data
  monthly_data <- split(data, data$YearMonth)
  
  # Use purrr::walk to iterate over the list and write each dataframe to a CSV file
  purrr::walk(names(monthly_data), function(name) {
    # Remove the 'YearMonth' column before writing to CSV
    df_to_write <- monthly_data[[name]][, !names(monthly_data[[name]]) %in% "YearMonth", drop = FALSE]
    filename <- sprintf("Optic_%s.csv", name)
    write.csv(df_to_write, filename, row.names = FALSE, quote = TRUE)
  })
}
