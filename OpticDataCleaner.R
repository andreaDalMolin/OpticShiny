library(lubridate)
library(dplyr)
library(purrr)


###################
#### IMPORTING ####
###################

# Load and bind the data
data_files <- c(
  paste0("Data/CSV/2023/Optic_2023_", sprintf("%02d.csv", 1:12), sep = ""),
  paste0("Data/CSV/2024/Optic_2024_", sprintf("%02d.csv", 1:3), sep = "")
)

# Load the data from CSV files
data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ';'))
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

export_monthly_csv(data_in_bounds)

# Import example
optic_2024_03 <- read.csv("Optic_2024_03.csv", header = TRUE, sep = ",")


export_monthly_csv <- function(data) {
  # Create a new column 'YearMonth' to store the combined year and month in 'YYYY_MM' format
  data <- data %>%
    
    mutate(YearMonth = format(RAISETIME, "%Y_%m")) %>%
    
    # Convert encoding to UTF-8 and clean data by replacing or removing carriage returns and new lines
    # This is done to avoid carriage-return problems when opening the CSV in Excel, ask me how I know :)
    mutate(across(where(is.character), ~ gsub("\r|\n", " ", iconv(.x, to = "UTF-8"))))
  
  # Split the data into a list of dataframes, each containing one month's data
  monthly_data <- split(data, data$YearMonth)
  
  # Use purrr::walk to iterate over the list and write each dataframe to a CSV file
  purrr::walk(names(monthly_data), function(name) {
    filename <- sprintf("Optic_%s.csv", name)
    write.csv(monthly_data[[name]], filename, row.names = FALSE, quote = TRUE)
  })
}