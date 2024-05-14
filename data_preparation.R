##########################
###### LOADING DATA ######
##########################

load_data <- function() {
  
  # List all files in the directory that match the naming pattern "Optic_202x_[month_number].csv"
  # ADD A SLASH AT THE START OF THE PATH TO RUN IN DOCKER
  data_files <- list.files(path = "home/shiny-app/Data/CSV", pattern = "Optic_202[0-9]_[0-9]{2}.csv", full.names = TRUE, recursive = TRUE)
  
  # Load the data from CSV files
  data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ','))
  
  # Combine all data frames into one data frame
  data <- do.call(rbind, data_list)
  
  
  ##########################
  ##### PREPPING DATA ######
  ##########################
  
  data$RAISETIME <- ymd_hms(data$RAISETIME)
  data$DATE <- as.Date(data$RAISETIME)
  data$TIME <- format(data$RAISETIME, "%H:%M:%S")
  data$TIME <- as.POSIXct(data$TIME, format = "%H:%M:%S")
  data$HOUR <- format(as.POSIXct(data$RAISETIME), "%H")
  days_of_week <- c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7)
  data$DAY_OF_WEEK <- sapply(weekdays(data$DATE), function(x) days_of_week[x])
  day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  # Step 1: Remove rows where AGENT is NA or ''
  data <- data %>%
    filter(!is.na(AGENT) & AGENT != "")
  
  # Step 2: Filter rows based on RAISETIME
  start_date <- dmy_hms("01/01/2020 00:00:00")
  end_date <- Sys.Date() # TODO change this to current date
  data <- data %>%
    filter(RAISETIME >= start_date & RAISETIME <= end_date)
  
  # Assuming 'days_of_week' and 'day_names' are defined as before
  data$DAY_OF_WEEK <- sapply(weekdays(data$DATE), function(x) days_of_week[x])
  
  data
}

