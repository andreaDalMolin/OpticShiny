##########################
###### LOADING DATA ######
##########################

setwd('C:\\Users\\EXU584\\OneDrive - INFRABEL\\Stage DAL MOLIN\\EMMA\\Code\\Optic\\Optic Alarms Dashboard')

# Load and bind the data
data_files <- c(
  #paste0("Data/CSV/2023/Optic_2023_", sprintf("%02d.csv", 1:12), sep = ""),
  paste0("Data/CSV/2024/Optic_2024_", sprintf("%02d.csv", 1:3), sep = "")
)

# Load the data from CSV files
data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ','))
data <- do.call(rbind, data_list)


##########################
##### PREPPING DATA ######
##########################

data$RAISETIME <- ymd_hms(data$RAISETIME)
data$DATE <- as.Date(data$RAISETIME)
data$TIME <- format(data$RAISETIME, "%H:%M:%S")
data$TIME <- as.POSIXct(data$TIME, format = "%H:%M:%S")
data$HOUR <- format(as.POSIXct(data$RAISETIME), "%H")
data$WEEK <- isoweek(data$DATE)
days_of_week <- c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7)
data$DAY_OF_WEEK <- sapply(weekdays(data$DATE), function(x) days_of_week[x])
day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Step 1: Remove rows where AGENT is NA or ''
data <- data %>%
  filter(!is.na(AGENT) & AGENT != "")

# Step 2: Filter rows based on RAISETIME
start_date <- dmy_hms("02/01/2023 00:00:00")
end_date <- dmy_hms("31/03/2024 23:59:59")
data <- data %>%
  filter(RAISETIME >= start_date & RAISETIME <= end_date)

# Step 3: Adjust week numbering
# First, calculate the year and week number separately
data$Year <- year(data$DATE)
data$WeekOfYear <- isoweek(data$DATE)

# Adjust week numbering so it increments continuously across years
data <- data %>%
  arrange(DATE) %>%
  mutate(WEEK = cummax(Year - min(Year)) * 52 + WeekOfYear)

# Remove the 'Year' and 'WeekOfYear' intermediary columns, keeping the adjusted 'WEEK'
data <- select(data, -c(Year, WeekOfYear))

# Assuming 'days_of_week' and 'day_names' are defined as before
data$DAY_OF_WEEK <- sapply(weekdays(data$DATE), function(x) days_of_week[x])
