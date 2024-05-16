# Set the working directory to ensure relative paths are correct
setwd("/home/shiny-app")

# Source necessary files with absolute paths
source("/home/shiny-app/global.R")
source("/home/shiny-app/OpticDataCleaner.R")

# Your function definition and execution
calculate_agent_overlap_statistics <- function(data, start_datetime, end_datetime, customThreshold) {
  agents <- unique(data$AGENT)
  
  total_agents <- length(agents)
  
  # Initialize an empty list to store surge periods for each agent
  surge_periods_list <- list()
  
  # Calculate surge periods for each agent
  for (i in seq_along(agents)) {
    cat(sprintf("Calculating surge periods for agent %d/%d...\n", i, total_agents))
    # Assuming calculate_surge_periods can handle one agent at a time
    agent_surge_periods <- calculate_surge_periods(data, start_datetime, end_datetime, customThreshold, agents[i])
    surge_periods_list[[i]] <- agent_surge_periods
    cat(sprintf("Completed agent %d/%d.\n", i, total_agents))
  }
  
  # Combine all surge periods into one dataframe
  surge_periods <- do.call(rbind, surge_periods_list)
  
  # Modify the Filter column and format the datetime columns
  surge_periods$Start <- format(as.POSIXct(surge_periods$Start), "%d/%m/%Y %H:%M:%S")
  surge_periods$End <- format(as.POSIXct(surge_periods$End), "%d/%m/%Y %H:%M:%S")
  
  write.csv(surge_periods, "surge_periods.csv", row.names = FALSE, quote = TRUE)
}

# Load the data and run the function
load_data <- function() {
  refresh_data_files()
  
  # List all files in the directory that match the naming pattern "Optic_202x_[month_number].csv"
  data_files <- list.files(path = "/home/shiny-app/Data/CSV", pattern = "Optic_202[0-9]_[0-9]{2}.csv", full.names = TRUE, recursive = TRUE)
  
  # Load the data from CSV files
  data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ','))
  
  # Combine all data frames into one data frame
  data <- do.call(rbind, data_list)
  
  # Prepping data
  data$RAISETIME <- dmy_hms(data$RAISETIME)
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
  end_date <- Sys.Date()
  data <- data %>%
    filter(RAISETIME >= start_date & RAISETIME <= end_date)
  
  # Assuming 'days_of_week' and 'day_names' are defined as before
  data$DAY_OF_WEEK <- sapply(weekdays(data$DATE), function(x) days_of_week[x])
  
  # Write imported files to LIVEDATA.txt avoiding duplicates
  imported_files_path <- "LIVEDATA.txt"
  existing_imported_files <- if (file.exists(imported_files_path)) readLines(imported_files_path) else character()
  new_imported_files <- setdiff(basename(data_files), existing_imported_files)
  
  if (length(new_imported_files) > 0) {
    con <- file(imported_files_path, open = "a")
    tryCatch({
      writeLines(new_imported_files, con)
    }, finally = {
      close(con)
    })
  }
  
  # Call calculate_agent_overlap_statistics function
  calculate_agent_overlap_statistics(data, "2020-01-01 00:00:00", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 1.5)
}

# Execute the data load and calculation
load_data()
