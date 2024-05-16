refresh_data_files <- function() {
  
  # Configuration Setup
  start_date <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  source_files_directory <- "/home/shiny-app/External/CSV/Data"
  destination_directory <- "/home/shiny-app/Data/CSV"
  processed_files_path <- "IMPORTED_FILES.txt"
  exported_files_path <- "EXPORTED_FILES.txt"
  column_types <- rep("text", 36)
  
  # Read already processed files
  processed_files <- if (file.exists(processed_files_path)) readLines(processed_files_path) else character()
  
  # Function to sanitize column names
  sanitize_column_names <- function(df) {
    names(df) <- gsub("[[:space:][:punct:]]", "", names(df))
    df
  }
  
  # Function to read and clean Excel files
  read_and_clean_excel <- function(file_path) {
    data <- read_excel(file_path, col_types = column_types)
    sanitize_column_names(data)
  }
  
  # Function to convert all columns to character type
  convert_to_character <- function(df) {
    df[] <- lapply(df, as.character)
    df
  }
  
  # Function to export data to monthly CSV
  export_monthly_csv <- function(data) {
    dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
    
    data <- data %>%
      mutate(YearMonth = format(as.POSIXct(RAISETIME, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"), "%Y_%m")) %>%
      split(.$YearMonth)
    
    exported_files <- character()
    
    walk(names(data), function(month) {
      filename <- file.path(destination_directory, paste0("Optic_", month, ".csv"))
      
      if (file.exists(filename)) {
        existing_data <- read.csv(filename, stringsAsFactors = FALSE)
        existing_data <- convert_to_character(existing_data)
        data[[month]] <- bind_rows(existing_data, data[[month]])
      }
      
      write.csv(data[[month]], filename, row.names = FALSE)
      exported_files <<- c(exported_files, basename(filename))
    })
    
    # Read already exported files
    existing_exported_files <- if (file.exists(exported_files_path)) readLines(exported_files_path) else character()
    
    # Determine new files to write
    new_exported_files <- setdiff(exported_files, existing_exported_files)
    
    if (length(new_exported_files) > 0) {
      # Write new exported files to EXPORTED_FILES.txt
      con <- file(exported_files_path, open = "a")
      tryCatch({
        writeLines(new_exported_files, con)
      }, finally = {
        close(con)
      })
    }
  }
  
  # Get new files to process
  data_files <- list.files(path = source_files_directory, pattern = "Optic_202[0-9]_[0-9]{2}_[0-9]{2}.xlsx", full.names = TRUE)
  new_files <- setdiff(basename(data_files), processed_files)
  
  if (length(new_files) == 0) {
    message("No new files to process.")
    return(invisible())
  }
  
  # message("New files to process: ", paste(new_files, collapse = ", "))
  
  # Read and combine new data files
  data_list <- map(file.path(source_files_directory, new_files), read_and_clean_excel)
  data_list <- map(data_list, convert_to_character)  # Ensure all data is character type
  
  # # Print data summary for debugging
  # print("Data summary before binding rows:")
  # print(lapply(data_list, summary))
  
  data <- bind_rows(data_list)
  
  # # Print combined data summary for debugging
  # print("Combined data summary:")
  # print(summary(data))
  
  # Convert RAISETIME to POSIXct for filtering
  data <- data %>%
    mutate(RAISETIME = as.POSIXct(RAISETIME, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")) %>%
    filter(RAISETIME >= start_date, RAISETIME <= Sys.time())
  
  # # Print filtered data summary for debugging
  # print("Filtered data summary:")
  # print(summary(data))
  
  # Revert RAISETIME to text for export
  data <- data %>%
    mutate(RAISETIME = format(RAISETIME, "%d/%m/%Y %H:%M:%S"))
  
  if (nrow(data) > 0) {
    export_monthly_csv(data)
    message("Data successfully written to CSV files.")
  } else {
    message("No data to write to CSV files.")
  }
  
  # Update processed files list
  con <- file(processed_files_path, open = "a")
  tryCatch({
    writeLines(new_files, con)
  }, finally = {
    close(con)
  })
  message("Processed files list updated.")
}

refresh_data_files()
