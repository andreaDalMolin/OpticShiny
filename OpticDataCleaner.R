###################
#### CONFIGURATION SETUP ####
###################

# Parameters
start_date <- as.Date("2020-01-01")  # Parameter for the start date
source_files_directory <- "../../../../Optic Alarms 2023-2024/New"  # Source files directory
destination_directory <- "home/shiny-app/Data/CSV"  # Destination directory for CSV files
processed_files_path <- "IMPORTED_FILES.txt"  # Path to the file tracking previously processed files

###################
#### FILE TRACKING SETUP ####
###################

processed_files <- if (file.exists(processed_files_path)) readLines(processed_files_path) else character()

###################
#### EXPORTING FUNCTION DEFINITION ####
###################

export_monthly_csv <- function(data) {
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  
  data <- data %>%
    mutate(YearMonth = format(RAISETIME, "%Y_%m"),
           RAISETIME = as.POSIXct(RAISETIME, tz = "UTC", origin = "1970-01-01")) %>%
    mutate(across(where(is.character), ~ gsub("\r|\n", " ", iconv(.x, to = "UTF-8"))))
  
  monthly_data <- split(data, data$YearMonth)
  
  purrr::walk(names(monthly_data), function(name) {
    df_to_write <- monthly_data[[name]][, !names(monthly_data[[name]]) %in% "YearMonth", drop = FALSE]
    filename <- sprintf("%s/Optic_%s.csv", destination_directory, name)
    
    if (file.exists(filename)) {
      existing_data <- tryCatch(read.csv(filename, stringsAsFactors = FALSE),
                                error = function(e) {
                                  warning("Failed to read existing file: ", e$message)
                                  NULL
                                })
      if (!is.null(existing_data)) {
        # Prepare for rbind by ensuring matching data types and columns
        tryCatch({
          existing_data$RAISETIME <- as.POSIXct(existing_data$RAISETIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          all_columns <- union(names(df_to_write), names(existing_data))
          existing_data[setdiff(all_columns, names(existing_data))] <- NA
          df_to_write[setdiff(all_columns, names(df_to_write))] <- NA
          
          existing_data <- existing_data[all_columns]
          df_to_write <- df_to_write[all_columns]
          
          combined_data <- rbind(existing_data, df_to_write)
          write.csv(combined_data, filename, row.names = FALSE, quote = TRUE)
        }, error = function(e) {
          warning("Failed to process and write data for ", name, ": ", e$message)
        })
      }
    } else {
      tryCatch({
        # Format dates before the initial write
        df_to_write$RAISETIME <- format(df_to_write$RAISETIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        write.csv(df_to_write, filename, row.names = FALSE, quote = TRUE)
      }, error = function(e) {
        warning("Failed to write new file ", filename, ": ", e$message)
      })
    }
    
  })
}

###################
#### IMPORTING ####
###################

data_files <- list.files(path = source_files_directory, 
                         pattern = "Optic_202[0-9]_[0-9]{2}_[0-9]{2}.xlsx", 
                         full.names = TRUE, 
                         recursive = TRUE)

new_files <- setdiff(data_files, paste0(source_files_directory, "/", processed_files))

if (length(new_files) > 0) {
  print("New files to be processed:")
  print(basename(new_files))
} else {
  print("No new files to process.")
}

###################
#### CLEANING AND EXPORTING #####
###################

data_list <- lapply(new_files, function(file) {
  tryCatch(read_excel(file), error = function(e) {
    warning("Failed to read file ", file, ": ", e$message)
    NULL  # Return NULL if there's an error reading the file
  })
})

data <- if (length(data_list) > 0) do.call(rbind, data_list) else NULL

if (!is.null(data)) {
  data <- data %>%
    mutate(RAISETIME = tryCatch(dmy_hms(RAISETIME), error = function(e) {
      warning("Date conversion error in RAISETIME: ", e$message)
      NA
    }),
    FIRSTOCCURRENCE = tryCatch(dmy_hms(FIRSTOCCURRENCE), error = function(e) {
      warning("Date conversion error in FIRSTOCCURRENCE: ", e$message)
      NA
    })) %>%
    arrange(RAISETIME) %>%
    filter(RAISETIME >= start_date & RAISETIME <= Sys.Date())
  
  if (nrow(data) > 0) {
    export_monthly_csv(data)
  }
}

# Append new files to the list of processed files and update the file
if (length(new_files) > 0) {
  con <- file(processed_files_path, open = "a")
  tryCatch({
    writeLines(basename(new_files), con)  # Write only the base names
  }, error = function(e) {
    warning("Failed to write processed file names to ", processed_files_path, ": ", e$message)
  }, finally = {
    close(con)  # Ensure the file connection is always closed
  })
}
