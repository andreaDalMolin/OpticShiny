# Load and bind the data
data_files <- c(
  #paste0("Data/CSV/2023/Optic_2023_", sprintf("%02d.csv", 1:12), sep = ""),
  paste0("Data/CSV/2023/Optic_2023_", sprintf("%02d.csv", 12:12), sep = "")
)

# Load the data from CSV files
data_list <- lapply(data_files, function(file) read.csv(file, header = TRUE, sep = ';'))
data_test <- do.call(rbind, data_list)

data_test$RAISETIME <- dmy_hms(data_test$RAISETIME, tz = "EET")
