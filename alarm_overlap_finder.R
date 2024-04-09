# find_overlapping_alarms <- function(surge_periods) {
#   surge_periods <- surge_periods[order(surge_periods$Start),]
#   
#   overlap_info <- list()
#   
#   for (i in 1:nrow(surge_periods)) {
#     # Current surge period to compare others against
#     current_period <- surge_periods[i, ]
#     
#     # Find potential overlaps: other periods that start before current period ends
#     potential_overlaps <- surge_periods[surge_periods$Start <= current_period$End, ]
#     
#     # Loop through each potential overlap
#     for (j in 1:nrow(potential_overlaps)) {
#       other_period <- potential_overlaps[j, ]
#       
#       # Check for actual overlap and that it's not the same agent or the same period
#       if (other_period$End >= current_period$Start &&
#           other_period$Filter != current_period$Filter) {
#         
#         # Record the overlap
#         overlap_info <- c(overlap_info, list(
#           list(
#             Agent1 = current_period$Filter,
#             Agent2 = other_period$Filter,
#             OverlapStart = max(current_period$Start, other_period$Start),
#             OverlapEnd = min(current_period$End, other_period$End)
#           )
#         ))
#       }
#     }
#   }
#   
#   return(overlap_info)
# }


set.seed(123) # For reproducibility

# Generate sample data
n <- 100 # Number of rows
agents <- c("EMMA", "Zabbix", "SONIA", "LUKE") # Agent names
start_times <- as.POSIXct("2024-02-05") + runif(n, min = 0, max = 7 * 24 * 3600) # Start times spread over a week
end_times <- start_times + runif(n, min = 3600, max = 3 * 3600) # End times 1 to 3 hours after start times

# Create the data frame
sample_surge_periods <- data.frame(
  Start = start_times,
  End = end_times,
  Filter = sample(agents, n, replace = TRUE)
)

# Ensure data is ordered by start time
sample_surge_periods <- sample_surge_periods[order(sample_surge_periods$Start),]

# Find overlaps
overlaps <- find_overlapping_alarms(sample_surge_periods)

# Assuming 'overlaps' is your list of overlap information
overlaps_df <- do.call(rbind, lapply(overlaps, function(overlap) {
  data.frame(
    Agent1 = overlap$Agent1,
    Agent2 = overlap$Agent2,
    OverlapStart = overlap$OverlapStart,
    OverlapEnd = overlap$OverlapEnd,
    stringsAsFactors = FALSE # Avoid factors for Agent names
  )
}))

# Calculate the duration of each overlap in minutes
overlaps_df$Duration <- as.numeric(difftime(overlaps_df$OverlapEnd, overlaps_df$OverlapStart, units = "mins"))

head(overlaps_df)