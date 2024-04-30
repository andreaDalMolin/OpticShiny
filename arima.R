calculate_agent_overlap_statistics <- function(data, start_datetime, end_datetime, customThreshold) {
  # Extract unique agents
  agents <- unique(data$AGENT)
  
  # Get the total number of agents for progress tracking
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
  
  # Format datetime columns to include both date and time
  surge_periods$Start <- format(surge_periods$Start, "%d/%m/%Y %H:%M:%S")
  surge_periods$End <- format(surge_periods$End, "%d/%m/%Y %H:%M:%S")
  
  # Now write the formatted surge_periods to CSV
  write.csv(surge_periods, "surge_periods.csv", row.names = FALSE)
  
  
}







start_datetime <- "2024-01-01 00:00:00"
end_datetime <- "2024-03-31 23:59:59"

overlap_statistics <- calculate_agent_overlap_statistics(data, start_datetime, end_datetime, 1.5)


filter_agent_overlaps <- function(overlap_data, agent_name) {
  filtered_data <- overlap_data %>%
    filter(Agent1 == agent_name | Agent2 == agent_name)
  
  filtered_data <- filtered_data %>%
    mutate(
      NewAgent1 = ifelse(Agent2 == agent_name, Agent2, Agent1),
      NewAgent2 = ifelse(Agent2 == agent_name, Agent1, Agent2),
      NewN = n
    ) %>%
    select(Agent1 = NewAgent1, Agent2 = NewAgent2, n = NewN)  # Reorder and clean columns
  
  return(filtered_data)
}



emma_overlaps <- filter_agent_overlaps(overlap_statistics, "EMMA")

emma_overlaps <- subset(emma_overlaps, select = -Agent1)

interactive_pie_chart <- plot_ly(emma_overlaps, labels = ~Agent2, values = ~n, type = 'pie',
                                 textinfo = 'label+percent',
                                 insidetextorientation = 'radial',
                                 marker = list(colors = rainbow(length(emma_overlaps$Agent2))))

# Print the interactive chart
interactive_pie_chart












surge_data <- read_csv("surge_periods.csv")

surge_data$Start <- as.numeric(ymd_hms(surge_data$Start))
surge_data$End <- as.numeric(ymd_hms(surge_data$End))

emma_surges <- surge_data[surge_data$Filter == "EMMA",]

analyze_overlaps <- function(surge_data, agent_name, output_csv = "overlap_details.csv") {
  
  # Ensure Start and End are character to prevent type mismatch when adding rows
  surge_data$Start <- as.character(surge_data$Start)
  surge_data$End <- as.character(surge_data$End)
  
  agent_surges <- surge_data[surge_data$Filter == agent_name,]
  other_agents_surges <- surge_data[surge_data$Filter != agent_name,]

  # Initialize a data frame to store individual overlaps
  overlaps <- tibble(Filter1 = character(), Filter2 = character(), Start1 = character(), End1 = character(), Start2 = character(), End2 = character())
  
  # Compare the specific agent's surge periods with all other surge periods to find overlaps
  for (agent_surge in 1:nrow(agent_surges)) {
    for (other_surge in 1:nrow(other_agents_surges)) {
      if (max(as.POSIXct(agent_surges$Start[agent_surge]), as.POSIXct(other_agents_surges$Start[other_surge])) < 
          min(as.POSIXct(agent_surges$End[agent_surge]), as.POSIXct(other_agents_surges$End[other_surge]))) {
        # Record the individual overlap
        overlaps <- overlaps %>%
          add_row(Filter1 = agent_surges$Filter[agent_surge], 
                  Filter2 = other_agents_surges$Filter[other_surge],
                  Start1 = agent_surges$Start[agent_surge], 
                  End1 = agent_surges$End[agent_surge],
                  Start2 = other_agents_surges$Start[other_surge], 
                  End2 = other_agents_surges$End[other_surge])
      }
    }
  }
  
  # Write the individual overlaps to a CSV file
  write_csv(overlaps, output_csv)
  
  # Return the overlaps data frame (optional)
  return(overlaps)
}

analyze_overlaps <- function(agent_name, output_csv = "overlap_details.csv") {
  
  #Import surge periods file
  surge_data <- read.csv("surge_periods.csv")
  
  setDT(surge_data)
  surge_data[, `:=` (Start = as.POSIXct(Start, format = "%Y-%m-%d %H:%M:%S"),
                     End = as.POSIXct(End, format = "%Y-%m-%d %H:%M:%S"))]
  
  agent_surges <- surge_data[Filter == agent_name]
  other_agents_surges <- surge_data[Filter != agent_name]
  
  # Set keys for joining by time intervals
  setkey(agent_surges, Start, End)
  setkey(other_agents_surges, Start, End)
  
  # Find overlaps using foverlaps, which respects the exact times
  overlaps <- foverlaps(agent_surges, other_agents_surges, type = "any", which = TRUE, nomatch = 0L)
  if (nrow(overlaps) > 0) {
    overlaps_result <- agent_surges[overlaps$xid, .(Filter1 = Filter, Start1 = Start, End1 = End)]
    overlaps_other <- other_agents_surges[overlaps$yid, .(Filter2 = Filter, Start2 = Start, End2 = End)]
    final_overlaps <- cbind(overlaps_result, overlaps_other)
    # Write to CSV using fwrite for efficiency
    fwrite(final_overlaps, output_csv)
    return(final_overlaps)
  } else {
    return(NULL)  # Return NULL if no overlaps found
  }
}

overlap_details <- analyze_overlaps("EMMA", "overlap_details.csv")

