# Function to create histogram by time
create_histogram_by_time <- function(data, title_prefix) {
  ggplot(data, aes(x = hour(TIME))) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste(title_prefix, "- Number of Alarms by Time of Day"),
         x = "Hour of Day",
         y = "Frequency") +
    theme_minimal()
}

# Function to create bar plot by day
create_barplot_by_day <- function(data, title_prefix) {
  ggplot(data, aes(x = DAY_OF_WEEK)) +
    geom_bar(stat = "count", fill = "blue", color = 'black') +
    labs(title = paste(title_prefix, "- Number of Alarms by Day of the Week"), 
         x = "Day of Week", 
         y = "Number of Events") +
    scale_x_discrete(drop = FALSE) +
    theme_minimal()
}

create_heatmap_by_hour_day <- function(data, start_date, end_date, agents) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  plot_list <- list()
  data_list <- list()
  
  for (agent in agents) {
    agent_data <- if (agent != '') {
      data[data$AGENT == agent,]
    } else {
      data
    }
    
    timeframe_data <- agent_data[agent_data$RAISETIME >= as.POSIXct(start_date) & agent_data$RAISETIME <= as.POSIXct(paste(end_date, "23:59:59")),]
    
    # Count the number of events for each combination of DAY_OF_WEEK and HOUR
    event_counts <- timeframe_data %>%
      dplyr::group_by(DAY_OF_WEEK, HOUR) %>%
      dplyr::summarise(Count = n(), .groups = 'drop')
    
    # Adjust DAY_OF_WEEK to factor with reversed labels for plotting
    event_counts$DAY_OF_WEEK <- factor(event_counts$DAY_OF_WEEK, levels = 7:1, labels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    
    # Create the heatmap plot
    p <- ggplot(event_counts, aes(x = HOUR, y = DAY_OF_WEEK, fill = Count)) + 
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste(agent, "- Number of Alarms by Hour and Day of the Week"), 
                    x = "Hour of the Day", 
                    y = "Day of the Week", 
                    fill = "Number of Events") +
      theme_minimal()
    
    plot_list[[agent]] <- p
    data_list[[agent]] <- event_counts
  }
  
  heatmap_data <- list(plot_list = plot_list, data_list = data_list)
  
  return(heatmap_data)
}

create_heatmap_for_week <- function(data, start_date, agents) {
  start_date <- as.Date(start_date)
  end_date <- start_date + 6
  
  plot_list <- list()
  data_list <- list()
  
  for (agent in agents) {
    agent_data <- if (agent != '') {
      data[data$AGENT == agent,]
    } else {
      data
    }
    
    week_data <- agent_data[agent_data$RAISETIME >= as.POSIXct(start_date) & agent_data$RAISETIME <= as.POSIXct(paste(end_date, "23:59:59")),]
    
    week_data <- week_data %>%
      dplyr::mutate(
        DAY = as.factor(strftime(RAISETIME, "%Y-%m-%d")),
        HOUR = lubridate::hour(RAISETIME)
      )
    
    all_days <- seq(from = start_date, to = end_date, by = "day")
    all_hours <- 0:23
    full_grid <- expand.grid(DAY = all_days, HOUR = all_hours)
    full_grid$DAY <- as.factor(strftime(full_grid$DAY, "%Y-%m-%d"))
    
    event_counts <- week_data %>%
      dplyr::group_by(DAY, HOUR) %>%
      dplyr::summarise(Count = n(), .groups = 'drop')
    
    # Merge event counts with the full grid, filling in zeros where there are no events
    event_counts <- full_grid %>%
      left_join(event_counts, by = c("DAY", "HOUR")) %>%
      replace_na(list(Count = 0))
    
    # Reorder DAY factor levels in descending order
    event_counts$DAY <- factor(event_counts$DAY, levels = rev(levels(event_counts$DAY)))
    
    # Generate the heatmap
    p <- ggplot(event_counts, aes(x = HOUR, y = DAY, fill = Count)) +
      geom_tile() +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
      scale_fill_gradientn(colors = c("#FFFFFF00", "lightblue", "darkblue"),
                           values = scales::rescale(c(0, 1, max(event_counts$Count, na.rm = TRUE))),
                           na.value = "#FFFFFF00") +
      labs(title = sprintf("%s - Alarms from %s to %s - Number of Alarms by Hour and Day",
                           agent,
                           format(start_date, "%d/%m/%Y"),
                           format(end_date, "%d/%m/%Y")),
           x = "Hour of the Day",
           y = "Date",
           fill = "Number of Events") +
      theme_minimal()
    
    plot_list[[agent]] <- p
    data_list[[agent]] <- event_counts
  }
  
  heatmap_data <- list(plot_list = plot_list, data_list = data_list)
  
  return(heatmap_data)
}

# The is_cumulative bool is to differentiate in graphs contruction
merge_heatmaps <- function(data_frames, is_cumulative) {
  # TODO Check that all dataframes have the same shape

  
  # Normalize the Count values of each dataset
  data_frames <- lapply(data_frames, function(df) {
    max_count <- max(df$Count, na.rm = TRUE)
    df$Count <- df$Count / max_count  # Normalize by the maximum count in each dataset
    df
  })
  
  # For each day/hour of day, multiply the value of each agent for that specific day/time
  # Assuming that all dataframes have exactly the same rows in the same order
  merged_data <- data_frames[[1]]
  if (length(data_frames) > 1) {
    for (i in 2:length(data_frames)) {
      merged_data$Count <- merged_data$Count * data_frames[[i]]$Count
    }
  }
  
  p <- NULL
  
  if (is_cumulative) {
    p <- ggplot(merged_data, aes(x = HOUR, y = DAY_OF_WEEK, fill = Count)) + 
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Merged", "- Number of Alarms by Hour and Day of the Week"), 
           x = "Hour of the Day", 
           y = "Day of the Week", 
           fill = "Number of Events") +
      theme_minimal()
  } else {
    p <- ggplot(merged_data, aes(x = HOUR, y = DAY, fill = Count)) +
      geom_tile() +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
      scale_fill_gradientn(colors = c("#FFFFFF00", "lightblue", "darkblue"),
                           values = scales::rescale(c(0, 1, max(merged_data$Count, na.rm = TRUE))),
                           na.value = "#FFFFFF00") +
      labs(title = sprintf("Merged - Alarms from %s to %s - Number of Alarms by Hour and Day",
                           format(start_date, "%d/%m/%Y"),
                           format(end_date, "%d/%m/%Y")),
           x = "Hour of the Day",
           y = "Date",
           fill = "Number of Events") +
      theme_minimal()
  }
  
  print(p)
  print(typeof(p))

  plot_list <- list()
  plot_list[["Merged"]] <- p
  
  return(plot_list)
}

generate_heatmaps_for_top_weeks <- function(data, n_weeks, day_names) {
  # Specify the directory to save the plots
  plot_directory <- "Data/Output/R Graphs/"
  if (!dir.exists(plot_directory)) {
    dir.create(plot_directory, recursive = TRUE)
  }
  
  # Aggregate data to count events per week
  events_per_week <- data %>%
    group_by(WEEK) %>%
    summarise(Events = n(), .groups = 'drop') %>%
    arrange(desc(Events)) %>%
    slice(1:n_weeks)  # Select the top N weeks with the most events
  
  # Extract the WEEK values for these top weeks
  interesting_weeks <- events_per_week$WEEK
  
  # Generate and save heatmaps for the top N interesting weeks
  for (week in interesting_weeks) {
    week_data <- data[data$WEEK == week,]
    p <- create_heatmap_by_hour_day(week_data, sprintf("Week %d", week), day_names)
    
    # Construct a filename for each plot
    plot_filename <- sprintf("Week_%d_Heatmap.png", week)
    full_plot_path <- file.path(plot_directory, plot_filename)
    
    # Save the plot using ggsave
    ggsave(full_plot_path, plot = p, width = 10, height = 8, dpi = 300)
  }
}

create_agent_alarm_bar_plot <- function(data, start_datetime, end_datetime, top_n_agents = NULL) {
  start_datetime <- as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end_datetime <- as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  filtered_data <- data %>%
    filter(RAISETIME >= start_datetime & RAISETIME <= end_datetime)
  
  agent_alarms_severity <- filtered_data %>%
    group_by(AGENT, ORIGINALSEVERITY) %>%
    summarise(Alarms = n(), .groups = 'drop') %>%
    ungroup() %>%
    arrange(desc(Alarms))
  
  # Calculating TotalAlarms for each AGENT for ordering purposes
  total_alarms_per_agent <- agent_alarms_severity %>%
    group_by(AGENT) %>%
    summarise(TotalAlarms = sum(Alarms)) %>%
    arrange(desc(TotalAlarms))
  
  # Joining total alarms back to the main dataset for plotting
  agent_alarms_severity <- agent_alarms_severity %>%
    inner_join(total_alarms_per_agent, by = "AGENT")
  
  # Filtering to top_n_agents if specified
  if (!is.null(top_n_agents)) {
    top_n_agents <- as.integer(top_n_agents)
    top_agents <- head(total_alarms_per_agent$AGENT, top_n_agents)
    agent_alarms_severity <- agent_alarms_severity %>%
      filter(AGENT %in% top_agents)
  }
  
  title_text <- if (is.null(top_n_agents)) "Number of Alarms by Agent" else paste("Top", top_n_agents, "Agents by Number of Alarms")
  p <- ggplot(agent_alarms_severity, aes(x = reorder(AGENT, TotalAlarms), y = Alarms, fill = ORIGINALSEVERITY, text = paste("Agent:", AGENT, "\nAlarms:", Alarms, "\nSeverity:", ORIGINALSEVERITY))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c("Critical" = "red", "Major" = "orange", "Minor" = "yellow", "Warning" = "lightblue", "Indeterminate" = "grey")) +
    theme_minimal() +
    labs(title = paste(title_text, "from", format(start_datetime, "%Y-%m-%d %H:%M:%S"), "to", format(end_datetime, "%Y-%m-%d %H:%M:%S")), x = "Agent", y = "Number of Alarms")
  
  
  print(p)
}

calculate_surge_periods <- function(data, start_datetime, end_datetime, customThreshold, ...) {
  start_time <- as.POSIXct(start_datetime)
  end_time <- as.POSIXct(end_datetime)
  
  filter_vals <- c(...)
  
  # Filter data once based on time
  timeframe_data <- data %>%
    filter(RAISETIME >= start_time & RAISETIME <= end_time) %>%
    mutate(Hour = floor_date(RAISETIME, "hour"))
  
  # Function to process each agent's data
  process_agent_data <- function(val) {
    filtered_data <- timeframe_data %>%
      filter(AGENT == val) %>%
      count(Hour) %>%
      rename(Count = n) %>%
      arrange(Hour) %>%
      mutate(Avg = RcppRoll::roll_mean(Count, 3, fill = NA, align = "right"),
             StdDev = RcppRoll::roll_sd(Count, 3, fill = NA, align = "right"))
    
    surge_detection <- which(diff(filtered_data$Avg) > customThreshold)
    surge_hours <- filtered_data$Hour[surge_detection + 1]
    
    if (length(surge_hours) > 0) {
      tibble(
        Start = surge_hours - hours(1),
        End = surge_hours + hours(1),
        Filter = val
      )
    } else {
      tibble(Start = integer(), End = integer(), Filter = character())
    }
  }
  
  # Apply the function to each agent and combine results
  surge_periods <- map_df(filter_vals, process_agent_data)
  
  # Process merging of consecutive surge periods
  surge_periods %>%
    arrange(Filter, Start) %>%
    group_by(Filter) %>%
    mutate(EndGroup = lag(End, default = first(End)) >= Start - minutes(1)) %>%
    group_by(Filter, cumsum(!EndGroup)) %>%
    summarise(Start = first(Start), End = last(End), .groups = 'drop')
}

create_line_plot_alarm <- function(data, start_datetime, end_datetime, customThreshold, drawAlarms, ...) {
  # Convert start_time and end_time to POSIXct if they are not already
  start_time <- as.POSIXct(start_datetime)
  end_time <- as.POSIXct(end_datetime)
  
  # Extract the ... arguments into a vector
  filter_vals <- c(...)
  
  # Use the new function to calculate surge periods
  surge_periods <- calculate_surge_periods(data, start_datetime, end_datetime, customThreshold, ...)
  
  # Initialize an empty data frame for the hourly counts
  all_hourly_counts <- data.frame(Hour = character(), Count = numeric(), Filter = character(), Avg = numeric(), StdDev = numeric())
  
  # Continue with the rest of the function, minus the surge period detection code
  for (val in filter_vals) {
    # Filter and summarize data
    filtered_data <- data[data$RAISETIME >= start_time & data$RAISETIME <= end_time & data$AGENT == val, ] %>%
      mutate(Hour = floor_date(RAISETIME, "hour")) %>%
      group_by(Hour) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Filter = val, 
             Avg = rollapply(Count, width = 3, FUN = mean, fill = NA, align = "right"),
             StdDev = rollapply(Count, width = 3, FUN = sd, fill = NA, align = "right"))
    
    # Combine with previous counts
    all_hourly_counts <- rbind(all_hourly_counts, filtered_data)
  }
  
  # Determine ymin and ymax based on data range
  ymin_val <- min(all_hourly_counts$Avg - all_hourly_counts$StdDev, na.rm = TRUE)
  ymax_val <- max(all_hourly_counts$Avg + all_hourly_counts$StdDev, na.rm = TRUE)
  padding <- (ymax_val - ymin_val) * 0.1 # 10% padding
  ymin_val <- ymin_val - padding
  ymax_val <- ymax_val + padding
  
  # Base plot with rolling averages and deviation ribbons
  plot <- ggplot(all_hourly_counts, aes(x = Hour, y = Avg, group = Filter, color = Filter)) +
    geom_line() +
    geom_ribbon(aes(ymin = Avg - StdDev, ymax = Avg + StdDev, fill = Filter), alpha = 0.2) +
    geom_point(aes(y = Count), alpha = 0.5)
  
  # Retrieve overlapping surges data
  overlapping_surges <- find_overlapping_alarms(surge_periods = surge_periods)
  print(str(overlapping_surges))
  print(str(surge_periods))
  
  # Decide how to draw alarms based on the drawAlarms setting
  if (drawAlarms == 3) {
    for(surge in unique(surge_periods$Filter)) {
      surge_data <- surge_periods[surge_periods$Filter == surge,]
      for(i in 1:nrow(surge_data)) {
        plot <- plot + geom_rect(data = surge_data, 
                                 xmin = as.numeric(surge_data[i,]$Start), 
                                 xmax = as.numeric(surge_data[i,]$End), 
                                 ymin = ymin_val, ymax = ymax_val, 
                                 fill = "red", alpha = 0.2, inherit.aes = FALSE)
      }
    }
  } else if (drawAlarms == 2) {
    # Draw rectangles only on overlapping alarms
    plot <- plot + geom_rect(data = overlapping_surges,
                             aes(xmin = OverlapStart, xmax = OverlapEnd,
                                 ymin = ymin_val, ymax = ymax_val, fill = Agent1),
                             alpha = 0.3, inherit.aes = FALSE)
  }
  
  return(list(plot = plot, surges = surge_periods))
}

find_overlapping_alarms <- function(surge_periods) {
  surge_periods <- surge_periods[order(surge_periods$Start),]
  
  overlap_info <- data.frame(
    Agent1 = character(),
    Agent2 = character(),
    OverlapStart = as.POSIXct(character()),
    OverlapEnd = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(surge_periods)) {
    current_period <- surge_periods[i,]
    
    # Find potential overlaps
    potential_overlaps <- surge_periods[surge_periods$Start <= current_period$End,]
    
    for (j in 1:nrow(potential_overlaps)) {
      other_period <- potential_overlaps[j,]
      
      # Check for actual overlap
      if (other_period$End >= current_period$Start && other_period$Filter != current_period$Filter) {
        
        # Sort agents to ensure consistency in how overlaps are recorded
        agents <- sort(c(current_period$Filter, other_period$Filter))
        
        # Create a potential new overlap entry
        new_overlap <- data.frame(
          Agent1 = agents[1],
          Agent2 = agents[2],
          OverlapStart = max(current_period$Start, other_period$Start),
          OverlapEnd = min(current_period$End, other_period$End),
          stringsAsFactors = FALSE
        )
        
        # Check if this overlap is already recorded
        if (!any(apply(overlap_info, 1, function(x) all(x == new_overlap)))) {
          overlap_info <- rbind(overlap_info, new_overlap)
        }
      }
    }
  }
  
  return(overlap_info)
}

fetch_alarm_table_data <- function(data, start_datetime, end_datetime, ...) {
  agents <- unlist(list(...))
  
  # Filter data based on RAISETIME and AGENT
  filtered_data <- data %>%
    filter(RAISETIME >= start_datetime, RAISETIME <= end_datetime, AGENT %in% agents)
  
  return(filtered_data)
}

concurrent_surge_agents <- function(data, start_datetime, end_datetime, customThreshold) {
  agents <- unique(data$AGENT)
  
  # Pre-allocate a list to collect results
  surges_summary_list <- vector("list", length(agents))
  
  # Iterate over agents using lapply or a loop, storing results in the list
  for (i in seq_along(agents)) {
    agent <- agents[i]
    surge_periods <- calculate_surge_periods(data, start_datetime, end_datetime, customThreshold, agent)
    
    num_surge_periods <- nrow(surge_periods)
    
    # Directly construct each part of the list
    surges_summary_list[[i]] <- data.frame(Agent = agent, NumSurges = num_surge_periods, stringsAsFactors = FALSE)
  }
  
  # Combine all results at once
  surges_summary <- do.call(rbind, surges_summary_list)
  
  # Order results
  surges_summary <- surges_summary %>% 
    arrange(desc(NumSurges))
  
  return(surges_summary)
}



###### UNUSED   ###### 


plot_missing_data <- function(data) {
  # Identify columns that are not date or time to avoid conversion issues
  non_datetime_cols <- sapply(data, function(x) !inherits(x, "Date") && !inherits(x, "POSIXt"))
  data_non_datetime <- data[, non_datetime_cols]

  # Calculate the number of missing values per column, including NA and empty strings
  missing_data <- sapply(data_non_datetime, function(x) sum(is.na(x) | x == ""))

  # Create a data frame for plotting
  missing_data_df <- data.frame(
    Column = names(missing_data),
    MissingValues = missing_data
  ) %>%
    arrange(desc(MissingValues)) %>%
    filter(MissingValues > 0)  # Optional: Filter out columns with no missing data
  
  # Generate the plot
  ggplot(missing_data_df, aes(x = reorder(Column, MissingValues), y = MissingValues)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Missing and Empty Values per Column (Excluding Date/Time)", x = "Column", y = "Number of Missing/Empty Values") +
    coord_flip()  # Flip coordinates to make it easier to read column names
}

extract_and_write_to_csv <- function(data, start_datetime, end_datetime, filename) {
  # Convert start_datetime and end_datetime to POSIXct in UTC timezone
  start_datetime <- as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end_datetime <- as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Filter data based on the provided datetime range
  extracted_data <- data %>%
    filter(RAISETIME >= start_datetime & RAISETIME <= end_datetime)
  
  # Write the extracted data to a CSV file
  # write.csv(extracted_data, filename, row.names = FALSE, sep = ';')
  
  cat("Data extracted and written to", filename, "\n")
  
  return(extracted_data)
}

plot_timeline_for_agent <- function(data, start_time, end_time) {
  # Convert start_time and end_time to POSIXct if they are not already
  start_time <- as.POSIXct(start_time)
  end_time <- as.POSIXct(end_time)
  
  # Filter data for the given time frame
  filtered_data <- data[data$RAISETIME >= start_time & data$RAISETIME <= end_time, ]
  
  # Plot timeline
  p <- ggplot(filtered_data, aes(x = RAISETIME, y = AGENT)) +
    geom_point(color = "blue", size = 3) +
    labs(x = "Time", y = "Agent", title = "Timeline of Triggers for Agent EMMA") +
    theme_minimal() +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

