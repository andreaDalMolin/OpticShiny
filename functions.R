##### MENU 1 #####

# Function to create histogram by day of week
create_histogram_by_day_of_week <- function(data, start_date = "2024-01-01", end_date = "2024-01-31", agent) {
  
  # Ensure RAISETIME is a datetime object
  data$RAISETIME <- as.POSIXct(data$RAISETIME, format = "%Y-%m-%d %H:%M:%S")
  
  # Validate and convert start_date and end_date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Filter data to include only the specified date range
  filtered_data <- data %>%
    filter(RAISETIME >= start_date & RAISETIME <= end_date)
  
  # Further filter by agent if the agent is not null or empty
  if (!is.null(agent) && agent != "") {
    filtered_data <- filtered_data %>%
      filter(AGENT == agent)
  }
  
  # Create a new column to represent the day of the week from RAISETIME
  filtered_data <- filtered_data %>%
    mutate(DAY_OF_WEEK = as.integer(format(RAISETIME, "%u")))  # %u is ISO-8601 day of the week (1=Monday, 7=Sunday)
  
  # Generate a bar plot using ggplot
  ggplot(filtered_data, aes(x = as.factor(DAY_OF_WEEK))) +
    geom_bar(fill = "skyblue", color = "black") +
    scale_x_discrete(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    labs(title = "Number of Alarms per Day of Week",
         x = "Day of Week",
         y = "Count") +
    theme_minimal()
}

# Function to create histogram by time
create_histogram_by_time <- function(data, start_date = Sys.Date() %m-% months(6), end_date = Sys.Date(), agent) {
  
  # Ensure RAISETIME is a datetime object
  data$RAISETIME <- as.POSIXct(data$RAISETIME, format = "%Y-%m-%d %H:%M:%S")
  
  # Validate and convert start_date and end_date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Filter data to include only the specified date range
  filtered_data <- data %>%
    filter(RAISETIME >= start_date & RAISETIME <= end_date)
  
  # Further filter by agent if the agent is not null or empty
  if (!is.null(agent) && agent != "") {
    filtered_data <- filtered_data %>%
      filter(AGENT == agent)
  }
  
  ggplot(filtered_data, aes(x = hour(TIME))) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = paste("Number of Alarms per Time of Day"),
         x = "Hour of Day",
         y = "Frequency") +
    theme_minimal()
}

# Function to create a bar plot of alarms by month or week with customizable date range
plot_alarms_by_time_unit <- function(data, by_month = TRUE, start_date = Sys.Date() %m-% months(6), end_date = Sys.Date(), agent) {
  # Ensure RAISETIME is a datetime object
  data$RAISETIME <- as.POSIXct(data$RAISETIME, format = "%Y-%m-%d %H:%M:%S")
  
  # Validate and convert start_date and end_date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Filter data to include only the specified date range
  filtered_data <- data %>%
    filter(RAISETIME >= start_date & RAISETIME <= end_date)
  
  # Further filter by agent if the agent is not null or empty
  if (!is.null(agent) && agent != "") {
    filtered_data <- filtered_data %>%
      filter(AGENT == agent)
  }
  
  # Determine the grouping by month or week
  if (by_month) {
    # Format as "Month Year" and use it for grouping
    filtered_data$TimeUnit <- format(filtered_data$RAISETIME, "%B %Y")
    filtered_data$TimeUnit <- factor(filtered_data$TimeUnit, levels = unique(filtered_data$TimeUnit[order(year(filtered_data$RAISETIME), month(filtered_data$RAISETIME))]))
    
    # Aggregate data by selected time unit
    time_counts <- filtered_data %>%
      group_by(TimeUnit) %>%
      summarise(AlarmCount = n(), .groups = 'drop')
  } else {
    # Calculate the start and end of the week
    filtered_data$WeekStart <- floor_date(filtered_data$RAISETIME, unit="week", week_start = 1)
    filtered_data$WeekEnd <- ceiling_date(filtered_data$RAISETIME, unit="week", week_start = 1) - days(1)
    # Group by WeekStart for aggregation
    time_counts <- filtered_data %>%
      group_by(WeekStart) %>%
      summarise(AlarmCount = n(), WeekEnd = max(WeekEnd), .groups = 'drop')
    # Create the time unit label after aggregation to avoid duplicates
    time_counts$TimeUnit <- paste(format(time_counts$WeekStart, "%d/%m/%Y"), format(time_counts$WeekEnd, "%d/%m/%Y"), sep="-")
    # Sort time_counts by WeekStart to ensure chronological order
    time_counts <- time_counts[order(time_counts$WeekStart),]
    # Set the factor levels to the ordered TimeUnit
    time_counts$TimeUnit <- factor(time_counts$TimeUnit, levels = time_counts$TimeUnit)
  }
  
  ggplot(time_counts, aes(x = TimeUnit, y = AlarmCount)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(x = if (by_month) "Month" else "Week", y = "Number of Alarms",
         title = if (by_month) "Alarms per Month" else "Alarms per Week",
         subtitle = paste("From", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"))) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),  # Rotate x labels for readability
          legend.position = "none")  # Remove legend
}

#####

##### MENU 2 #####

create_heatmap_by_hour_day <- function(data, start_date, end_date, agents) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  plot_list <- list()
  data_list <- list()
  
  # Generate a full grid of all days of the week (1-7) and hours (0-23)
  all_days_of_week <- 1:7
  all_hours <- 0:23
  full_grid <- expand.grid(DAY_OF_WEEK = all_days_of_week, HOUR = all_hours)
  
  for (agent in agents) {
    agent_data <- if (agent != '' && agent != 'All agents') {
      data[data$AGENT == agent,]
    } else {
      data
    }
    
    timeframe_data <- agent_data[agent_data$RAISETIME >= as.POSIXct(start_date) & agent_data$RAISETIME <= as.POSIXct(paste(end_date, "23:59:59")),]
    
    # Extract DAY_OF_WEEK and HOUR
    timeframe_data <- timeframe_data %>%
      dplyr::mutate(
        DAY_OF_WEEK = as.integer(format(RAISETIME, "%u")),  # Monday=1, Sunday=7
        HOUR = as.integer(format(RAISETIME, "%H"))
      )
    
    # Count the number of events for each combination of DAY_OF_WEEK and HOUR
    event_counts <- timeframe_data %>%
      dplyr::group_by(DAY_OF_WEEK, HOUR) %>%
      dplyr::summarise(Count = n(), .groups = 'drop')
    
    # Merge with full grid and fill in zeros where there are no events
    event_counts <- full_grid %>%
      left_join(event_counts, by = c("DAY_OF_WEEK", "HOUR")) %>%
      replace_na(list(Count = 0))
    
    # Adjust DAY_OF_WEEK to factor with specific labels for plotting
    event_counts$DAY_OF_WEEK <- factor(event_counts$DAY_OF_WEEK, levels = 7:1, labels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    
    # Create the heatmap plot
    p <- ggplot(event_counts, aes(x = HOUR, y = DAY_OF_WEEK, fill = Count)) + 
      geom_tile() +
      scale_fill_gradientn(colors = c("#FFFFFF00", "lightblue", "darkblue"),
                           values = scales::rescale(c(0, 1, max(event_counts$Count, na.rm = TRUE))),
                           na.value = "#FFFFFF00") +
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

create_heatmap_for_week <- function(data, start_date, agents, timezone = "UTC") {
  start_date <- as.Date(start_date)
  end_date <- start_date + 6
  
  # Extending the end time to the very start of the next day
  filter_end_date <- as.POSIXct(paste(end_date + 1, "00:00:00"), tz = timezone)
  
  # Calculate a sequence of dates for the week
  week_dates <- seq(start_date, end_date, by = "day")
  
  plot_list <- list()
  data_list <- list()
  
  # Generate a full grid of all days of the week (1-7) and hours (0-23)
  all_days_of_week <- 1:7
  all_hours <- 0:23
  full_grid <- expand.grid(DAY_OF_WEEK = all_days_of_week, HOUR = all_hours)
  
  # Create reversed date labels for correct plotting
  date_labels <- setNames(as.character(week_dates), 1:7)
  reverse_date_labels <- rev(date_labels)
  
  for (agent in agents) {
    agent_data <- if (agent != '' && agent != 'All agents') {
      data[data$AGENT == agent,]
    } else {
      data
    }
    
    # Filter data for the specified date range
    timeframe_data <- agent_data %>%
      filter(RAISETIME >= as.POSIXct(start_date, tz = timezone) & RAISETIME < filter_end_date)
    
    # Extract DAY_OF_WEEK and HOUR
    timeframe_data <- timeframe_data %>%
      mutate(
        DAY_OF_WEEK = as.integer(lubridate::wday(RAISETIME, week_start = 1)),  # Monday=1, Sunday=7
        HOUR = as.integer(format(RAISETIME, "%H"))
      )
    
    # Count the number of events for each combination of DAY_OF_WEEK and HOUR
    event_counts <- timeframe_data %>%
      group_by(DAY_OF_WEEK, HOUR) %>%
      summarise(Count = n(), .groups = 'drop')
    
    # Merge with full grid and fill in zeros where there are no events
    event_counts <- full_grid %>%
      left_join(event_counts, by = c("DAY_OF_WEEK", "HOUR")) %>%
      replace_na(list(Count = 0))
    
    # Replace DAY_OF_WEEK with reversed actual date labels
    event_counts$DAY_OF_WEEK <- factor(event_counts$DAY_OF_WEEK, levels = 7:1, labels = reverse_date_labels)
    
    # Create the heatmap plot
    p <- ggplot(event_counts, aes(x = HOUR, y = DAY_OF_WEEK, fill = Count)) +
      geom_tile() +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
      scale_fill_gradientn(colors = c("#FFFFFF00", "lightblue", "darkblue"),
                           values = scales::rescale(c(0, 1, max(event_counts$Count, na.rm = TRUE))),
                           na.value = "#FFFFFF00") +
      labs(title = sprintf("%s - Alarms from %s to %s - Number of Alarms by Hour and Date",
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
  
  return(list(plot_list = plot_list, data_list = data_list))
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
    p <- ggplot(merged_data, aes(x = HOUR, y = DAY_OF_WEEK, fill = Count)) +
      geom_tile() +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
      scale_fill_gradientn(colors = c("#FFFFFF00", "lightblue", "darkblue"),
                           values = scales::rescale(c(0, 1, max(merged_data$Count, na.rm = TRUE))),
                           na.value = "#FFFFFF00") +
      labs(title = sprintf("Merged - Number of Alarms by Hour and Day"),
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

#####

##### MENU 3 #####

create_agent_alarm_bar_plot <- function(data, start_datetime, end_datetime, top_n_agents = NULL, agents) {
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

# TODO params match sister function but some arent necessary
create_specific_agent_alarm_bar_plot <- function(data, start_datetime, end_datetime, top_n_agents = NULL, agents) {

  start_datetime <- as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end_datetime <- as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Filter data by date range
  filtered_data <- data %>%
    filter(RAISETIME >= start_datetime & RAISETIME <= end_datetime)
  
  # Filter data for specific agents
  if (!is.null(agents) && length(agents) > 0) {
    filtered_data <- filtered_data %>%
      filter(AGENT %in% agents)
  } else {
    stop("No agents specified for filtering.")
  }
  
  # Group by AGENT and ORIGINALSEVERITY and summarize alarms
  agent_alarms_severity <- filtered_data %>%
    group_by(AGENT, ORIGINALSEVERITY) %>%
    summarise(Alarms = n(), .groups = 'drop')
  
  # Calculate TotalAlarms for each AGENT
  total_alarms_per_agent <- agent_alarms_severity %>%
    group_by(AGENT) %>%
    summarise(TotalAlarms = sum(Alarms), .groups = 'drop')
  
  # Join TotalAlarms back to the main dataset
  agent_alarms_severity <- agent_alarms_severity %>%
    inner_join(total_alarms_per_agent, by = "AGENT") %>%
    mutate(AGENT = reorder(AGENT, TotalAlarms))
  
  # Plotting
  p <- ggplot(agent_alarms_severity, aes(x = AGENT, y = Alarms, fill = ORIGINALSEVERITY, text = paste("Agent:", AGENT, "\nAlarms:", Alarms, "\nSeverity:", ORIGINALSEVERITY))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c("Critical" = "red", "Major" = "orange", "Minor" = "yellow", "Warning" = "lightblue", "Indeterminate" = "grey")) +
    theme_minimal() +
    labs(title = paste("Number of Alarms by Specified Agents from", format(start_datetime, "%Y-%m-%d %H:%M:%S"), "to", format(end_datetime, "%Y-%m-%d %H:%M:%S")), x = "Agent", y = "Number of Alarms")
  
  p
}


#####

##### MENU 4 #####

plot_timeline_for_agent <- function(data, start_time, end_time, agents) {
  # Convert start_time and end_time to POSIXct if they are not already
  start_time <- as.POSIXct(start_time)
  end_time <- as.POSIXct(end_time)
  
  # Filter data for the given time frame
  filtered_data <- data[data$RAISETIME >= start_time & data$RAISETIME <= end_time, ]
  
  # Filter data for specific agents
  if (!is.null(agents) && length(agents) > 0) {
    filtered_data <- filtered_data %>%
      filter(AGENT %in% agents)
  } else {
    stop("No agents specified for filtering.")
  }
  
  # Plot timeline
  p <- ggplot(filtered_data, aes(x = RAISETIME, y = AGENT)) +
    geom_point(color = "blue", size = 3) +
    labs(x = "Time", y = "Agent", title = "Timeline of triggers for selected agents") +
    theme_minimal() +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

#####

##### MENU 6 #####

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
      tibble(
        Start = as.POSIXct(character(0)), # Empty but with correct types
        End = as.POSIXct(character(0)),   # Empty but with correct types
        Filter = character(0)
      )
    }
  }

  # Apply the function to each agent and combine results
  surge_periods <- map_df(filter_vals, process_agent_data)
  
  # Ensure there is a fallback value for End if no data is present
  default_end_value <- if (nrow(timeframe_data) == 0 || all(is.na(timeframe_data$End))) {
    as.POSIXct("1970-01-01 00:00:00") # This is the fallback date when no data is present
  } else {
    first(timeframe_data$End) # The first valid End value in the data
  }
  
  # Modify the mutate call to handle potentially empty data
  surge_periods %>%
    arrange(Filter, Start) %>%
    group_by(Filter) %>%
    mutate(EndGroup = lag(End, default = default_end_value) >= Start - minutes(1)) %>%
    group_by(Filter, cumsum(!EndGroup)) %>%
    summarise(Start = first(Start), End = last(End), .groups = 'drop')
}

create_line_plot_alarm <- function(data, start_datetime, end_datetime, customThreshold, drawAlarms, ...) {
  start_time <- as.POSIXct(start_datetime)
  end_time <- as.POSIXct(end_datetime)
  
  filter_vals <- c(...)
  
  surge_periods <- calculate_surge_periods(data, start_datetime, end_datetime, customThreshold, ...)
  
  all_hourly_counts <- data.frame(Hour = character(), Count = numeric(), Filter = character(), Avg = numeric(), StdDev = numeric())
  
  for (val in filter_vals) {
    filtered_data <- data[data$RAISETIME >= start_time & data$RAISETIME <= end_time & data$AGENT == val, ] %>%
      mutate(Hour = floor_date(RAISETIME, "hour")) %>%
      group_by(Hour) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Filter = val, 
             Avg = rollapply(Count, width = 3, FUN = mean, fill = NA, align = "right"),
             StdDev = rollapply(Count, width = 3, FUN = sd, fill = NA, align = "right"))
    
    all_hourly_counts <- rbind(all_hourly_counts, filtered_data)
  }
  
  ymin_val <- min(all_hourly_counts$Avg - all_hourly_counts$StdDev, na.rm = TRUE)
  ymax_val <- max(all_hourly_counts$Avg + all_hourly_counts$StdDev, na.rm = TRUE)
  padding <- (ymax_val - ymin_val) * 0.1
  ymin_val <- ymin_val - padding
  ymax_val <- ymax_val + padding
  
  plot <- ggplot(all_hourly_counts, aes(x = Hour, y = Avg, group = Filter, color = Filter)) +
    geom_line() +
    geom_ribbon(aes(ymin = Avg - StdDev, ymax = Avg + StdDev, fill = Filter), alpha = 0.2) +
    geom_point(aes(y = Count), alpha = 0.5)
  
  if (drawAlarms == 3) { # Only show individual surges
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
  } else if (drawAlarms == 2) { # Show overlapping surges
    overlapping_surges <- find_overlapping_alarms(surge_periods = surge_periods)
    if(nrow(overlapping_surges)) {
      plot <- plot + geom_rect(data = overlapping_surges,
                               aes(xmin = OverlapStart, xmax = OverlapEnd,
                                   ymin = ymin_val, ymax = ymax_val, fill = Agent1),
                               alpha = 0.3, inherit.aes = FALSE)
    }
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
  
  for (i in 1:(nrow(surge_periods)-1)) {
    current_period <- surge_periods[i,]
    
    for (j in (i+1):nrow(surge_periods)) {
      other_period <- surge_periods[j,]
      
      if (other_period$Start <= current_period$End && other_period$End >= current_period$Start && other_period$Filter != current_period$Filter) {
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
        
        # Add new overlap entry without checking for duplicates because we manage overlaps in ordered, non-redundant pairs
        overlap_info <- rbind(overlap_info, new_overlap)
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

analyze_overlaps <- function(agent_name, output_csv = "overlap_details.csv") {
  
  #Import surge periods file
  surge_data <- read.csv("surge_periods.csv")
  
  surge_data$Start <- as.POSIXct(surge_data$Start, format = "%d/%m/%Y %H:%M:%S")
  surge_data$End <- as.POSIXct(surge_data$End, format = "%d/%m/%Y %H:%M:%S")
  
  surge_data <- surge_data %>%
    filter(complete.cases(.))
  
  setDT(surge_data)
  surge_data[, `:=`(Start = as.POSIXct(Start), End = as.POSIXct(End))]
  
  agent_surges <- surge_data[Filter == agent_name]
  other_agents_surges <- surge_data[Filter != agent_name]
  
  # Set keys for joining
  setkey(agent_surges, Start, End)
  setkey(other_agents_surges, Start, End)
  
  # Find overlaps
  overlaps <- foverlaps(agent_surges, other_agents_surges, type = "any", which = TRUE, nomatch = 0L)
  if (nrow(overlaps) > 0) {
    overlaps_result <- agent_surges[overlaps$xid][, .(Filter1 = Filter, Start1 = Start, End1 = End)]
    overlaps_other <- other_agents_surges[overlaps$yid][, .(Filter2 = Filter, Start2 = Start, End2 = End)]
    final_overlaps <- cbind(overlaps_result, overlaps_other)
    setorder(final_overlaps, -Start1)
    # fwrite(final_overlaps, output_csv) # OPTIONAL
    return(final_overlaps)
  } else {
    return(NULL)
  }
}

# TODO : make this auto run every day and incrementally add to existing data
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

#####
