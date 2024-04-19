library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)
library(DT)

source("global.R")
source("functions.R")
source("data_preparation.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Optic Alarms Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "menu1"),
      menuItem("Heatmap", tabName = "menu2"),
      menuItem("Alarm density", tabName = "menu3"),
      menuItem("Timeline", tabName = "menu4"),
      menuItem("Correlation map", tabName = "menu5"),
      menuItem("RC Tracker", tabName = "menu6")
    )
  ),
  dashboardBody(
    tabItems(
      
      ##### MENU 1 ######
      
      tabItem(tabName = "menu1",
              fluidRow(
                
                column(4,
                       shinydashboard::box(
                         title = "Filters",
                         dateInput("start_date_menu1", label = "Select Start Date", value = Sys.Date()),
                         dateInput("end_date_menu1", label = "Select End Date", value = Sys.Date()),
                         textInput("start_time_menu1", label = "Enter Start Time (HH:MM)", value = "12:00"),
                         textInput("end_time_menu1", label = "Enter End Time (HH:MM)", value = "12:00"),
                         selectInput("select_menu1", label = "Select the number of items shown", choices = c(5, 10, 15))
                       )     
                ),
                column(8,
                       plotlyOutput("menu1_output")
                )
              )
      ),
      
      ##### MENU 2 ######
      
      tabItem(tabName = "menu2",
              fluidRow(
                column(
                  width = 2,
                  shinydashboard::box(
                    title = "Filters",
                    width = NULL,
                    selectizeInput("select_menu2", label = "Select an Agent (or multiple)", choices = unique(data$AGENT), multiple = TRUE),
                    radioButtons("radio_btn", label = "Select Type", choices = c("Weekly" = "Weekly", "Cumulative" = "Cumulative")),

                    # Only show when Weekly selected
                    conditionalPanel(
                      condition = "input.radio_btn === 'Weekly'",
                      dateInput("start_date_weekly", label = "Select Start Date", value = Sys.Date(), weekstart = 1),
                      actionButton("prev_week", "Previous week"),
                      actionButton("next_week", "Next week"),
                      checkboxInput("merge_toggle_weekly", label = "Merge results", value = FALSE)
                    ),
                    
                    # Only show when Cumulative selected
                    conditionalPanel(
                      condition = "input.radio_btn === 'Cumulative'",
                      dateInput("start_date_cumulative", label = "Select Start Date", value = Sys.Date() - 30, weekstart = 1),
                      dateInput("end_date_cumulative", label = "Select End Date", value = Sys.Date(), weekstart = 1),
                      checkboxInput("merge_toggle_cumulative", label = "Merge results", value = FALSE)
                    )
                  )
                ),
                column(
                  width = 10,
                  # Only show when Weekly selected
                  conditionalPanel(
                    condition = "input.radio_btn === 'Weekly'",
                    box(
                      title = "7 Days heatmap",
                      width = NULL,
                      uiOutput("output_week")
                    )
                  ),
                  
                  # Only show when Cumulative selected
                  conditionalPanel(
                    condition = "input.radio_btn === 'Cumulative'",
                    box(
                      title = "Cumulative heatmap",
                      width = NULL,
                      uiOutput("output_cumulative")
                    )
                  )
                )
              )
      ),
      
      ##### MENU 3 ######
      
      tabItem(tabName = "menu3",
              fluidRow(
                column(2,
                       dateInput("start_date_menu3", label = "Select Start Date", value = Sys.Date()),
                       dateInput("end_date_menu3", label = "Select End Date", value = Sys.Date()),
                       textInput("start_time_menu3", label = "Enter Start Time (HH:MM)", value = "12:00"),
                       textInput("end_time_menu3", label = "Enter End Time (HH:MM)", value = "12:00"),
                       selectInput("select_menu3", label = "Select the number of items shown", choices = c(5, 10, 15))
                ),
                column(8,
                       plotlyOutput("menu3_output")
                )
              )
      ),
      
      ##### MENU 4 ######
      
      tabItem(tabName = "menu4",
              fluidRow(
                column(4,
                       dateInput("start_date_menu4", label = "Select Start Date", value = Sys.Date()),
                       dateInput("end_date_menu4", label = "Select End Date", value = Sys.Date()),
                       textInput("start_time_menu4", label = "Enter Start Time (HH:MM)", value = "12:00"),
                       textInput("end_time_menu4", label = "Enter End Time (HH:MM)", value = "12:00")
                ),
                column(8,
                       plotlyOutput("menu4_output")
                )
              )
      ),
      
      ##### MENU 5 ######
      
      tabItem(tabName = "menu5",
              fluidRow(
                column(4,
                       dateInput("start_date_menu5", label = "Select Start Date", value = Sys.Date()),
                       dateInput("end_date_menu5", label = "Select End Date", value = Sys.Date()),
                       textInput("start_time_menu5", label = "Enter Start Time (HH:MM)", value = "12:00"),
                       textInput("end_time_menu5", label = "Enter End Time (HH:MM)", value = "12:00")
                ),
                column(8,
                       plotlyOutput("menu5_output")
                )
              )
      ),
      
      ##### MENU 6 ######

      tabItem(tabName = "menu6",
              fluidRow(
                column(2,
                   shinydashboard::box(
                     title = "Filters",
                     width = NULL,
                     selectizeInput("select_menu6", label = "Agent(s)", choices = unique(data$AGENT), multiple = TRUE),
                     hr(),
                     h4("Timeframe"),
                     dateInput("start_date_menu6", label = "Start Date", value = "2024-02-01"),
                     textInput("start_time_menu6", label = "Start Time (HH:MM)", value = "00:00"),
                     dateInput("end_date_menu6", label = "End Date", value = "2024-02-05"),
                     textInput("end_time_menu6", label = "End Time (HH:MM)", value = "00:00"),
                     hr(),
                     radioButtons("alarm_toggle_menu6", label = h4("Surges"),
                                  choices = list("No surges" = 1, "Common surges" = 2, "All surges" = 3), 
                                  selected = 1),
                     sliderInput("slider_menu6", label = "Surge sensitivity", min = 0.1, max = 25, value = 1.5),
                     textOutput("overlapping_nb")
                   )
                ),
                column(10,
                   shinydashboard::box(
                     title = "Alarm timeline",
                     width = NULL,
                     plotlyOutput("menu6_output"),
                   ),
                   fluidRow(
                     shinydashboard::box(
                       title = "Selected data",
                       DTOutput("table_menu6")
                     ),
                     shinydashboard::box(
                       title = "RC-GPT" # Root Cause - Global Problem Tracker ;)
                       
                     )
                     
                   )
                )
              )
      )
      
      #####
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  ###### MENU 1 ######

  output$menu1_output <- renderPlotly({
    plot <- ggplotly(create_histogram_by_time(data, "Test Histogram"), source= "testHistogram")
    plot
  })
  
  observeEvent(event_data("plotly_click", source = "testHistogram"), {
    click_data <- event_data("plotly_click", source = "testHistogram")
    if (!is.null(click_data)) {
      showModal(modalDialog(
        title = "Details",
        p(paste("You clicked on hour:", click_data$x)),
        p(paste("Count:", click_data$y))
      ))
    }
  })
  
  observeEvent(event_data("plotly_click", source = "alarm_density"), {
    click_data <- event_data("plotly_click", source = "alarm_density")
    if (!is.null(click_data)) {
      # Print the clicked bar's custom information
      print(event_data(source = "alarm_density"))
    }
  })
  
  ###### MENU 2 ######
  
  heatmap_plots <- reactiveValues(weeklyPlots = NULL, cumulativePlots = NULL)

  # Function to update the start week and refresh the plot for "Per week"
  observeEvent(input$prev_week, {
    new_date <- as.Date(input$start_date_weekly) - 7
    updateDateInput(session, "start_date_weekly", value = new_date)
  })
  
  observeEvent(input$next_week, {
    new_date <- as.Date(input$start_date_weekly) + 7
    updateDateInput(session, "start_date_weekly", value = new_date)
  })
  
  observe({
    req(input$start_date_weekly, input$select_menu2)  # Ensure necessary inputs are available
    heatmap_data <- create_heatmap_for_week(data, input$start_date_weekly, input$select_menu2)
    heatmap_plots$weeklyPlots <- heatmap_data$plot_list
    
    if (input$merge_toggle_weekly) {
      heatmap_plots$weeklyPlots <- merge_heatmaps(heatmap_data$data_list, FALSE)
    } else {
      heatmap_plots$weeklyPlots <- heatmap_data$plot_list
    }
    
    lapply(seq_along(heatmap_plots$weeklyPlots), function(i) {
      output[[paste("plot", i, sep="_")]] <- renderPlotly({
        return(heatmap_plots$weeklyPlots[[i]])
      })
    })
  })
  
  observe({
    req(input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)  # Ensure necessary inputs are available
    if (as.Date(input$start_date_cumulative) <= as.Date(input$end_date_cumulative)) {
      heatmap_data <- create_heatmap_by_hour_day(data, input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)
      heatmap_plots$cumulativePlots <- heatmap_data$plot_list
      
      if (input$merge_toggle_cumulative) {
        heatmap_plots$cumulativePlots <- merge_heatmaps(heatmap_data$data_list, TRUE)
      } else {
        heatmap_plots$cumulativePlots <- heatmap_data$plot_list
      }
      
      lapply(seq_along(heatmap_plots$cumulativePlots), function(i) {
        output[[paste("cumulative_plot", i, sep="_")]] <- renderPlotly({
          return(heatmap_plots$cumulativePlots[[i]])
        })
      })
    }
  })

  # Render the heatmap for the "Per week" tab
  output$output_week <- renderUI({
    req(input$start_date_weekly, input$select_menu2)  # Ensure necessary inputs are available
    
    plot_output_list <- lapply(seq_along(heatmap_plots$weeklyPlots), function(i) {
      plotName <- paste("plot", i, sep="_")
      plotlyOutput(plotName, height = "300px")
    })
    
    # Return a list of plot outputs
    do.call(tagList, plot_output_list)
  })

  # Render the heatmap for the "Cumulative" tab
  output$output_cumulative <- renderUI({
    req(input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)  # Ensure necessary inputs are available
    if (as.Date(input$start_date_cumulative) <= as.Date(input$end_date_cumulative)) {
      
      plot_output_list <- lapply(seq_along(heatmap_plots$cumulativePlots), function(i) {
        plotName <- paste("cumulative_plot", i, sep="_")
        plotlyOutput(plotName, height = "300px")
      })
      
      # Return a list of plot outputs
      do.call(tagList, plot_output_list)
    } else {
      showNotification("Start date must be before end date!", type = "error")
    }
  })

  
  ###### MENU 3 ######
  
  output$menu3_output <- renderPlotly({
    
    # Check if any input is NULL or empty
    if(is.null(input$start_date_menu3) || is.null(input$end_date_menu3) || is.null(input$start_time_menu3) || 
       input$end_time_menu3 == "") {
      shiny::showNotification("Please enter both date and time information.", type = "error")
      return(NULL)
    }
    
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$start_time_menu3)) {
      shiny::showNotification("Invalid start time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    
    # Validate the end time format
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$end_time_menu3)) {
      shiny::showNotification("Invalid end time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    
    # Combine the date and start time into a single datetime string
    start_datetime <- paste(input$start_date_menu3, input$start_time_menu3)
    # Combine the date and end time into a single datetime string
    end_datetime <- paste(input$end_date_menu3, input$end_time_menu3)
    
    # Format the datetime strings
    start_datetime_formatted <- format(as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime_formatted <- format(as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")

    tryCatch({
      plot <- create_agent_alarm_bar_plot(data, start_datetime_formatted, end_datetime_formatted, input$select_menu3)
      if(is.null(plot)) {
        shiny::showNotification("Plot could not be generated. Please check the input data.", type = "error")
      } else {
        ggplotly(plot, source = 'alarm_density', tooltip = "text")
      }
    }, error = function(e) {
      # Log the error message and notify the user
      message <- paste("Error in generating plot:", e$message)
      cat(message, "\n") # This will print the error message in the R console
      shiny::showNotification("An error occurred while generating the plot. Please check the console for more information.", type = "error")
      NULL
    })
  })
  
  ###### MENU 4 ######
  
  output$menu4_output <- renderPlotly({
    
    # Check if any input is NULL or empty
    if(is.null(input$start_date_menu4) || is.null(input$end_date_menu4) || is.null(input$start_time_menu4) || 
       input$end_time_menu4 == "") {
      shiny::showNotification("Please enter both date and time information.", type = "error")
      return(NULL)
    }
    
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$start_time_menu4)) {
      shiny::showNotification("Invalid start time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    
    # Validate the end time format
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$end_time_menu4)) {
      shiny::showNotification("Invalid end time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    
    # Combine the date and start time into a single datetime string
    start_datetime <- paste(input$start_date_menu4, input$start_time_menu4)
    # Combine the date and end time into a single datetime string
    end_datetime <- paste(input$end_date_menu4, input$end_time_menu4)
    
    # Format the datetime strings to match your function's expected format
    start_datetime_formatted <- format(as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime_formatted <- format(as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    
    # Use the formatted datetime strings in your function call
    plot <- plot_timeline_for_agent(data, start_datetime_formatted, end_datetime_formatted)
    
    tryCatch({
      plot <- plot_timeline_for_agent(data, start_datetime_formatted, end_datetime_formatted)
      if(is.null(plot)) {
        shiny::showNotification("Plot could not be generated. Please check the input data.", type = "error")
      } else {
        plot
      }
    }, error = function(e) {
      # Log the error message and notify the user
      message <- paste("Error in generating plot:", e$message)
      cat(message, "\n") # This will print the error message in the R console
      shiny::showNotification("An error occurred while generating the plot. Please check the console for more information.", type = "error")
      NULL
    })
  })
  
  surges_reactive <- reactiveValues(data = NULL)
  
  ###### MENU 6 ######
  
  output$menu6_output <- renderPlotly({
    
    # Combine the date and start time into a single datetime string
    start_datetime <- paste(input$start_date_menu6, input$start_time_menu6)
    # Combine the date and end time into a single datetime string
    end_datetime <- paste(input$end_date_menu6, input$end_time_menu6)

    # Format the datetime strings
    start_datetime_formatted <- format(as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime_formatted <- format(as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")

    result <- do.call(create_line_plot_alarm, 
                    list(data = data, 
                         start_datetime = start_datetime_formatted, 
                         end_datetime = end_datetime_formatted, 
                         input$slider_menu6,
                         input$alarm_toggle_menu6,
                         input$select_menu6))
    
    # Update the reactive value with the surge data
    surges_reactive$data <- result$surges
    
    result$plot
  })
  
  output$table_menu6 <- renderDT({
    # Assuming surges_reactive$data is your input data for finding overlaps
    overlapping_df <- find_overlapping_alarms(surges_reactive$data)
    
    # Check if the dataframe has any rows
    if (nrow(overlapping_df) > 0) {
      # Optionally, you can format the datetime columns here if needed, 
      # but it seems your function might already handle datetime formatting correctly.
      colnames(overlapping_df) <- c("Agent1", "Agent2", "Overlap Start", "Overlap End")
    } else {
      # Create an empty dataframe with the same columns if no overlaps are found
      overlapping_df <- data.frame(
        Agent1 = character(),
        Agent2 = character(),
        OverlapStart = as.POSIXct(character()),
        OverlapEnd = as.POSIXct(character()),
        stringsAsFactors = FALSE
      )
      colnames(overlapping_df) <- c("Agent1", "Agent2", "Overlap Start", "Overlap End")
    }
    
    # Render the DataTable
    datatable(overlapping_df, options = list(pageLength = 5))
  })
  
  
  output$overlapping_nb <- renderText({
    if (is.null(surges_reactive$data) || nrow(surges_reactive$data) == 0) {
      return("No overlaps found.")
    } else {
      return(paste("Total number of surges found:", nrow(surges_reactive$data)))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
