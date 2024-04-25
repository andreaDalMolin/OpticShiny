library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)
library(DT)
library(shinycssloaders)

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
      menuItem("RC Tracker", tabName = "menu6")
    )
  ),
  dashboardBody(
    tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
    
    tabItems(

      ##### MENU 1 ######
      
      tabItem(tabName = "menu1",
              fluidRow(
                column(2,
                   shinydashboard::box(
                     title = "Filters",
                     width = NULL,
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     h4("Timeframe"),
                     dateInput("start_date_menu1", label = "Start Date", value = Sys.Date()),
                     dateInput("end_date_menu1", label = "End Date", value = Sys.Date()),
                     textInput("start_time_menu1", label = "Start Time (HH:MM)", value = "12:00"),
                     textInput("end_time_menu1", label = "End Time (HH:MM)", value = "12:00"),
                   )     
                ),
                column(10,
                   shinydashboard::box(
                     title = "Histogram",
                     status = "info",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = NULL,
                     plotlyOutput("menu1_output")
                   )
                )
              )
      ),
      
      ##### MENU 2 ######
      
      tabItem(tabName = "menu2",
              fluidRow(
                column(2,
                  shinydashboard::box(
                    title = "Filters",
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    selectizeInput("select_menu2", label = "Agent(s)", choices = unique(data$AGENT), multiple = TRUE),
                    h4("Heatmap settings"),
                    radioButtons("radio_btn", label = "Heatmap Type", choices = c("Weekly" = "Weekly", "Cumulative" = "Cumulative")),
                    checkboxInput("merge_toggle", label = "Merge results", value = FALSE),

                    # Only show when Weekly selected
                    conditionalPanel(
                      condition = "input.radio_btn === 'Weekly'",
                      hr(),
                      h4("Timeframe"),
                      dateInput("start_date_weekly", label = "Start Date", value = Sys.Date(), weekstart = 1),
                      actionButton("prev_week", "Previous week"),
                      actionButton("next_week", "Next week"),
                    ),
                    
                    # Only show when Cumulative selected
                    conditionalPanel(
                      condition = "input.radio_btn === 'Cumulative'",
                      hr(),
                      h4("Timeframe"),
                      dateInput("start_date_cumulative", label = "Start Date", value = Sys.Date() - 30, weekstart = 1),
                      dateInput("end_date_cumulative", label = "End Date", value = Sys.Date(), weekstart = 1),
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
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,                      
                      uiOutput("output_week")
                    )
                  ),
                  
                  # Only show when Cumulative selected
                  conditionalPanel(
                    condition = "input.radio_btn === 'Cumulative'",
                    box(
                      title = "Cumulative heatmap",
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE,
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
                   shinydashboard::box(
                     title = "Filters",
                     width = NULL,
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     selectizeInput("selectize_menu3", label = "Agent(s)", choices = unique(data$AGENT), multiple = TRUE),
                     hr(),
                     h4("Timeframe"),
                     dateInput("start_date_menu3", label = "Start Date", value = Sys.Date()),
                     dateInput("end_date_menu3", label = "End Date", value = Sys.Date()),
                     textInput("start_time_menu3", label = "Start Time (HH:MM)", value = "12:00"),
                     textInput("end_time_menu3", label = "End Time (HH:MM)", value = "12:00"),
                     selectInput("select_menu3", label = "Shown items", choices = c(5, 10, 15))
                   )
                ),
                column(10,
                   shinydashboard::box(
                     title = "Alarms timeline",
                     status = "info",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = NULL,
                     plotlyOutput("menu3_output")
                   )
                )
              )
      ),
      
      ##### MENU 4 ######
      
      tabItem(tabName = "menu4",
              fluidRow(
                column(2,
                   shinydashboard::box(
                     title = "Filters",
                     width = NULL,
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     selectizeInput("select_menu4", label = "Agent(s)", choices = unique(data$AGENT), multiple = TRUE),
                     hr(),
                     h4("Timeframe"),
                     dateInput("start_date_menu4", label = "Start Date", value = Sys.Date()),
                     dateInput("end_date_menu4", label = "End Date", value = Sys.Date()),
                     textInput("start_time_menu4", label = "Start Time (HH:MM)", value = "12:00"),
                     textInput("end_time_menu4", label = "End Time (HH:MM)", value = "12:00")
                   )
                ),
                column(10,
                    shinydashboard::box(
                      title = "Alarm timeline",
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      plotlyOutput("menu4_output")
                    )
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
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
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
                     status = "info",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = NULL,
                     plotlyOutput("menu6_output"),
                   ),
                   fluidRow(
                     tabBox(
                       title = "Data insight",
                       tabPanel("Surges overlap", DTOutput("table_menu6")),
                       tabPanel("Events"),
                     ),
                     shinydashboard::box(
                       title = "RC-GPT", # Root Cause - Global Problem Tracker ;)
                       status = "success",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       div(
                         actionButton("loadData", "Load Data", icon = icon("refresh")),
                         style = "text-align: right;"  # Aligns the button to the right
                       ),
                       br(),
                       DTOutput("surgeDataTable")
                     ),
                     shinydashboard::box(
                       title = "Historical data", # Root Cause - Global Problem Tracker ;)
                       status = "warning",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       
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
    
    if (input$merge_toggle) {
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
      
      if (input$merge_toggle) {
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
  
  output$overlapping_nb <- renderText({
    if (is.null(surges_reactive$data) || nrow(surges_reactive$data) == 0) {
      return("No overlaps found.")
    } else {
      return(paste("Total number of surges found:", nrow(surges_reactive$data)))
    }
  })
  
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

  # Initialize the data table with a message or empty structure
  output$surgeDataTable <- renderDT({
    datatable(data.frame(
      "Agent" = character(),
      "Number of surges" = character()
    ), options = list(pageLength = 5, 
                      language = list(emptyTable = "No data loaded. Click 'Load Data' to display results.")))
  })
  
  observeEvent(input$loadData, {
    # Validate necessary inputs
    req(input$start_date_menu6, input$start_time_menu6, input$end_date_menu6, input$end_time_menu6, input$slider_menu6)
    
    # Prepare data based on input
    start_datetime <- format(as.POSIXct(paste(input$start_date_menu6, input$start_time_menu6), format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime <- format(as.POSIXct(paste(input$end_date_menu6, input$end_time_menu6), format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    
    # Assuming concurrent_surge_agents returns a dataframe
    surgesPerAgent <- concurrent_surge_agents(data, start_datetime, end_datetime, input$slider_menu6)
    output$surgeDataTable <- DT::renderDT({
      datatable(surgesPerAgent, options = list(pageLength = 5))
    })
  })
  
  # output$spinner <- renderUI({
  #   withSpinner(dataTableOutput("surgeDataTable"), color="black")
  # })
  # 
  # output$surgeDataTable <- renderDT({
  #   # Validate necessary inputs
  #   req(input$start_date_menu6, input$start_time_menu6, input$end_date_menu6, input$end_time_menu6, input$slider_menu6)
  #   
  #   start_datetime <- format(as.POSIXct(paste(input$start_date_menu6, input$start_time_menu6), format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
  #   end_datetime <- format(as.POSIXct(paste(input$end_date_menu6, input$end_time_menu6), format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
  #   
  #   surgesPerAgent <- concurrent_surge_agents(data, start_datetime, end_datetime, input$slider_menu6)
  #   
  #   DT::datatable(surgesPerAgent, options = list(pageLength = 5))
  # })


}

shinyApp(ui = ui, server = server)
