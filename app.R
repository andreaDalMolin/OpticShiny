library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)

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
      tabItem(tabName = "menu1",
              fluidRow(
                column(4,
                       dateInput("start_date_menu1", label = "Select Start Date", value = Sys.Date()),
                       dateInput("end_date_menu1", label = "Select End Date", value = Sys.Date()),
                       textInput("start_time_menu1", label = "Enter Start Time (HH:MM)", value = "12:00"),
                       textInput("end_time_menu1", label = "Enter End Time (HH:MM)", value = "12:00"),
                       selectInput("select_menu1", label = "Select the number of items shown", choices = c(5, 10, 15))
                ),
                column(8,
                       plotlyOutput("menu1_output")
                )
              )
      ),
      tabItem(tabName = "menu2",
              fluidRow(
                column(4,
                       selectInput("select_menu2", label = "Select an Agent", choices = unique(data$AGENT)),
                       sliderInput("slider_menu2", label = "Select Week", min = 1, max = 65, value = 53)
                       #dateInput("start_date_menu2", label = "Select Start Date", value = Sys.Date()),
                       #dateInput("end_date_menu2", label = "Select End Date", value = Sys.Date()),
                ),
                column(8,
                       plotlyOutput("menu2_output")
                )
              )
      ),
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
      tabItem(tabName = "menu6",
              fluidRow(
                column(4,
                       selectizeInput("select_menu6", label = "Select an Agent", choices = unique(data$AGENT), multiple = TRUE),
                       dateInput("start_date_menu6", label = "Select Start Date", value = "2024-01-28"),
                       dateInput("end_date_menu6", label = "Select End Date", value = "2024-01-29"),
                       textInput("start_time_menu6", label = "Enter Start Time (HH:MM)", value = "12:00"),
                       textInput("end_time_menu6", label = "Enter End Time (HH:MM)", value = "13:00"),
                       checkboxInput("alarm_toggle_menu6", label = "Toggle Feature On/Off", value = TRUE),
                       sliderInput("slider_menu6", label = "Alarm Sensitivity", min = 0.1, max = 50, value = 1.5)
                ),
                column(8,
                       plotlyOutput("menu6_output")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

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
  
  output$menu2_output <- renderPlotly({
    create_heatmap_for_week(data, input$slider_menu2, day_names, input$select_menu2)
  })
  
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
    
    print(start_datetime_formatted)
    
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
  
  output$menu6_output <- renderPlotly({
    
    # Combine the date and start time into a single datetime string
    start_datetime <- paste(input$start_date_menu6, input$start_time_menu6)
    # Combine the date and end time into a single datetime string
    end_datetime <- paste(input$end_date_menu6, input$end_time_menu6)
    
    # Format the datetime strings
    start_datetime_formatted <- format(as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime_formatted <- format(as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    
    
    plot <- do.call(create_line_plot_alarm, 
                    list(data = data, 
                         start_datetime = start_datetime_formatted, 
                         end_datetime = end_datetime_formatted, 
                         input$slider_menu6,
                         input$alarm_toggle_menu6,
                         input$select_menu6))
    
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
