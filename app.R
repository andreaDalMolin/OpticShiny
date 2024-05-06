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

agent_choices <- c(sort(unique(data$AGENT)))


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
    tags$div(tags$style(HTML("
                             .dropdown-menu{z-index:10000 !important;}
                             .scrollable-mainTable::-webkit-scrollbar {
                                display: none; /* for Chrome, Safari, and Opera */
                              }
                              .scrollable-mainTable {
                                -ms-overflow-style: none;  /* IE and Edge */
                                scrollbar-width: none;  /* Firefox */
                                overflow-y: auto;
                                height: 400px; /* Fixed height */
                              }
                             ")
                        )
             ),
    useShinyjs(),
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
                     dateInput("start_date_menu1", label = "Start Date", value = Sys.Date(), weekstart = 1),
                     dateInput("end_date_menu1", label = "End Date", value = Sys.Date(), weekstart = 1),
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
                         selectizeInput(
                           "select_menu2", 
                           label = "Agent(s)", 
                           choices = data.frame(All=c("All agents"),Agents=agent_choices),
                           multiple = TRUE
                         ),
                         hr(),
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
                      # Conditional content inside the box
                      conditionalPanel(
                        condition = "input.select_menu2 != ''",
                        uiOutput("output_week")
                      ),
                      conditionalPanel(
                        condition = "input.select_menu2 == ''",
                        div(style = "display: flex; justify-content: center; align-items: center; height: 300px;",
                            h3("Please select an agent first", style = "text-align: center;")
                        )
                      )
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
                      # Conditional content inside the box
                      conditionalPanel(
                        condition = "input.select_menu2 != ''",
                        uiOutput("output_cumulative")
                      ),
                      conditionalPanel(
                        condition = "input.select_menu2 == ''",
                        div(style = "display: flex; justify-content: center; align-items: center; height: 300px;",
                            h3("Please select an agent first", style = "text-align: center;")
                        )
                      )
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
                     dateInput("start_date_menu3", label = "Start Date", value = Sys.Date(), weekstart = 1),
                     dateInput("end_date_menu3", label = "End Date", value = Sys.Date(), weekstart = 1),
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
                     dateInput("start_date_menu4", label = "Start Date", value = Sys.Date(), weekstart = 1),
                     dateInput("end_date_menu4", label = "End Date", value = Sys.Date(), weekstart = 1),
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
                     dateInput("start_date_menu6", label = "Start Date", value = "2024-03-04", weekstart = 1),
                     textInput("start_time_menu6", label = "Start Time (HH:MM)", value = "00:00"),
                     dateInput("end_date_menu6", label = "End Date", value = "2024-03-14", weekstart = 1),
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
                       title = "Data Insight",
                       tabPanel("Common Surges",
                                div(style = "height: 400px; overflow-y: auto;", 
                                    uiOutput("table_menu6_ui"))),
                       tabPanel("Notable Periods",
                                uiOutput("selectedAgentLabel"),
                                uiOutput("contentDisplay")  # This output will handle the conditional content
                       )
                     ),
                     shinydashboard::box(
                       title = "Root Cause Tracker", # Root Cause - Global Problem Tracker ;)
                       status = "success",
                       #height = 400,
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
                       title = "Historical data",
                       status = "warning",
                       #height = 400,
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
    tryCatch({
      heatmap_data <- create_heatmap_for_week(data, input$start_date_weekly, input$select_menu2)
      heatmap_plots$weeklyPlots <- heatmap_data$plot_list
      
      if (input$merge_toggle) {
        heatmap_plots$weeklyPlots <- merge_heatmaps(heatmap_data$data_list, FALSE)
      }
      
      outputOptions(output, "output_week", suspendWhenHidden = FALSE)  # Ensure plots are always updated regardless of tab visibility
      
      # Use seq_along safely to handle empty or NULL lists
      lapply(seq_along(heatmap_plots$weeklyPlots), function(i) {
        output[[paste("plot", i, sep="_")]] <- renderPlotly({
          # Check if the plot exists before trying to render it
          if (i <= length(heatmap_plots$weeklyPlots)) {
            return(heatmap_plots$weeklyPlots[[i]])
          }
        })
      })
    }, error = function(e) {
      showNotification(paste("Error in weekly heatmap generation:", e$message), type = "error")
    })
  })
  
  
  observe({
    req(input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)  # Ensure necessary inputs are available
    #tryCatch({
      if (as.Date(input$end_date_cumulative) > as.Date(input$start_date_cumulative)) {
        if (difftime(as.Date(input$end_date_cumulative), as.Date(input$start_date_cumulative), units = "days") >= 14) {
          heatmap_data <- create_heatmap_by_hour_day(data, input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)
          heatmap_plots$cumulativePlots <- heatmap_data$plot_list
          
          if (input$merge_toggle) {
            heatmap_plots$cumulativePlots <- merge_heatmaps(heatmap_data$data_list, TRUE)
          }
          
          outputOptions(output, "output_cumulative", suspendWhenHidden = FALSE)  # Ensure plots are always updated regardless of tab visibility
          
          lapply(seq_along(heatmap_plots$cumulativePlots), function(i) {
            output[[paste("cumulative_plot", i, sep="_")]] <- renderPlotly({
              # Check if the plot exists before trying to render it
              if (i <= length(heatmap_plots$cumulativePlots)) {
                return(heatmap_plots$cumulativePlots[[i]])
              }
            })
          })
        } else {
          showNotification("Date range must span at least 14 days for cumulative heatmaps.", type = "error")
        }
      } else {
        showNotification("Start date must be before end date!", type = "error")
      }
    #}, error = function(e) {
    #  showNotification(paste("Error in cumulative heatmap generation:", e$message), type = "error")
    #})
  })
  
  
  # Render the heatmap for the "Per week" tab
  output$output_week <- renderUI({
    req(input$start_date_weekly, input$select_menu2)  # Ensure necessary inputs are available
    tryCatch({
      plot_output_list <- lapply(seq_along(heatmap_plots$weeklyPlots), function(i) {
        plotName <- paste("plot", i, sep="_")
        div(
          plotlyOutput(plotName, height = "300px"),
          style = "padding-bottom: 50px;"  # Add padding to the bottom of each plot
        )
      })
      
      # Return a list of plot outputs
      do.call(tagList, plot_output_list)
    }, error = function(e) {
      showNotification(paste("Error displaying weekly plots:", e$message), type = "error")
    })
  })
  
  # Render the heatmap for the "Cumulative" tab
  output$output_cumulative <- renderUI({
    req(input$start_date_cumulative, input$end_date_cumulative, input$select_menu2)  # Ensure necessary inputs are available
    tryCatch({
      if (as.Date(input$end_date_cumulative) > as.Date(input$start_date_cumulative)) {
        plot_output_list <- lapply(seq_along(heatmap_plots$cumulativePlots), function(i) {
          plotName <- paste("cumulative_plot", i, sep="_")
          div(
            plotlyOutput(plotName, height = "300px"),
            style = "padding-bottom: 50px;"  # Add padding to the bottom of each plot
          )
        })
        
        # Return a list of plot outputs
        do.call(tagList, plot_output_list)
      }
    }, error = function(e) {
      showNotification(paste("Error displaying cumulative plots:", e$message), type = "error")
    })
  })
  

  
  ###### MENU 3 ######
  
  output$menu3_output <- renderPlotly({
    
    # Check if any input is NULL or empty
    if(is.null(input$start_date_menu3) || is.null(input$end_date_menu3) || is.null(input$start_time_menu3) || 
       input$end_time_menu3 == "") {
      shiny::showNotification("Please enter both date and time information.", type = "error")
      return(NULL)
    }
    
    # Validate start and end time formats
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$start_time_menu3)) {
      shiny::showNotification("Invalid start time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    if(!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$end_time_menu3)) {
      shiny::showNotification("Invalid end time format. Please enter time as HH:MM (24-hour format).", type = "error")
      return(NULL)
    }
    
    # Check that end date is later than start date
    if(as.Date(input$start_date_menu3) > as.Date(input$end_date_menu3)) {
      shiny::showNotification("End date must be later than the start date.", type = "error")
      return(NULL)
    }
    
    # Combine the date and time into single datetime strings
    start_datetime <- paste(input$start_date_menu3, input$start_time_menu3)
    end_datetime <- paste(input$end_date_menu3, input$end_time_menu3)
    
    # Format the datetime strings
    start_datetime_formatted <- format(as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    end_datetime_formatted <- format(as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d %H:%M:%S")
    
    tryCatch({
      plot <- create_agent_alarm_bar_plot(data, start_datetime_formatted, end_datetime_formatted, input$select_menu3)
      if(is.null(plot)) {
        shiny::showNotification("Plot could not be generated. Please check the input data.", type = "error")
        return(NULL)
      }
      div(ggplotly(plot, source = 'alarm_density', tooltip = "text"), style = "padding-bottom: 50px;") # Adding padding
    }, error = function(e) {
      # Log the error message and notify the user
      message <- paste("Error in generating plot:", e$message)
      cat(message, "\n") # This prints the error message in the R console
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
  
  output$table_menu6_ui <- renderUI({
    # Check if the input has been initialized and is not empty; if not, prompt for selection
    if (is.null(input$select_menu6) || length(input$select_menu6) < 2) {
      # Display a message asking for more agents to be selected
      tags$h3("Please select at least two agents", 
              style = "text-align: center; margin-top: 180px;")
    } else {
      # Now that we know at least two agents are selected, process the data
      req(surges_reactive$data)  # Ensure data is available
      
      # Proceed only if the data is not empty
      if (nrow(surges_reactive$data) == 0) {
        # No data available in the selected period
        tags$h3("No data available for the selected period.", 
                style = "text-align: center; margin-top: 180px;")
      } else {
        overlapping_df <- find_overlapping_alarms(surges_reactive$data)
        if (nrow(overlapping_df) > 0) {
          colnames(overlapping_df) <- c("Agent", "Overlaps with", "Overlap Start", "Overlap End")
          # Render the datatable if we have overlaps
          DT::datatable(overlapping_df, options = list(pageLength = 5))
        } else {
          # Handle case where there are no overlaps found
          tags$h3("No overlaps found", 
                  style = "text-align: center; margin-top: 180px;")
        }
      }
    }
  })

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
  
  # Output for the label
  output$selectedAgentLabel <- renderUI({
    req(input$select_menu6)
    if (length(input$select_menu6) > 0) {
      selected_agent <- input$select_menu6[1]
      h4("Notable periods for the agent:", strong(selected_agent), style = "margin-bottom: 10px;")
    } else {
      return(NULL)
    }
  })
  
  # Conditional display content
  output$contentDisplay <- renderUI({
    if (length(input$select_menu6) > 0) {
      # If there's a selection, display the tables
      div(style = "height: 400px; overflow-y: auto;",
          column(6, 
                 div(class = "scrollable-mainTable",
                     DTOutput("mainTable"))),
          column(6, uiOutput("detailsPanel"))
      )
    } else {
      # If no selection, display the centered message
      div(style = "display: flex; justify-content: center; align-items: center; height: 400px;",
          h3("Select an agent first", style = "text-align: center;")
      )
    }
  })
  
  output$selectedAgentLabel <- renderUI({
    req(input$select_menu6)  # Ensure there is at least one selection
    selected_agent <- input$select_menu6[1]  # Take the first selected element
    
    # Return a styled HTML output
    HTML(sprintf(
      '<div style="padding-bottom: 10px;"><h4 style="font-size: 16px;">Notable periods for the agent: <strong>%s</strong></h4></div>',
      selected_agent
    ))
  })
  
  overlap_details <- reactive({
    req(input$select_menu6)  # Ensure that the input is not NULL or empty
    if (length(input$select_menu6) > 0) {
      agent_name <- input$select_menu6[1]  # Use the first selected agent
      overlaps_data <- list()
      
      overlaps_data$raw_overlaps <- analyze_overlaps(agent_name)
      overlaps_data$summarized_data <- overlaps_data$raw_overlaps %>%
        select(Start1, End1) %>%
        distinct() %>%
        arrange(Start1, End1)
      
      return(overlaps_data)
    } else {
      return(list(raw_overlaps = data.frame(), summarized_data = data.frame()))  # Return structured empty data
    }
  })
  
  # Main table output utilizing overlap_details
  output$mainTable <- renderDT({
    req(overlap_details())
    formatted_summarized_data <- overlap_details()$summarized_data %>%
      mutate(`Surge start` = format(Start1, "%d-%m-%Y %T"),
             `Surge end` = format(End1, "%d-%m-%Y %T")) %>%
      select(`Surge start`, `Surge end`)
    
    datatable(formatted_summarized_data, selection = 'single', rownames = FALSE, options = list(
      dom = 't', # This option is to remove the datatable's controls and only show the table
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
  }, server = FALSE)
  
  # Details panel UI based on selection in the main table
  output$detailsPanel <- renderUI({
    if (is.null(input$mainTable_rows_selected)) {
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 200px;",
        h4("Select timeframe for details")
      )
    } else {
      selectedRow <- input$mainTable_rows_selected
      detailData <- overlap_details()$raw_overlaps %>%
        filter(Start1 == overlap_details()$summarized_data$Start1[selectedRow], 
               End1 == overlap_details()$summarized_data$End1[selectedRow]) %>%
        select(Agent = Filter2, From = Start2, Till = End2)
      
      dataTableOutput("detailsTable")
    }
  })
  
  output$detailsTable <- renderDataTable({
    req(input$mainTable_rows_selected)
    selectedRow <- input$mainTable_rows_selected
    
    detailData <- overlap_details()$raw_overlaps %>%
      filter(Start1 == overlap_details()$summarized_data$Start1[selectedRow], 
             End1 == overlap_details()$summarized_data$End1[selectedRow]) %>%
      select(Agent = Filter2, From = Start2, Till = End2) %>%
      mutate(
        From = format(From, "%d-%m-%Y %T"),
        Till = format(Till, "%d-%m-%Y %T")
      ) %>%
      arrange(`From`)
    
    datatable(detailData, rownames = FALSE, options = list(
      dom = 't',
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
  })
}

shinyApp(ui = ui, server = server)
