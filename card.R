library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)
library(DT)
library(shinycssloaders)

# Sample data including the new entries
data <- data.frame(
  EMMA = c(rep("EMMA", 4), rep("EMMA", 2)),
  Filter1 = c("SCOM", "Zabbix", "CA_Spectrum", "LucentOMS", "SCOM", "DICA"),
  Start1 = as.POSIXct(c("2024-03-30 20:00:00", "2024-03-30 20:00:00", "2024-03-30 20:00:00", "2024-03-31 15:00:00", "2024-03-28 15:00:00", "2024-03-28 15:00:00")),
  End1 = as.POSIXct(c("2024-03-30 22:00:00", "2024-03-30 22:00:00", "2024-03-30 22:00:00", "2024-03-31 17:00:00", "2024-03-28 19:00:00", "2024-03-28 19:00:00")),
  Start2 = as.POSIXct(c("2024-03-30 20:00:00", "2024-03-30 20:00:00", "2024-03-30 20:00:00", "2024-03-31 14:00:00", "2024-03-28 14:00:00", "2024-03-28 14:00:00")),
  End2 = as.POSIXct(c("2024-03-30 23:00:00", "2024-03-31 06:00:00", "2024-03-30 22:00:00", "2024-03-31 16:00:00", "2024-03-28 16:00:00", "2024-03-28 16:00:00"))
)

# Create a summarized version for the main table
summarized_data <- data %>%
  select(Start1, End1) %>%
  distinct() %>%
  arrange(Start1, End1)

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
    column(
      5,
      box(
        title = "Events",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        column(6, DTOutput("mainTable")),
        column(6, uiOutput("detailsPanel"))
      )
    )
  )
  
)

server <- function(input, output, session) {
  output$mainTable <- renderDT({
    # Format Start1 and End1 columns for the main table
    formatted_summarized_data <- summarized_data %>%
      mutate(Start1 = paste(format(Start1, "%d-%m-%Y"), format(Start1, "%T")),
             End1 = paste(format(End1, "%d-%m-%Y"), format(End1, "%T")))
    
    datatable(formatted_summarized_data, selection = 'single', rownames = FALSE, options = list(
      dom = 't', # This option is to remove the datatable's controls and only show the table
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
  }, server = FALSE)

  output$detailsPanel <- renderUI({
    # If no row is selected, show "No data selected" centered horizontally and vertically
    if (is.null(input$mainTable_rows_selected)) {
      return(div(
        style = "display: flex; justify-content: center; align-items: center; height: 200px;", # Adjust the height as needed
        h4("No data selected")
      ))
    } else {
      selectedRow <- input$mainTable_rows_selected
      detailData <- data %>%
        filter(Start1 == summarized_data$Start1[selectedRow] & End1 == summarized_data$End1[selectedRow]) %>%
        select(Filter1, Start2, End2)
      
      dataTableOutput("detailsTable")
    }
  })
  
  
  output$detailsTable <- renderDataTable({
    req(input$mainTable_rows_selected)
    selectedRow <- input$mainTable_rows_selected
    
    detailData <- data %>%
      filter(Start1 == summarized_data$Start1[selectedRow] & End1 == summarized_data$End1[selectedRow]) %>%
      select(Filter1, Start2, End2) %>%
      mutate(Start2 = format(Start2, "%d-%m-%Y %T"),
             End2 = format(End2, "%d-%m-%Y %T"))
    
    datatable(detailData, rownames = FALSE, options = list(
      dom = 't', # This option is to remove the datatable's controls and only show the table
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE
    ))
  })
}

shinyApp(ui, server)
