library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)
library(DT)
library(shinycssloaders)

# Sample data including the new entries
# Example revised data structure based on your new format
library(lubridate)

data <- data.frame(
  Filter1 = rep("EMMA", 15),
  Start1 = ymd_hms(c(
    "2024-01-02 00:00:00", "2024-01-02 00:00:00", "2024-01-02 00:00:00",
    "2024-01-02 00:00:00", "2024-01-02 00:00:00", "2024-01-02 00:00:00",
    "2024-01-02 00:00:00", "2024-01-02 00:00:00", "2024-01-09 10:00:00",
    "2024-01-09 10:00:00", "2024-01-09 10:00:00", "2024-01-09 10:00:00",
    "2024-01-09 10:00:00", "2024-01-09 10:00:00", "2024-01-09 10:00:00"
  )),
  End1 = ymd_hms(c(
    "2024-01-02 02:00:00", "2024-01-02 02:00:00", "2024-01-02 02:00:00",
    "2024-01-02 02:00:00", "2024-01-02 02:00:00", "2024-01-02 02:00:00",
    "2024-01-02 02:00:00", "2024-01-02 02:00:00", "2024-01-09 12:00:00",
    "2024-01-09 12:00:00", "2024-01-09 12:00:00", "2024-01-09 12:00:00",
    "2024-01-09 12:00:00", "2024-01-09 12:00:00", "2024-01-09 12:00:00"
  )),
  Filter2 = c(
    "SCOM", "Zabbix", "HA", "StruxureWare", "C-NMS", "EML",
    "SDH", "LucentOMS", "StruxureWare", "SCOM", "SkyWalker", "LucentOMS",
    "NetAct", "EML", "MileStone"
  ),
  Start2 = ymd_hms(c(
    "2024-01-01 20:00:00", "2024-01-01 21:00:00", "2024-01-02 00:00:00",
    "2024-01-02 00:00:00", "2024-01-02 01:00:00", "2024-01-02 01:00:00",
    "2024-01-02 01:00:00", "2024-01-02 01:00:00", "2024-01-09 08:00:00",
    "2024-01-09 08:00:00", "2024-01-09 09:00:00", "2024-01-09 09:00:00",
    "2024-01-09 09:00:00", "2024-01-09 09:00:00", "2024-01-09 09:00:00"
  )),
  End2 = ymd_hms(c(
    "2024-01-02 02:00:00", "2024-01-02 02:00:00", "2024-01-02 02:00:00",
    "2024-01-02 02:00:00", "2024-01-02 03:00:00", "2024-01-02 03:00:00",
    "2024-01-02 03:00:00", "2024-01-02 10:00:00", "2024-01-09 10:00:00",
    "2024-01-09 12:00:00", "2024-01-09 11:00:00", "2024-01-09 13:00:00",
    "2024-01-09 13:00:00", "2024-01-09 13:00:00", "2024-01-09 14:00:00"
  )),
  stringsAsFactors = FALSE
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
      style = "height: 500px; overflow-y: auto;", # Set the box height and make it scrollable
      column(6, DTOutput("mainTable")),
      column(6, uiOutput("detailsPanel"))
    )
  )
)

  
)

server <- function(input, output, session) {
  output$mainTable <- renderDT({
    # Format and rename Start1 and End1 columns for the main table
    formatted_summarized_data <- summarized_data %>%
      mutate(`Surge start` = paste(format(Start1, "%d-%m-%Y"), format(Start1, "%T")),
             `Surge end` = paste(format(End1, "%d-%m-%Y"), format(End1, "%T"))) %>%
      select(`Surge start`, `Surge end`)  # Make sure to only select the necessary columns
    
    datatable(formatted_summarized_data, selection = 'single', rownames = FALSE, options = list(
      dom = 't', # This option is to remove the datatable's controls and only show the table
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
  }, server = FALSE)
  
  output$detailsPanel <- renderUI({
    if (is.null(input$mainTable_rows_selected)) {
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 200px;",
        h4("No data selected")
      )
    } else {
      selectedRow <- input$mainTable_rows_selected
      detailData <- data %>%
        filter(Start1 == summarized_data$Start1[selectedRow], End1 == summarized_data$End1[selectedRow]) %>%
        select(Agent = Filter2, From = Start2, Till = End2)
      
      dataTableOutput("detailsTable")
    }
  })
  
  output$detailsTable <- renderDataTable({
    req(input$mainTable_rows_selected)
    selectedRow <- input$mainTable_rows_selected
    
    detailData <- data %>%
      filter(Start1 == summarized_data$Start1[selectedRow], End1 == summarized_data$End1[selectedRow]) %>%
      select(Agent = Filter2, From = Start2, Till = End2) %>%
      mutate(
        From = format(From, "%d-%m-%Y %T"),
        Till = format(Till, "%d-%m-%Y %T")
      )
    
    datatable(detailData, rownames = FALSE, options = list(
      dom = 't',
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE
    ))
  })
}

shinyApp(ui, server)

