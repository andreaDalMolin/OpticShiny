library(plotly)
library(shiny)
library(htmlwidgets)

# Initial dataset
initDF <- data.frame(x = LETTERS[1:10], y = 1:10)

ui <- fluidPage(
  plotlyOutput("myBarPlot")
)

server <- function(input, output, session) {
  
  js <- "
    function(el, x, inputName) {
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      gd.on('plotly_click', function(data){
        var pts = data.points[0];
        var x = pts.x;
        var y = pts.y;
        var coordinates = [x, y];
        Shiny.setInputValue(inputName, coordinates);
      });
    }
  "
  
  # Listening for clicks and showing modal dialog
  observeEvent(input$clickInfo, {
    click_data <- input$clickInfo
    showModal(modalDialog(
      title = "Clicked Bar Information",
      p(paste("Category (X):", click_data[1])),
      p(paste("Value (Y):", click_data[2]))
    ))
  }, ignoreNULL = TRUE)
  
  output$myBarPlot <- renderPlotly({
    plot_ly(data = initDF, x = ~x, y = ~y, type = 'bar') %>%
      onRender(js, data = "clickInfo")
  })
}

shinyApp(ui, server)
