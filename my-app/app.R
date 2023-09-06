library(shiny)
library(plotly)
library(gridlayout)
library(DT)


ui <- grid_page(
  layout = c(
    "header  header header",
    "sidebar .      area4 ",
    "table   table  plotly",
    "table   table  plotly"
  ),
  row_sizes = c(
    "100px",
    "1fr",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "0.59fr",
    "1.41fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    title = "Settings",
    item_gap = "12px",
    sliderInput(
      inputId = "bins",
      label = "Number of Bins",
      min = 12,
      max = 100,
      value = 30,
      width = "100%"
    ),
    numericInput(
      inputId = "numRows",
      label = "Number of table rows",
      value = 10,
      min = 1,
      step = 1,
      width = "100%"
    )
  ),
  grid_card_text(
    area = "header",
    content = "Geysers!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    title = "Table",
    item_gap = "12px",
    DTOutput(outputId = "myTable", width = "100%")
  ),
  grid_card(
    area = "plotly",
    title = "Interactive Plot",
    plotlyOutput(
      outputId = "distPlot",
      width = "100%",
      height = "100%"
    )
  ),
  grid_card(
    area = "area4",
    checkboxInput(
      inputId = "myCheckboxInput",
      label = "Checkbox Input",
      value = TRUE,
      width = "100%"
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  

