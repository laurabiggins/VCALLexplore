library(shiny)
library(shinydashboard)
library(magrittr)
library(plotly)
library(DT)

joined <- readRDS("data/joined.rds")
vcalls <- joined %>%
  dplyr::distinct(V_CALL) %>%
  dplyr::pull()

cols_to_round <- grep("percent|fold_change", colnames(joined))

table_data <- joined
table_data$pos <- as.integer(table_data$pos)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel("", windowTitle = "V CALLS"),
  br(),
  div(id = "table_div", DT::dataTableOutput("main_table")),
  br(),
  p(id = "info_msg", "Select a row from the table to display plot for that CDR3"),
  div(
    id = "plot_div",
    plotly::plotlyOutput("AAplot"),
    radioButtons(
      "yaxis_data", 
      label = NULL, 
      inline = TRUE,
      choices=c("difference in proportions" = "percent_diff", "fold change" ="fold_change")
    )
  ),
  actionButton("browser", "browser")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$browser, browser())
  
  shinyjs::hideElement("plot_div")
  
  observeEvent(selected_vcall(), {
    if(isTruthy(selected_vcall())) {
      shinyjs::showElement("plot_div")
      shinyjs::hideElement("info_msg")
    }
    else {
      shinyjs::showElement("info_msg")
      shinyjs::hideElement("plot_div")
    }
  })
  
  selected_vcall <- reactive({
    
    row_number <- input$main_table_rows_selected
    joined$V_CALL[row_number]
  })
  
  output$main_table <- DT::renderDataTable({

    DT::datatable(
      table_data,
      rownames = FALSE,
      filter = list(position = "top"),
      selection = "single",
      options = list(dom = "fltip", pageLength = 10, scrollX = TRUE, autoWidth = FALSE)
      ) %>% 
      formatStyle(0, target = 'row', lineHeight = '50%', `font-size` ='80%') %>%
      formatRound(cols_to_round)
  })
  
  output$AAplot <- renderPlotly({
    req(selected_vcall())
    
    filtered_joined <- joined %>%
      dplyr::filter(V_CALL == selected_vcall()) 
    
    count_values <- filtered_joined %>%
      dplyr::select(aa_count1, aa_count2) %>%
      tidyr::unite(col = both_counts, sep = ":") %>%
      dplyr::pull(both_counts)

    filtered_joined %>%
      plotly::plot_ly(
        x= ~pos, 
        y= ~.data[[input$yaxis_data]], 
        color= ~value, 
        customdata=count_values
      ) %>%
      plotly::add_text(
        text = ~value,
        hovertemplate = '<b>counts DS1:DS2</b>: %{customdata}',
        size = I(20)
      ) %>%
      plotly::layout(
        title = list(text = selected_vcall(), pad = list(t=200, b=200)), # pad doesn't seem to be doing anything
        yaxis = list(title = input$yaxis_data)
      )
    
    

    # this works with more extensive annotation but we lose the colours - I don't know how to get it to work with the colours
    # https://github.com/plotly/plotly.R/issues/1548
    # annotation_list <- split(filtered_joined, seq_len(nrow(filtered_joined)))
    # filtered_joined %>%
    #   plotly::plot_ly(
    #     x= ~pos, 
    #     y= ~.data[[input$yaxis]], 
    #     #color= ~value, 
    #     customdata=annotation_list,
    #     hovertemplate = paste(
    #       '<b>DS1 count</b>: %{customdata.aa_count1}', 
    #       '<br><b>DS2 count</b>: %{customdata.aa_count2}',
    #       '<br><b>DS1 percentage of %{customdata.value} at position %{customdata.pos}: %{customdata.aa_percent1}' 
    #     )) %>%
    #   plotly::add_text(
    #     text = ~value,
    #     #color= ~value,
    #     size = I(20)
    #   ) %>%
    #   plotly::layout(title = selected_vcall())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)





