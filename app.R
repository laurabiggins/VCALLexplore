library(shiny)
library(shinydashboard)
library(shinyalert)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)

joined <- readRDS("data/joined.rds")
vcalls <- joined %>%
  dplyr::distinct(V_CALL) %>%
  dplyr::pull()

cols_to_round <- grep("percent|fold_change", colnames(joined))

table_data <- joined
table_data$pos <- as.integer(table_data$pos)

available_datasets <- list.files(path = "data", pattern=".rds") |>
  stringr::str_remove(pattern = ".rds")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  shinyalert::useShinyalert(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel("", windowTitle = "V CALLS"),
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::virtualSelectInput(
           inputId="dataset1_selector", 
           label = "Choose dataset 1", 
           choices = available_datasets
          )
        ),
        column(width = 3,
          shinyWidgets::virtualSelectInput(
           inputId="dataset2_selector", 
           label = "Choose dataset 2", 
           choices = available_datasets,
           selected = available_datasets[2]
          )
        ),
        column(
          width = 2,
          br(),
          shinyWidgets::actionBttn(
            inputId = "load_datasets",
            label = "Load",
            style = "jelly", 
            color = "royal"
          )
        )
      ),
      shinyWidgets::virtualSelectInput(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = "",
        search = TRUE
      ),
      box(
        id = "aa_lengths", 
        title = "amino acid lengths", 
        width = 3, 
        plotOutput("aa_length_plot1")#,
        #plotOutput("aa_length_plot2")
      ),
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
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$browser, browser())
  
  shinyjs::hideElement("plot_div")
  shinyjs::disable("vcall_selector")
  
  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  observeEvent(input$load_datasets, {
    
    if(input$dataset1_selector == input$dataset2_selector){
      shinyalert::shinyalert(text = "Choose 2 different datasets", type="warning")
    }
    else {
      ds1(readRDS(paste0("data/", input$dataset1_selector, ".rds")))
      ds2(readRDS(paste0("data/", input$dataset2_selector, ".rds")))
      allV <- unique(c(ds1()$V_calls, ds2()$V_calls))
      shinyWidgets::updateVirtualSelect(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = allV
      )
      shinyjs::enable("vcall_selector")
    }
  })
  
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
  
  aa_lengths <- reactive({
    x1 <- ds1()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(ds = ds1()$name)
    
    ds2()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(ds = ds2()$name) |>
      dplyr::bind_rows(x1)
  })
  
  output$aa_length_plot1 <- renderPlot({
    
    req(aa_lengths())
    
    ggplot(aa_lengths(), aes(n_aa, fill = ds)) +
      geom_density(adjust = 3, alpha =0.5, colour = "black")
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





