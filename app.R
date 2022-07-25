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

box_wrapper <- function(box_id, box_title, panel_tags, box_width = 6, collapsible = TRUE) {
  box(
    id = box_id,
    title = box_title,
    width = box_width, 
    class = "plotbox",
    collapsible = collapsible,
    panel_tags)
}


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
        plotOutput("aa_length_plot")
      ),
      box(
        id = "J calls", 
        title = "J call counts", 
        width = 3, 
        plotOutput("Jcall_plot")
        
      ),
      box(
        id = "D calls", 
        title = "D call counts", 
        width = 6, 
        plotOutput("Dcall_plot")
      ),
     # mod_barplotUI(id="myJbarplot"),
      uiOutput("Jcall_barplot"),
      
      
      # div(id = "table_div", DT::dataTableOutput("main_table")),
      # br(),
      # p(id = "info_msg", "Select a row from the table to display plot for that CDR3"),
      # div(
      #   id = "plot_div",
      #   plotly::plotlyOutput("AAplot"),
      #   radioButtons(
      #     "yaxis_data", 
      #     label = NULL, 
      #     inline = TRUE,
      #     choices=c("difference in proportions" = "percent_diff", "fold change" ="fold_change")
      #   )
      # ),
      actionButton("browser", "browser")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$browser, browser())
  
  shinyjs::hideElement("plot_div")
  shinyjs::disable("vcall_selector")
  shinyjs::hide("Jcall_barplot")
  
  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  selectedV <- reactive(input$vcall_selector)

  observeEvent(selectedV(), {
    if(isTruthy(selectedV())){
      shinyjs::show("Jcall_barplot")
    } else {
      shinyjs::hide("Jcall_barplot")
    }
  })
  
  ## Jcalls ----  
  Jcalls1 <- reactive({
    req(ds1())
    ds1()$get_Jcalls(selectedV())
  })
  
  Jcalls2 <- reactive({
    req(ds2())
    ds2()$get_Jcalls(selectedV())
  })
  
  Jcalls <- reactive({
    
    req(Jcalls1(), Jcalls2())
    
    Jcalls1() |>
      dplyr::rename(!!ds1()$name := n) |>
      dplyr::full_join(Jcalls2()) |>
      dplyr::select(-V_CALL) |>
      dplyr::rename(!!ds2()$name := n) |>
      tidyr::pivot_longer(-J_CALL, names_to="dataset", values_to="count")
    
  })
  
  ## Dcalls ----  
  Dcalls1 <- reactive({
    req(ds1())
    ds1()$get_Dcalls(selectedV())
  })
  
  Dcalls2 <- reactive({
    req(ds2())
    ds2()$get_Dcalls(selectedV())
  })
  
  Dcalls <- reactive({
    
    req(Dcalls1(), Dcalls2())
    
    Dcalls1() |>
      dplyr::rename(!!ds1()$name := n) |>
      dplyr::full_join(Dcalls2()) |>
      dplyr::select(-V_CALL) |>
      dplyr::rename(!!ds2()$name := n) |>
      tidyr::pivot_longer(-singleD, names_to="dataset", values_to="count")
    
  })
  
  
  
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

  ## aa lengths ----
    
  aa_lengths <- reactive({
    
    req(ds1(), ds2())
    
    x1 <- ds1()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(ds = ds1()$name)
    
    ds2()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(ds = ds2()$name) |>
      dplyr::bind_rows(x1)
  })
  
  output$aa_length_plot <- renderPlot({
    
    req(aa_lengths())
    
    ggplot(aa_lengths(), aes(n_aa, fill = ds)) +
      geom_density(adjust = 3, alpha =0.5, colour = "black")
  })
  
  output$Jcall_barplot <- renderUI({

    JbarplotUI <- mod_barplotUI(id="Jbarplot")
    box_wrapper(box_id="Jbarplotbox", box_title="J call counts", JbarplotUI)
  })
  
  mod_barplotServer("Jbarplot", ds=Jcalls, feature="J_CALL")
  
  output$Jcall_plot <- renderPlot({
    
    req(Jcalls())
    
    ggplot(Jcalls(), aes(x = J_CALL, y = count, fill = dataset)) +
      geom_col(colour = "black", position = position_dodge2())
    
    #ggplot(Jcalls(), aes(x = dataset, y = count, fill = J_CALL)) +
    #  geom_col(colour = "black", position = position_dodge2())
    
  })
  
  output$Dcall_plot <- renderPlot({
    
    req(Dcalls())
    
    ggplot(Dcalls(), aes(x = singleD, y = count, fill = dataset)) +
      geom_col(colour = "black", position = position_dodge2())
    
    #ggplot(Dcalls(), aes(x = dataset, y = count, fill = singleD)) +
    #  geom_col(colour = "black", position = position_dodge2())
    
  })
  
  
}  
  # observeEvent(selected_vcall(), {
  #   if(isTruthy(selected_vcall())) {
  #     shinyjs::showElement("plot_div")
  #     shinyjs::hideElement("info_msg")
  #   }
  #   else {
  #     shinyjs::showElement("info_msg")
  #     shinyjs::hideElement("plot_div")
  #   }
  # })
  
  # selected_vcall <- reactive({
  #   
  #   row_number <- input$main_table_rows_selected
  #   joined$V_CALL[row_number]
  # })
  
  # output$main_table <- DT::renderDataTable({
  # 
  #   DT::datatable(
  #     table_data,
  #     rownames = FALSE,
  #     filter = list(position = "top"),
  #     selection = "single",
  #     options = list(dom = "fltip", pageLength = 10, scrollX = TRUE, autoWidth = FALSE)
  #     ) %>% 
  #     formatStyle(0, target = 'row', lineHeight = '50%', `font-size` ='80%') %>%
  #     formatRound(cols_to_round)
  # })
  
  # output$AAplot <- renderPlotly({
  #   req(selected_vcall())
  #   
  #   filtered_joined <- joined %>%
  #     dplyr::filter(V_CALL == selected_vcall()) 
  #   
  #   count_values <- filtered_joined %>%
  #     dplyr::select(aa_count1, aa_count2) %>%
  #     tidyr::unite(col = both_counts, sep = ":") %>%
  #     dplyr::pull(both_counts)
  # 
  #   filtered_joined %>%
  #     plotly::plot_ly(
  #       x= ~pos, 
  #       y= ~.data[[input$yaxis_data]], 
  #       color= ~value, 
  #       customdata=count_values
  #     ) %>%
  #     plotly::add_text(
  #       text = ~value,
  #       hovertemplate = '<b>counts DS1:DS2</b>: %{customdata}',
  #       size = I(20)
  #     ) %>%
  #     plotly::layout(
  #       title = list(text = selected_vcall(), pad = list(t=200, b=200)), # pad doesn't seem to be doing anything
  #       yaxis = list(title = input$yaxis_data)
  #     )
    
    

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
    #})
#}

# Run the application 
shinyApp(ui = ui, server = server)





