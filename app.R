library(shiny)
library(shinydashboard)
library(shinyalert)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)

# we need to add another function to the R6 class to have AA counts from the right.

# as well as showing the percentage for that V call at that position, have an option to also show 
# the overall proportion of that AA at that position in the whole dataset.

#joined <- readRDS("data/joined.rds")
#vcalls <- joined %>%
#  dplyr::distinct(V_CALL) %>%
#  dplyr::pull()

#cols_to_round <- grep("percent|fold_change", colnames(joined))

#table_data <- joined
#table_data$pos <- as.integer(table_data$pos)

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
        ),
        column(
          width= 4,
          box(
           id = "aa_lengths", 
           title = "amino acid lengths", 
           width = 12, 
           collapsible = TRUE,
           plotOutput("aa_length_plot", height = "200px")
          )
        )
      ),
      shinyWidgets::virtualSelectInput(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = "",
        search = TRUE
      ),
      # box(
      #   id = "aa_lengths", 
      #   title = "amino acid lengths", 
      #   width = 3, 
      #   collapsible = TRUE,
      #   plotOutput("aa_length_plot")
      # ),
      uiOutput("np1_lengths_plot"),
      uiOutput("np2_lengths_plot"),
      uiOutput("Jcall_barplot"),
      uiOutput("Dcall_barplot"),
      
      box_wrapper(
        box_id="AAplotbox", 
        box_title="AA counts",
        plotly::plotlyOutput("AAplot"),
        box_width = 12
      ),
       # plotly::plotlyOutput("AAplot"),
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
  shinyjs::hide("Dcall_barplot")
  shinyjs::hide("AAplotbox")
  shinyjs::hide("aa_lengths")
  
  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  selectedV <- reactive(input$vcall_selector)

  observeEvent(selectedV(), {
    if(isTruthy(selectedV())){
      shinyjs::show("Jcall_barplot")
      shinyjs::show("Dcall_barplot")
      shinyjs::show("AAplotbox")
    } else {
      shinyjs::hide("Jcall_barplot")
      shinyjs::hide("Dcall_barplot")
      shinyjs::hide("AAplotbox")
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
    
    J2 <- Jcalls2() |>
      tibble::add_column(dataset = ds2()$name)
    
    Jcalls1() |>
      tibble::add_column(dataset = ds1()$name) |>
      dplyr::bind_rows(J2)

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
    
    D2 <- Dcalls2() |>
      tibble::add_column(dataset = ds2()$name)
    
    Dcalls1() |>
      tibble::add_column(dataset = ds1()$name) |>
      dplyr::bind_rows(D2)
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
      shinyjs::show("aa_lengths")
    }
  })  

  aa_joined_data <- reactive(dplyr::filter(joined(), V_CALL==input$vcall_selector))

  ## joined data ----
  joined <- reactive({
    dplyr::full_join(ds1()$aa_counts_left, ds2()$aa_counts_left, by = c("V_CALL", "pos", "value")) |>
    dplyr::mutate(percent_diff_vcall = aa_ds_percent.x-aa_ds_percent.y) |>
    dplyr::mutate(percent_diff_ds = aa_ds_percent.x-aa_ds_percent.y) |>
    dplyr::mutate(fold_change_vcall = dplyr::if_else(aa_percent.x>=aa_percent.y, aa_percent.x/aa_percent.y, -(aa_percent.y/aa_percent.x))) |>
    dplyr::mutate(fold_change_vcall = dplyr::if_else(is.infinite(fold_change_vcall), percent_diff_vcall, fold_change_vcall)) |>
    dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall > 100, 100)) |>
    dplyr::mutate(fold_change_vcall = replace(fold_change_vcall, fold_change_vcall < -100, -100)) |>
    dplyr::mutate(fold_change_ds = dplyr::if_else(aa_ds_percent.x>=aa_ds_percent.y, aa_ds_percent.x/aa_ds_percent.y, -(aa_ds_percent.y/aa_ds_percent.x))) |>
    dplyr::mutate(fold_change_ds = dplyr::if_else(is.infinite(fold_change_ds), percent_diff_ds, fold_change_ds)) |>
    dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds > 100, 100)) |>
    dplyr::mutate(fold_change_ds = replace(fold_change_ds, fold_change_ds < -100, -100))
  }) |>
    bindCache(input$dataset1_selector, input$dataset2_selector) |>
    bindEvent(input$load_datasets)
  
  
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
  
  
  ## np lengths -----
  output$np1_lengths_plot <- renderUI({
    
    box_wrapper(box_id="np1plotbox", box_title="np 1 lengths", plotOutput("np1_lengths"))
    
  })
  
  output$np2_lengths_plot <- renderUI({
    
    box_wrapper(box_id="np2plotbox", box_title="np 2 lengths", plotOutput("np2_lengths"))
    
  })
  
  output$np1_lengths <- renderPlot(plot(1:10))
  output$np2_lengths <- renderPlot(plot(1:10))
  
  output$Jcall_barplot <- renderUI({

    JbarplotUI <- mod_barplotUI(id="Jbarplot")
    box_wrapper(box_id="Jbarplotbox", box_title="J call counts", JbarplotUI)
  })
  
  output$Dcall_barplot <- renderUI({
    
    DbarplotUI <- mod_barplotUI(id="Dbarplot")
    box_wrapper(box_id="Dbarplotbox", box_title="D call counts", DbarplotUI)
  })
  
  mod_barplotServer("Dbarplot", ds=Dcalls, feature="singleD")
  mod_barplotServer("Jbarplot", ds=Jcalls, feature="J_CALL")
  
  output$AAplot <- renderPlotly({
    
    aa_joined_data() |>
      plotly::plot_ly(x= ~pos, y= ~percent_diff_vcall, color= ~value) %>%
        plotly::add_text(
          text = ~value,
          #hovertext = ~name,
          #hoverinfo = "text",
          size = I(20)
        )
  })
}  


# Run the application 
shinyApp(ui = ui, server = server)





