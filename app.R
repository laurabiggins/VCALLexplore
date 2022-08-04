library(shiny)
library(shinydashboard)
library(shinyjs)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)

# we need to add another function to the R6 class to have AA counts from the right.

# as well as showing the percentage for that V call at that position, have an option to also show 
# the overall proportion of that AA at that position in the whole dataset.

#cols_to_round <- grep("percent|fold_change", colnames(joined))

#table_data <- joined
#table_data$pos <- as.integer(table_data$pos)

available_datasets <- list.files(path = "data", pattern=".rds") |>
  stringr::str_remove(pattern = ".rds")

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


# UI -----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  shinyalert::useShinyalert(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$head(tags$title("V CALLS")),
  #titlePanel(title=NULL, windowTitle = "V CALLS"),
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      ### info message ----
      htmlOutput(outputId = "info_text", inline=FALSE),
      br(),
      tabsetPanel(
        id="control_panel",
        type="hidden",
        tabPanelBody(
          "load_panel",
          fluidRow(
            column(
              width = 4, offset = 4,
              verticalLayout(
                shinyWidgets::virtualSelectInput(
                 inputId="dataset1_selector", 
                 label = "Choose dataset 1", 
                 choices = available_datasets
                ),
                br(),
                shinyWidgets::virtualSelectInput(
                 inputId="dataset2_selector", 
                 label = "Choose dataset 2", 
                 choices = available_datasets,
                 selected = available_datasets[2]
                ),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    actionButton(inputId = "load_datasets", label = "Load")
                  ),
                  column(
                    width = 6,
                    actionButton(inputId = "select_Vgene_btn", label = "Select V gene")
                  )
                )
              )
            )
          )
        ),
        tabPanelBody(
          "v_selector_panel", 
          fluidRow(
           column(
             width = 3, offset = 1, 
             br(),
             shinyWidgets::virtualSelectInput(
               inputId="vcall_selector", 
               label = "Select V gene", 
               choices = "",
               search = TRUE
             )
           ),
           column(width = 2, br(), br(), actionButton("next_Vgene", "Next V gene")),
           column(width = 2, offset = 4, br(), br(), actionButton("change_datasets", "Change datasets"))
          ),
          br(),
          div(
            id="main_plots", 
            ## main plots ----
            box_wrapper(box_id="Jbarplotbox", box_title="J call counts", mod_barplotUI(id="Jbarplot")),
            box_wrapper(box_id="Dbarplotbox", box_title="D call counts", mod_barplotUI(id="Dbarplot")),
            box_wrapper(box_id="np1plotbox", box_title="np 1 lengths", mod_densityplotUI(id="np1plot")),
            box_wrapper(box_id="np2plotbox", box_title="np 2 lengths", mod_densityplotUI(id="np2plot")),
  
            ### AA letter plots ----
            box_wrapper(
              box_id="AAplotbox", 
              box_title="AA counts",
              plotly::plotlyOutput("AAplot"),
              box_width = 12
            )
          )
        )
      ),
      
      # ## small AA plot ----
      # box_wrapper(
      #   box_id="aa_lengths", 
      #   box_title="amino acid lengths", 
      #   box_width = 3, 
      #   mod_densityplotUI(id="aa_length_plot", plot_height=200)
      # ),
      br(), br(),
      actionButton("browser", "browser")#,
      #actionButton("change_datasets", "Change datasets")
    )
  )
)




# server ----
server <- function(input, output, session) {

  observeEvent(input$browser, browser())
  
  #shinyjs::hideElement("plot_div")
  shinyjs::disable("vcall_selector")
  shinyjs::hide("next_Vgene")
  shinyjs::hide("select_Vgene_btn")
  shinyjs::hide("main_plots")
  #shinyjs::hide("aa_lengths")
  #hidePlots()
  #hide_all()
  
  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  selectedV <- reactive(input$vcall_selector)
  
  dataset_msg <- reactiveVal("Select and load 2 datasets")
  v_gene_msg <- reactiveVal("Select a V gene to see more information")
  
  observeEvent(input$select_Vgene_btn, {
    #print("show v gene panel")
    updateTabsetPanel(session, "control_panel", selected = "v_selector_panel")
  })
  observeEvent(input$change_datasets, {
    updateTabsetPanel(session, "control_panel", selected = "load_panel")
  })
  
  ## info message ----
  info_msg <- reactive({
    
    if(!isTruthy(ds1()) | !isTruthy(ds2())) {
      msg <- p(class="info_text", style="text-align:center;", dataset_msg())
    } else {
      msg <- p(span(class="info_text", style="float:left;", dataset_msg()), span(class="info_text", style="float:right;", v_gene_msg()))
    }
    msg
    
  })
  
  output$info_text <- renderUI(info_msg())
  
  allVgenes <- reactiveVal()
  
  ## Inputs - observeEvents ----
  ### Load the 2 datasets ---- 
  observeEvent(input$load_datasets, {
    
    if(input$dataset1_selector == input$dataset2_selector){
      #dataset_msg("Choose 2 different datasets!")
      shinyalert::shinyalert(text = "Choose 2 different datasets", type="warning")
    }
    else {
      ds1(readRDS(paste0("data/", input$dataset1_selector, ".rds")))
      ds2(readRDS(paste0("data/", input$dataset2_selector, ".rds")))
      #allV <- unique(c(ds1()$V_calls, ds2()$V_calls))
      allVgenes(unique(c(ds1()$V_calls, ds2()$V_calls)))
      shinyWidgets::updateVirtualSelect(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = allVgenes()
      )
      shinyjs::enable("vcall_selector")
      shinyjs::show("aa_lengths")
      shinyjs::show("select_Vgene_btn")
      dataset_msg(paste0("Datasets loaded: ", ds1()$name, ", ", ds2()$name))
      #show_all()
    }
  }) 
  
  ### select VCALL ----
  observeEvent(selectedV(), {
    if(isTruthy(selectedV())) {
      shinyjs::show("next_Vgene")
      v_gene_msg(paste0("V gene:  ", selectedV()))
      shinyjs::show("main_plots")
    }
    else {
      shinyjs::hide("next_Vgene")
      shinyjs::hide("main_plots")
      v_gene_msg("Select a V gene to see more information")
    }
  })
  
  ### next button ----
  observeEvent(input$next_Vgene, {
    current_index <- which(allVgenes() == selectedV())
    shinyWidgets::updateVirtualSelect(
      inputId="vcall_selector", selected = allVgenes()[current_index+1] 
    )
    #browser()
  })
  
  ## Reactive data for plots ----
  
  ### aa lengths ----
  aa_lengths <- reactive({
    
    req(ds1(), ds2())
    
    dataset1_aa <- ds1()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(dataset = ds1()$name)
    
    ds2()$aa_lengths |>
      dplyr::select(n_aa) |>
      tibble::add_column(dataset = ds2()$name) |>
      dplyr::bind_rows(dataset1_aa)
  })
  
  ### np lengths ----
  #### np1&2 lengths for dataset 1 ----
  np_lengths_ds1 <- reactive({
    req(ds1())
    ds1()$get_np_lengths(selectedV())
  })
  
  np_lengths_ds2 <- reactive({
    req(ds2())
    ds2()$get_np_lengths(selectedV())
  })
    
  np1_lengths <- reactive({
    
    req(np_lengths_ds1(), np_lengths_ds2())
    
    dataset1_np1 <- np_lengths_ds1() |>
      dplyr::select(NP1_LENGTH) |>
      tibble::add_column(dataset = ds1()$name)
    
    np_lengths_ds2() |>
      dplyr::select(NP1_LENGTH) |>
      tibble::add_column(dataset = ds2()$name) |>
      dplyr::bind_rows(dataset1_np1)
  })
  
  np2_lengths <- reactive({
    
    req(np_lengths_ds1(), np_lengths_ds2())
    
    dataset1_np2 <- np_lengths_ds1() |>
      dplyr::select(NP2_LENGTH) |>
      tibble::add_column(dataset = ds1()$name)
    
    np_lengths_ds2() |>
      dplyr::select(NP2_LENGTH) |>
      tibble::add_column(dataset = ds2()$name) |>
      dplyr::bind_rows(dataset1_np2)
  })
  
  
  ### Jcalls ----  
  # keep these separate as one dataset may change while the other stays the same.
  Jcalls1 <- reactive({
    req(ds1())
    ds1()$get_Jcalls(selectedV())
  })
  
  Jcalls2 <- reactive({
    req(ds2())
    ds2()$get_Jcalls(selectedV())
  })
  
  # joined Jcalls for using in plot
  Jcalls <- reactive({
    
    req(Jcalls1(), Jcalls2())
    
    J2 <- Jcalls2() |>
      tibble::add_column(dataset = ds2()$name)
    
    Jcalls1() |>
      tibble::add_column(dataset = ds1()$name) |>
      dplyr::bind_rows(J2)

  })
  
  ### Dcalls ----  
  Dcalls1 <- reactive({
    req(ds1())
    ds1()$get_Dcalls(selectedV())
  })
  
  Dcalls2 <- reactive({
    req(ds2())
    ds2()$get_Dcalls(selectedV())
  })
  
  # joined Dcalls for using in plot
  Dcalls <- reactive({
    
    req(Dcalls1(), Dcalls2())
    
    D2 <- Dcalls2() |>
      tibble::add_column(dataset = ds2()$name)
    
    Dcalls1() |>
      tibble::add_column(dataset = ds1()$name) |>
      dplyr::bind_rows(D2)
  })
  
  ### AA positions ----
  #### all joined data ----
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
  
  #### filtered by VCALL ----
  aa_joined_data <- reactive(dplyr::filter(joined(), V_CALL==input$vcall_selector))
  
  ## Plots ----
  
  ### AA lengths ----
  mod_densityplotServer("aa_length_plot", ds=aa_lengths, feature="n_aa", colour_palette=colour_palette)
  
  ### Dcalls ----
  mod_barplotServer("Dbarplot", ds=Dcalls, feature="singleD", colour_palette=colour_palette)
  
  ### Jcalls ----
  mod_barplotServer("Jbarplot", ds=Jcalls, feature="J_CALL", colour_palette=colour_palette)
  
  ### NP1 lengths ----
  mod_densityplotServer("np1plot", ds=np1_lengths, feature="NP1_LENGTH", colour_palette=colour_palette)
  
  ### NP2 lengths ----
  mod_densityplotServer("np2plot", ds=np2_lengths, feature="NP2_LENGTH", colour_palette=colour_palette)
  
  ### AA positions ----
  output$AAplot <- renderPlotly({
    
    aa_joined_data() |>
      plotly::plot_ly(x= ~pos, y= ~percent_diff_vcall, color= ~value, colors = extra_hex_cols) |>
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





