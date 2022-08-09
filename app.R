library(shiny)
library(shinydashboard)
library(shinyjs)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)

# we need to add another function to the R6 class to have AA counts from the right.

#cols_to_round <- grep("percent|fold_change", colnames(joined))

#table_data <- joined
#table_data$pos <- as.integer(table_data$pos)

available_datasets <- list.files(path = "data", pattern=".rds") |>
  stringr::str_remove(pattern = ".rds")

# UI -----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  shinyalert::useShinyalert(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$head(tags$title("V CALLS")),
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      ### info message ----
      htmlOutput(outputId = "info_text", inline=FALSE),
      tabsetPanel(
        id="control_panel",
        type="hidden",
        tabPanelBody(
          "load_panel",
          br(),
          fluidRow(
            column(
              width = 4, offset = 4,
              box(id = "load_panel_id", title = NULL, collapsible = FALSE, width = NULL,
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
                  actionButton(inputId = "load_datasets", label = "Load")
                )
              )
            )
          )
        ),
        tabPanelBody("v_selector_panel", 
          br(),
          fluidRow(
            column(width = 3, offset = 1,
              shinyWidgets::virtualSelectInput(
                 inputId="vcall_selector", 
                 label = "Select V gene", 
                 choices = "",
                 search = TRUE
              )
            ),
            column(width = 2, br(), actionButton("next_Vgene", "Next V gene"))
          )
        )
      ),
      br(),
      ## main plots ----
      div(
        id="main_plots",
        box_wrapper(box_id="Jbarplotbox", box_title="J call counts", mod_barplotUI(id="Jbarplot")),
        box_wrapper(box_id="Dbarplotbox", box_title="D call counts", mod_barplotUI(id="Dbarplot")),
        box(box_id="length_plots", title = "Length plots", width = 12, collapsible = TRUE, collapsed = TRUE,
            fluidRow(
              column(width = 4, mod_densityplotUI(id="np1plot", plot_height=250)),
              column(width = 4, mod_densityplotUI(id="np2plot", plot_height=250)),
              column(width = 4, mod_densityplotUI(id="aa_length_plot", plot_height=250))
            )
        ),
        box_wrapper(box_id="AAplotbox", box_width = 12, box_title="AA", mod_letterplotUI(id="AA_plot"))#,
       # actionButton("browser", "browser")
      )
    )
  )
)

# server ----
server <- function(input, output, session) {

  observeEvent(input$browser, browser())
  
  shinyjs::hide("next_Vgene")
  shinyjs::hide("main_plots")

  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  selectedV <- reactive(input$vcall_selector)
  
  dataset_msg <- reactiveVal("Select and load 2 datasets")

  observeEvent(input$change_datasets, {
    updateTabsetPanel(session, "control_panel", selected = "load_panel")
    shinyjs::hide("main_plots")
  })
  
  ## info message ----
  info_msg <- reactive({
    
    if(!isTruthy(ds1()) | !isTruthy(ds2())) {
      msg <- p(class="info_text", style="text-align:center;", dataset_msg())
    } else {
      msg <- p(span(class="info_text", style="float:right;", dataset_msg(), actionButton("change_datasets", "Change")))
    }
    msg
  }) |>
    bindCache(dataset_msg()) |>
    bindEvent(dataset_msg())
  
  output$info_text <- renderUI(info_msg())
  
  allVgenes <- reactiveVal()
  
  ## Inputs - observeEvents ----
  ### Load the 2 datasets ---- 
  observeEvent(input$load_datasets, {
    
    if(input$dataset1_selector == input$dataset2_selector){
      shinyalert::shinyalert(text = "Choose 2 different datasets", type="warning")
    }
    else {
      ds1(readRDS(paste0("data/", input$dataset1_selector, ".rds")))
      ds2(readRDS(paste0("data/", input$dataset2_selector, ".rds")))
      allVgenes(unique(c(ds1()$V_calls, ds2()$V_calls)))
      shinyWidgets::updateVirtualSelect(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = allVgenes()
      )
      dataset_msg(paste0("Datasets loaded: ", ds1()$name, ", ", ds2()$name))
      updateTabsetPanel(session, "control_panel", selected = "v_selector_panel")
    }
  }) 
  
  ### select VCALL ----
  observeEvent(selectedV(), {
    if(isTruthy(selectedV())) {
      shinyjs::show("next_Vgene")
      shinyjs::show("main_plots")
    }
    else {
      shinyjs::hide("next_Vgene")
      shinyjs::hide("main_plots")
    }
  })
  
  ### next button ----
  observeEvent(input$next_Vgene, {
    current_index <- which(allVgenes() == selectedV())
    shinyWidgets::updateVirtualSelect(
      inputId="vcall_selector", selected = allVgenes()[current_index+1] 
    )
  })
  
  ## Reactive data for plots ----
  
  ### aa lengths ----
  aa_lengths <- reactive({
    
    req(ds1(), ds2())
    
    dataset1_aa <- ds1()$get_aa_lengths(selectedV()) |>
      dplyr::select(n_aa) |>
      tibble::add_column(dataset = ds1()$name)
    
    ds2()$get_aa_lengths(selectedV()) |>
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
    tidy_aa_joined_data()
  }) |>
    bindCache(input$dataset1_selector, input$dataset2_selector) |>
    bindEvent(input$load_datasets)
  
 
  #### filtered by VCALL ----
  aa_joined_data <- reactive(dplyr::filter(joined(), V_CALL==input$vcall_selector))
  
  ## Plots ----
  
  ### AA lengths ----
  mod_densityplotServer("aa_length_plot", ds=aa_lengths, feature="n_aa", feature_formatted="CDR3 lengths", selected_V=selectedV,  colour_palette=colour_palette)
  
  ### Dcalls ----
  mod_barplotServer("Dbarplot", ds=Dcalls, feature="singleD", feature_formatted="D call", selected_V=selectedV, colour_palette=colour_palette)
  
  ### Jcalls ----
  mod_barplotServer("Jbarplot", ds=Jcalls, feature="J_CALL", feature_formatted="J call", selected_V=selectedV, colour_palette=colour_palette)
  
  ### NP1 lengths ----
  mod_densityplotServer("np1plot", ds=np1_lengths, feature="NP1_LENGTH", feature_formatted="NP1 lengths", selected_V=selectedV, colour_palette=colour_palette)
  
  ### NP2 lengths ----
  mod_densityplotServer("np2plot", ds=np2_lengths, feature="NP2_LENGTH",  feature_formatted="NP2 lengths", selected_V=selectedV, colour_palette=colour_palette)
  
  ### AA as letters  ----
  mod_letterplotServer("AA_plot", ds=aa_joined_data, raw_colours=extra_hex_cols, selected_V=selectedV, ds1_name=reactive(ds1()$name), ds2_name=reactive(ds2()$name))
}  


# Run the application 
shinyApp(ui = ui, server = server)

