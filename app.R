library(shiny)
library(shinydashboard)
library(shinyjs)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)
source("R/processing_utils.R")

# we need to add another function to the R6 class to have AA counts from the right.

#cols_to_round <- grep("percent|fold_change", colnames(joined))

#table_data <- joined
#table_data$pos <- as.integer(table_data$pos)

available_datasets <- local(list.files(path = "data", pattern=".rds")) |>
  stringr::str_remove(pattern = ".rds")

# UI -----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  #shinyalert::useShinyalert(),
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
                   choices = available_datasets,
                   selected = "BC"
                  ),
                  br(),
                  shinyWidgets::virtualSelectInput(
                   inputId="dataset2_selector", 
                   label = "Choose dataset 2", 
                   choices = available_datasets,
                   #selected = available_datasets[2]
                   selected = "T1"
                  ),
                  br(),
                  actionButton(inputId = "load_datasets", label = "Load")
                )
              )
            )
          )
        ),
        ## filters ----
        tabPanelBody(
          "v_selector_panel",
          fluidRow(
            column(width = 4,
              tabsetPanel(
                id = "vtype_selection", 
                tabPanel(title = "V gene",
                         value = "vgene",
                  br(),
                  fluidRow(
                    column(width = 6, offset = 1,
                      shinyWidgets::virtualSelectInput(
                         inputId="vcall_selector", 
                         label = "Select V gene", 
                         choices = "",
                         search = TRUE
                      )
                    ),
                    column(width = 5, br(), actionButton("next_Vgene", "Next V gene"))
                  )
                ),
                tabPanel(
                  title = "V group",
                  value = "vgroup",
                  br(),
                  fluidRow(
                    column(width = 7, offset = 1,
                           shinyWidgets::virtualSelectInput(
                             inputId="vgroup_selector", 
                             label = "Select V group", 
                             choices = "",
                             search = TRUE
                           )
                    ),
                    column(width = 4, br(), actionButton("next_Vgroup", "Next V group"))
                  )
                ),
                tabPanel(
                  title = "All V genes",
                  value = "allV",
                  # this should just work without a button
                  h2("Using all V genes", id="all_Vgenes_msg")
                  #actionButton(inputId = "allVgenes", label = "Combine all V calls")
                )
              ),
            ),
            column(
              width = 2, 
              # this doesn't need to be a tabset panel but it does then match with the V selection
              tabsetPanel(
                id = "drf", 
                tabPanel(
                  title = "D reading frame",
                  br(),
                  fluidRow(
                    column(width = 10, offset = 1,
                      shinyWidgets::awesomeRadio(
                        inputId="drf_selector", 
                        label = NULL, 
                        choices = c("All", 1:3),
                        selected = "All"
                      )
                    )  
                  )
                )
              )
            ),
            column(
              width = 2,   
              tabsetPanel(
                id = "cdr3_length_tab", 
                tabPanel(
                  title = "CDR3 length",
                  br(),
                  verticalLayout(
                    fluidRow(
                      column(width = 10, offset = 1,
                        shinyWidgets::awesomeRadio(
                         inputId="cdr3_selector", 
                         label = NULL, 
                         choices = c("All", "Custom"),
                         selected = "All"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.cdr3_selector == 'Custom'",
                      shinyWidgets::chooseSliderSkin("Modern"),
                      sliderInput(
                        inputId = "cdr3_length",
                        label = NULL, 
                        min = 9,
                        max = 15, 
                        value = 12
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      br(),
      br(),
      ## main plots ----
      div(
        id="main_plots",
        box_wrapper(box_id="Jbarplotbox", box_title="J call counts", mod_barplotUI(id="Jbarplot")),
        box_wrapper(box_id="Dbarplotbox", box_title="D call counts", mod_barplotUI(id="Dbarplot")),
        #box(box_id="length_plots", title = "Length plots", width = 12, collapsible = TRUE, collapsed = TRUE,
        box_wrapper(
          box_id="length_plots", box_title = "Length plots", box_width = 12,    
          fluidRow(
            column(width = 4, mod_densityplotUI(id="np1plot", plot_height=250)),
            column(width = 4, mod_densityplotUI(id="np2plot", plot_height=250)),
            column(width = 4, mod_densityplotUI(id="aa_length_plot", plot_height=250))
          )
        ),
        box_wrapper(box_id="AAplotbox", box_width = 12, box_title="AA", mod_letterplotUI(id="AA_plot"))#,
        #actionButton("browser", "browser")
      )
    )
  )
)

# server ----
server <- function(input, output, session) {

  observeEvent(input$browser, browser())
  
  shinyjs::hide("next_Vgene")
  shinyjs::hide("next_Vgroup")
  shinyjs::hide("main_plots")

  ds1 <- reactiveVal()
  ds2 <- reactiveVal()
  
  # vtype  - determined by the selected tab - vgene, vgroup or allV
  vtype <- reactive(input$vtype_selection)
  
  # an individual v gene
  selectedV <- reactive(input$vcall_selector)
  # a group of selected v genes
  selectedVgroup <- reactive(input$vgroup_selector)
  
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
      msg <- p(span(class="info_text", style="float:right;", dataset_msg(), br(), actionButton("change_datasets", "Change")))
    }
    msg
  }) |>
    bindCache(dataset_msg()) |>
    bindEvent(dataset_msg())
  
  output$info_text <- renderUI(info_msg())
  
  allVgenes <- reactiveVal()
  allVgroups <- reactiveVal()
  
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
      allVgroups(unique(c(ds1()$V_groups, ds2()$V_groups)))
      shinyWidgets::updateVirtualSelect(
        inputId="vcall_selector", 
        label = "Select V gene", 
        choices = allVgenes()
      )
      shinyWidgets::updateVirtualSelect(
        inputId="vgroup_selector", 
        label = "Select V group", 
        choices = allVgroups()
      )
      dataset_msg(paste0("Datasets loaded: ", ds1()$name, ", ", ds2()$name))
      updateTabsetPanel(session, "control_panel", selected = "v_selector_panel")
      shinyjs::show("main_plots")
    }
  }) 

  ### valid selection check ----
  # selected V or all genes or group
  validSelection <- reactive({
    
    if (vtype() == "allV") {
      return(TRUE)
    } else if (vtype() == "vgroup"){
        if (isTruthy(selectedVgroup())) {
          return(TRUE)
        } else {
          return(FALSE) 
        }
    } else if (vtype() == "vgene") {
        if (isTruthy(selectedV())) {
          return(TRUE)
        } else {
          return(FALSE) 
        }
    }
  })
  
  ### select VCALL ----
  observeEvent(selectedV(), {
    if(isTruthy(selectedV())) {
      shinyjs::show("next_Vgene")
      #shinyjs::show("main_plots")
    }
    else {
      shinyjs::hide("next_Vgene")
      #shinyjs::hide("main_plots")
    }
  })

  observeEvent(selectedVgroup(), {
    if(isTruthy(selectedVgroup())) {
      shinyjs::show("next_Vgroup")
     # shinyjs::show("main_plots")
    }
    else {
      shinyjs::hide("next_Vgroup")
     # shinyjs::hide("main_plots")
    }
  })
  
  ### next V gene button ----
  observeEvent(input$next_Vgene, {
    current_index <- which(allVgenes() == selectedV())
    shinyWidgets::updateVirtualSelect(
      inputId="vcall_selector", selected = allVgenes()[current_index+1] 
    )
  })
  
  ### next V group button ----
  observeEvent(input$next_Vgroup, {
    current_index <- which(allVgroups() == selectedVgroup())
    shinyWidgets::updateVirtualSelect(
      inputId="vgroup_selector", selected = allVgroups()[current_index+1] 
    )
  })

  ## Reactive data for plots ----
  
  ### aa lengths ----
  aa_lengths <- reactive({
    
    req(ds1(), ds2())
    req(validSelection())
    
    dataset1_aa <- do.call(ds1()$get_aa_lengths, chosenVlist()) |>
      dplyr::select(CDR3_LENGTH) |>
      tibble::add_column(dataset = ds1()$name)
    
    do.call(ds2()$get_aa_lengths, chosenVlist()) |>
      dplyr::select(CDR3_LENGTH) |>
      tibble::add_column(dataset = ds2()$name) |>
      dplyr::bind_rows(dataset1_aa)
    
    
    # dataset1_aa <- ds1()$get_aa_lengths(selectedV()) |>
    #   dplyr::select(n_aa) |>
    #   tibble::add_column(dataset = ds1()$name)
    
    # ds2()$get_aa_lengths(selectedV()) |>
    #   dplyr::select(n_aa) |>
    #   tibble::add_column(dataset = ds2()$name) |>
    #   dplyr::bind_rows(dataset1_aa)
  })
  
  ### np lengths ----
  #### np1&2 lengths for dataset 1 ----
  np_lengths_ds1 <- reactive({
    req(ds1())
    req(validSelection())
    
    do.call(ds1()$get_np_lengths, chosenVlist())
  })
  
  np_lengths_ds2 <- reactive({
    req(ds2())
    req(validSelection())
    
    do.call(ds2()$get_np_lengths, chosenVlist())
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
  
  ## V selection ----
  ## # this might be better with observeEvent and reactive Values bu I'm not sure 
  ## that it matters too much.
  chosenVlist <- reactive({

    if(vtype() == "vgroup"){
      if(isTruthy(selectedVgroup())){
        vlist <- list(vgroup = selectedVgroup(), v_call = NULL)
      }
    } else if(vtype() == "vgene"){
      if(isTruthy(selectedV())){
        vlist <- list(vgroup = NULL, v_call = selectedV())
      }
    } else{
      vlist <- list(vgroup = NULL, v_call = NULL)
    }
    if(input$drf_selector != "All") {
      vlist[["drf"]] = input$drf_selector
    } else vlist[["drf"]] = NULL
    
    if(input$cdr3_selector == "Custom"){
      vlist[["CDR3_length"]] = input$cdr3_length
    } else vlist[["CDR3_length"]] = NULL
   
    vlist
     
  })
  
  #chosenV <- reactive(Filter(Negate(is.null), chosenVlist())[[1]])
  
  ## filter annotations ----
  
  v_text <- reactive({
    v <- "all V genes"
    if (! is.null(chosenVlist()$v_call)) {
      v <- chosenVlist()$v_call
    } else if (! is.null(chosenVlist()$vgroup)) {
      v <- chosenVlist()$vgroup
    }
    v  
  })
  
  drf_text <- reactive({
    dplyr::if_else(
      is.null(chosenVlist()$drf), 
      "all D reading frames",
      paste("D reading frame", chosenVlist()$drf)
    )
  })
  
  cdr3_text <- reactive({
    dplyr::if_else(
      is.null(chosenVlist()$CDR3_length), 
      "all CDR3 lengths",
      paste("CDR3 lengths of ", chosenVlist()$CDR3_length)
    )
  })
  
  filter_text <- reactive(paste(v_text(), "for", drf_text(), "and", cdr3_text()))
  
  ### Jcalls ----  
  # keep these separate as one dataset may change while the other stays the same.
  Jcalls1 <- reactive({
    
    req(ds1())
    req(validSelection())
    
    do.call(ds1()$get_Jcalls, chosenVlist()) %>%
      dplyr::add_count(J_CALL) %>%
      mutate(percent_ds = (n/ds_Jtotal)*100) %>%
      select(J_CALL, n, percent_ds) %>%
      distinct()
  })
  
  Jcalls2 <- reactive({
    
    req(ds2())
    req(validSelection())
    
    do.call(ds2()$get_Jcalls, chosenVlist()) %>%
      dplyr::add_count(J_CALL) %>%
      mutate(percent_ds = (n/ds_Jtotal)*100) %>%
      select(J_CALL, n, percent_ds) %>%
      distinct()
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
    req(validSelection())
    
    do.call(ds1()$get_Dcalls, chosenVlist()) %>%
      dplyr::add_count(D_CALL) %>%
      mutate(percent_ds = (n/ds_Dtotal)*100) %>%
      select(D_CALL, n, percent_ds) %>%
      distinct()
  })
  
  Dcalls2 <- reactive({
    req(ds2())
    req(validSelection())
    
    do.call(ds2()$get_Dcalls, chosenVlist()) %>%
      dplyr::add_count(D_CALL) %>%
      mutate(percent_ds = (n/ds_Dtotal)*100) %>%
      select(D_CALL, n, percent_ds) %>%
      distinct()
    #ds2()$get_Dcalls(selectedV())
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
  
  aa1 <- reactive({
    req(validSelection())
    do.call(ds1()$get_aa_left , chosenVlist()) 
      
  })
  
  aa2 <- reactive({
    req(validSelection())
    do.call(ds2()$get_aa_left, chosenVlist())
  })
  
  # need to work out what we actually want to show here
  
  ### AA positions ----
  #### all joined data ----
  # joined <- reactive({
  #   dplyr::full_join(ds1()$aa_counts_left, ds2()$aa_counts_left, by = c("V_CALL", "pos", "value")) |>
  #   tidy_aa_joined_data()
  # }) |>
  #   bindCache(input$dataset1_selector, input$dataset2_selector) |>
  #   bindEvent(input$load_datasets)
  
 
  #### filtered by VCALL ----
  #aa_joined_data <- reactive(dplyr::filter(joined(), V_CALL==input$vcall_selector))
  
  ## Plots ----
  
  ### AA lengths ----
  mod_densityplotServer("aa_length_plot", ds=aa_lengths, feature="CDR3_LENGTH", feature_formatted="CDR3 lengths", filter_text=filter_text,  colour_palette=colour_palette)
  
  ### Dcalls ----
  mod_barplotServer("Dbarplot", ds=Dcalls, feature="D_CALL", feature_formatted="D call", filter_text=filter_text, colour_palette=colour_palette)
  
  ### Jcalls ----
  mod_barplotServer("Jbarplot", ds=Jcalls, feature="J_CALL", feature_formatted="J call", filter_text=filter_text, colour_palette=colour_palette)
  
  ### NP1 lengths ----
  mod_densityplotServer("np1plot", ds=np1_lengths, feature="NP1_LENGTH", feature_formatted="NP1 lengths", filter_text=filter_text, colour_palette=colour_palette)
  
  ### NP2 lengths ----
  mod_densityplotServer("np2plot", ds=np2_lengths, feature="NP2_LENGTH",  feature_formatted="NP2 lengths", filter_text=filter_text, colour_palette=colour_palette)
  
  ### AA as letters  ----
 # mod_letterplotServer("AA_plot", ds=aa_joined_data, raw_colours=extra_hex_cols, ds1_name=reactive(ds1()$name), ds2_name=reactive(ds2()$name))
}  


# Run the application 
shinyApp(ui = ui, server = server)

