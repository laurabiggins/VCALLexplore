mod_letterplotUI <- function(id){#}, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    wellPanel(
      class = "options", 
      style = "background: #112A46; color: white; padding: 10px",
      plotlyOutput(outputId = ns("letterplot")),
      br(),
      textOutput(ns("plot_description")),
      br(),
      fluidRow(
        column(
          width = 6, 
          radioButtons(ns("yaxis"), label=NULL, choices = c("within selected V" = "within_v", "whole dataset" = "whole_dataset"), inline = TRUE)
        ),
        column(
          width = 2, offset = 4,
          checkboxInput(ns("show_data"), label = "Show data")
        )
      ),
      conditionalPanel(
        style = "background: white; color: #112A46; padding: 10px",
        #condition = "input.show_data == 1",
        condition = paste0("input['", ns("show_data"), "']"),
        DT::dataTableOutput(ns("aa_data")),
        downloadButton(ns("download_data"), "Download data")
      )
    ),
    ##br(),
    actionButton(ns("browser"), "browser")
  )
}

mod_letterplotServer <- function(id, ds, raw_colours, ds1_name, ds2_name) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$show_data, print("showing data"))
    
    observeEvent(input$browser, browser())
    
    y_type <- reactive({
      
      if(input$yaxis == "within_v"){
        return("diff_withinV")
      } else if(input$yaxis == "whole_dataset"){
        return("diff_dataset")
      } else {
        print("Oops, something went wrong with the y axis selection on the letter plot")
      }
    })

    ds1_text <- reactive(paste("Proportion of amino acid at specified position in", ds1_name()))
    ds2_text <- reactive(paste("proportion of amino acid at specified position in", ds2_name()))
    
    description <- reactive({
      switch(y_type(), 
             diff_withinV = paste0(ds1_text(), " - ", ds2_text(), " for selected V gene(s)"),
             #fold_change_vcall = paste0(ds1_text(), "/",  ds2_text(), " for selected V gene(s)"),
             #fold_change_ds = paste0(ds1_text(), "/",  ds2_text(), " over whole dataset."), 
             diff_dataset = paste0(ds1_text(), " - ", ds2_text(), " over whole dataset.")
             )
    })
    
   # output$plot_description <- renderText(description())
    
    output$aa_data <- DT::renderDataTable(ds())
    
    output$letterplot <- renderPlotly({
      
      ds() |>
        plotly::plot_ly(x= ~pos, y= ~get(y_type()), color= ~value, colors = raw_colours) |>
        #plotly::plot_ly(x= ~pos, y= ~pos_aa_count, color= ~value, colors = raw_colours) |>
        plotly::add_text(
          text = ~value,
          #hovertext = ~name,
          #hoverinfo = "text",
          size = I(20)
        ) %>%
        layout(
          title = list(
            text = "Difference in amino acids frequencies along selected V call(s) between the 2 datasets",
            #text = "Temporarily just showing counts for dataset 1",
            pad = list(t = 100, b = 200), yanchor = "top"
          ),
          yaxis = list(title = description()),
          xaxis = list(title="position")
        )
    })
    
    output$download_data <- downloadHandler(
      
      filename = function() ("AA_plot_data.csv"),
      content = function(file){
        readr::write_csv(file=file, ds())
      }
    )
    
  })
        
}