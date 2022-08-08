mod_letterplotUI <- function(id){#}, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    wellPanel(
      class = "options", 
      style = "background: #112A46; color: white;",
      plotlyOutput(outputId = ns("letterplot")),
      br(),
      textOutput(ns("plot_description")),
      br(),
      fluidRow(
        column(
          width = 6, 
          radioButtons(ns("yaxis1"), label=NULL, inline = TRUE, choices = c("percent difference" = "percent_diff", "fold change" = "fc"))
        ),
        column(
          width = 6, 
          radioButtons(ns("yaxis2"), label=NULL, choices = c("within VCall" = "within_vcall", "whole dataset" = "whole_dataset"), inline = TRUE)
        )
      )
    ),
    ##br(),
    actionButton(ns("browser"), "browser")
  )
}

mod_letterplotServer <- function(id, ds, raw_colours, selected_V, ds1_name, ds2_name) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    y_type <- reactive({
      
      if(input$yaxis1 == "percent_diff" & input$yaxis2 == "within_vcall"){
        return("percent_diff_vcall")
      } else if(input$yaxis1 == "fc" & input$yaxis2 == "within_vcall"){
        return("fold_change_vcall")
      } else if(input$yaxis1 == "fc" & input$yaxis2 == "whole_dataset"){
        return("fold_change_ds")
      } else if (input$yaxis1 == "percent_diff" & input$yaxis2 == "whole_dataset"){
        return("percent_diff_ds")
      } else {
        print("Oops, something went wrong with the y axis selection on the letter plot")
      }
    })

    ds1_text <- reactive(paste("Proportion of amino acid at specified position in", ds1_name()))
    ds2_text <- reactive(paste("proportion of amino acid at specified position in", ds2_name()))
    
    description <- reactive({
      switch(y_type(), 
             percent_diff_vcall = paste0(ds1_text(), " - ", ds2_text(), " for ", selected_V()),
             fold_change_vcall = paste0(ds1_text(), "/",  ds2_text(), " for ", selected_V()),
             fold_change_ds = paste0(ds1_text(), "/",  ds2_text(), " over whole dataset."), 
             percent_diff_ds = paste0(ds1_text(), " - ", ds2_text(), " over whole dataset.")
             )
    })
    
    output$plot_description <- renderText(description())
    
    output$letterplot <- renderPlotly({
      
      ds() |>
        plotly::plot_ly(x= ~pos, y= ~get(y_type()), color= ~value, colors = raw_colours) |>
        plotly::add_text(
          text = ~value,
          #hovertext = ~name,
          #hoverinfo = "text",
          size = I(20)
        ) %>%
        layout(
          title = list(
            text = paste("Difference in amino acids frequencies along", selected_V(), "between the 2 datasets"), 
            pad = list(t = 100, b = 200), yanchor = "top"
          ), 
          yaxis = list(title = y_type()), 
          xaxis = list(title="position")
        )
    })
  })
        
}