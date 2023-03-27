
mod_barplotUI <- function(id){#}, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    wellPanel(
      class = "options", 
      style = "background: #112A46; color: white; padding: 10px",
      plotOutput(outputId = ns("barplot")),
      br(),
      fluidRow(
        column(
          width = 6, 
          radioButtons(ns("yaxis"), label=NULL, inline = TRUE, 
                       choices = c("count" = "count", 
                                   #"% V gene" = "percentV", 
                                   "% whole dataset" = "percent_ds"
                                   )
                       )
        ),
        column(
          width = 6, 
          radioButtons(ns("xaxis"), label=NULL, choices = c("within", "between"), inline = TRUE)
        )
      )
    ),
    br()#,
    #actionButton(ns("browser"), "browser")
  )
}

mod_barplotServer <- function(id, ds, feature, colour_palette, feature_formatted, filter_text) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    y_val <- reactive({
      switch(input$yaxis,
        count = "n", 
        #percentV = "percent_per_Vgene",
        percent_ds = "percent_ds"
      )
    })
    
    #y_val <- reactive(dplyr::if_else(input$yaxis == "count", "n", "percentage"))
    
    plot_title <- reactive({
      y_info <- switch(y_val(), 
                       n = "Counts for each", 
                       percent_ds = paste(feature_formatted, "count/Total no of that call in entire dataset"))
        
      text <- paste(y_info, feature_formatted, "for", filter_text())
      
    })
    
    barplot_base <- reactive({

     if(input$xaxis == "between"){
       p <-  ggplot(ds(), aes(x = .data[[feature]], y=.data[[y_val()]], fill = dataset)) +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust=1))
         
     } else {
       p <- ggplot(ds(), aes(x = dataset, y = .data[[y_val()]], fill = .data[[feature]]))+
         theme_minimal() +
         theme(axis.text.x = element_text(size = 14))
     }
      
     p + 
       geom_col(colour = "black", position = position_dodge2()) +
       xlab("") +
       colour_palette +
       ggtitle(plot_title())
    
    })

    output$barplot <- renderPlot(barplot_base())
    
  })
}
