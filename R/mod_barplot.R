
mod_barplotUI <- function(id){#}, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
       width = 3,
       class = "options",
       radioButtons(ns("yaxis"), label=NULL, choices = c("count", "percentage")),
       br(),
       radioButtons(ns("xaxis"), label=NULL, choices = c("within", "between")),
       actionButton(ns("browser"), "browser")
      ),
      mainPanel = mainPanel(plotOutput(outputId = ns("barplot")), width = 9),
      position = "right"
    )
  )
}

mod_barplotServer <- function(id, ds, feature, colour_palette) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    y_val <- reactive(dplyr::if_else(input$yaxis == "count", "n", "percentage"))
    
    barplot_base <- reactive({

     if(input$xaxis == "between"){
       p <-  ggplot(ds(), aes(x = .data[[feature]], y=.data[[y_val()]], fill = dataset)) +
         theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust=1))
         
     } else {
       p <- ggplot(ds(), aes(x = dataset, y = .data[[y_val()]], fill = .data[[feature]]))
     }
      
     p + 
       geom_col(colour = "black", position = position_dodge2()) +
       xlab("") +
       colour_palette
    
    })
    
    output$barplot <- renderPlot(barplot_base())
    
  })
}
