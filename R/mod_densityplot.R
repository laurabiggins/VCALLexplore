
mod_densityplotUI <- function(id, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    wellPanel(
      class = "options", 
      style = "background: #112A46; color: white;",
      plotOutput(outputId = ns("densityplot"), height = plot_height)
    )
    #actionButton(ns("browser"), "browser")
  )
}

mod_densityplotServer <- function(id, ds, feature, colour_palette) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    density_obj <- reactive({
      
      ggplot(ds(), aes(.data[[feature]], fill = dataset)) +
        geom_density(adjust = 3, alpha =0.8, colour = "black", size=1) +
        colour_palette +
        theme_minimal()
    })
    
    output$densityplot <- renderPlot(density_obj())
    
  })
}
