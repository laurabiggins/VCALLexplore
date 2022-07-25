
mod_barplotUI <- function(id){#}, plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
       width = 3,
       class = "options",
       radioButtons(ns("yaxis"), label=NULL, choices = c("count", "percentage")),
       radioButtons(ns("xaxis"), label=NULL, choices = c("within", "between")),
       actionButton(ns("browser"), "browser")
      ),
      mainPanel = mainPanel(plotOutput(outputId = ns("barplot"))),
      position = "right"
    )
  )
}

mod_barplotServer <- function(id, ds, feature) {
  moduleServer(id, function(input, output, session) {
    
    set.seed(1)
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    barplot_base <- reactive({
      
      p <-  ds() %>%
        ggplot(aes(x = .data[[feature]], y=count, fill = dataset)) +
        geom_col(colour = "black", position = position_dodge2())#fill="#3C6997", color="#F57200")
      
      p + xlab("")
      
    })
    
    
    output$barplot <- renderPlot({
      barplot_base()
      #plot(1:10)
    })
    
  })
}
