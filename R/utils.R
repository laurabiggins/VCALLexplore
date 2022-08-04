box_wrapper <- function(box_id, box_title, panel_tags, box_width = 6, collapsible = TRUE, collapsed=TRUE) {
  shinydashboard::box(
    id = box_id,
    title = box_title,
    width = box_width, 
    class = "plotbox",
    collapsible = collapsible,
    collapsed = collapsed,
    panel_tags)
}

hex_colours <- c("#f9c74f", "#43aa8b", "#f3722c", "#577590", "#f94144", "#90be6d", "#f8961e", "#4a4e69") 
extra_hex_cols <- c(hex_colours, paste0(hex_colours, "a3"), paste0(hex_colours, "5e"))

colour_palette <- ggplot2::scale_fill_manual(values = extra_hex_cols)

# this is for all the plots that require a V gene to be selected
hidePlots <- function(){
  #shinyjs::hide("Jbarplotbox")
  #shinyjs::hide("Dbarplotbox")
  shinyjs::hide("AAplotbox")
  shinyjs::hide("np1plotbox")
  shinyjs::hide("np2plotbox")
}

showPlots <- function(){
  #shinyjs::show("Jbarplotbox")
  #shinyjs::show("Dbarplotbox")
  shinyjs::show("np1plotbox")
  shinyjs::show("np2plotbox")
  shinyjs::show("AAplotbox")
}