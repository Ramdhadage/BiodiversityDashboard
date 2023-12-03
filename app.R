library(shiny)
devtools::load_all()
ui <- bslib::page_sidebar(
  title = "Biodiversity Dashboard",
  sidebar = bslib::sidebar(
    mod01_search_species_by_ui("input")
  ),
  bslib::card(
    full_screen = TRUE,
    mod02_viewMap_ui("map")
  )

)

server <- function(input, output, session) {

  input_list <- mod01_search_species_by_Server("input")
  mod02_viewMap_server("map", input_list)

}

shinyApp(ui, server)
