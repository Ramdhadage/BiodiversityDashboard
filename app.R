#' The application User-Interface
#'
#' @import shiny
# Suppressing unused warnings
options(warn=-1)

options(shiny.autoload.r=FALSE)

# Load all files and folders from Biodiversity Dashboard to run the app

pkgload::load_all()

app_ui <- function(request) {
  tagList(
    # build the ui
    createPage(
      title = "Biodiversity Dashboard",
      subtitle = "",
      context = ""
    )
  )
}
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REAMOVE.
#' @import shiny
app_server <- function(input, output, session) {
  # Your application server logic

  # initiates searchSpeciesIds reactive list with null
  searchSpeciesIds <- reactiveValues(
    vernacularNameId = NULL,
    scientificNameId = NULL
  )
  # load the data, collect user input and  filter as per user input
  mod_loadData_server("loadData", searchSpeciesIds)

  shiny::observe({
    # added module to map the occurences using leaflet library
    mod_viewMap_server("map", searchSpeciesIds)
    # added thid module to plot occurences per month
    mod_timelineVisualization_server("timeline_plot", searchSpeciesIds)
  })
}
shiny::shinyApp(
  ui = app_ui,
  server = app_server,
)

