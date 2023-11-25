#' The application User-Interface
#'
#' @import shiny
# Suppressing unused warnings
options(warn = -1)

options(shiny.autoload.r = FALSE)

# Load all files and folders from Biodiversity Dashboard to run the app

pkgload::load_all()

app_ui <- function(request) {
  shiny::tagList(
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
  # initiates All inputs values to NULL
  input_list <- shiny::reactiveValues(
    radioBtn_searchByName = NULL, searchByVerOrSciName = NULL,
    # this will returns the searched input values as Species Name by
    # Vernacular or Scientific based on  seach by species radio button choices

    selectedBySciOrVerName = NULL
    # this will returns the  selectedinput values as Species Name by
    # Vernacular or Scientific based on  seach by species radio button choices
  )

  # create the inputs and store its inputs id for next modules
  mod01_loadInputs_server("inputs", input_list)
  shiny::observe({
    # added module to map the occurences using leaflet library
    mod02_viewMap_server("map", input_list)
    # added third module to plot occurrences per month
    mod03_timelineVisualization_server("timeline_plot", input_list,
      flg_darkMode = shiny::reactive(input$dark_mode)
    )
  })
}
shiny::shinyApp(
  ui = app_ui,
  server = app_server,
)
