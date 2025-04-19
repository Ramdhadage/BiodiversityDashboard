
# Suppressing unused warnings
# options(warn=-1)

# options(shiny.autoload.r=FALSE)

# Load all files and folders from Biodiversity Dashboard to run the app

# pkgload::load_all()
#' The application User-Interface
#'
#' @import shiny
app_ui <- function(request) {
  tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
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
  inputList <- shiny::reactiveValues(
    radioBtn_searchByName = NULL,
    searchByVerOrSciName = NULL, # this will returns the searched input values as Species Name by Vernacular or Scientific based on  seach by species radio button choices
    selectedBySciOrVerName =  NULL # this will returns the  selectedinput values as Species Name by Vernacular or Scientific based on  seach by species radio button choices
  )

  # create the inputs and store its inputs id for next modules
  mod01_loadInputs_server("inputs",inputList)
  shiny::observe({
    # added module to map the occurences using leaflet library
    mod02_viewMap_server("map", inputList)
    # added third module to plot occurrences per month
    mod03_timelineVisualization_server("timeline_plot", inputList,
                                       flg_darkMode = reactive(input$dark_mode))
  })
}

#' Run Biodiversity Dashboard
#'
#' @importFrom shiny shinyApp
#'
#' @export

run_bd_app <- function(...) {
  shinyApp(
    ui = app_ui,
    server = app_server
    )

}


