
# Suppressing unused warnings
# options(warn=-1)

# options(shiny.autoload.r=FALSE)

# Load all files and folders from Biodiversity Dashboard to run the app

# pkgload::load_all()
#' Create the Shiny application user interface
#'
#' This function defines the complete user interface (UI) for the biodiversity dashboard application.
#' It includes setup for the page layout, title, and any external dependencies.
#'
#' @param request The request object, typically passed from the Shiny framework when
#'   the application is initialized. This enables bookmarking and other server-side
#'   features that depend on the request state.
#'
#' @return A tagList containing the complete UI definition for the Shiny application.
#'
#' @details
#' The UI includes:
#' - External JavaScript for timing operations from the shiny.tictoc package
#' - The main application page structure defined by the createPage function
#'
#' @examples
#' # This function is typically called by shinyApp:
#' # shinyApp(ui = app_ui, server = app_server)
#'
#' @import shiny
#' @importFrom htmltools tagList tags
app_ui <- function(request) {
  tagList(
    # Include external JavaScript for timing operations
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    # Build the main UI page
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


