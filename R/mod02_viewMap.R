#' viewMap UI Function
#' @description  This module will show searched species on map and show its occurrences
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import leaflet shinycssloaders
#' @noRd

mod02_viewMap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "View Map",
      width = 12,
      solidHeader = TRUE,
      status = "success",
      leaflet::leafletOutput(ns("map")) %>%
        shinycssloaders::withSpinner(type = 8, size = 0.5, color = "#999999")
    )
  )
}

#' viewMap Server Functions
#' @import memoise htmlwidgets
mod02_viewMap_server <- function(id, inputList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # added req so that null plots are not generated
    req(inputList$searchByVerOrSciName)
    req(inputList$selectedBySciOrVerName)

    # select from bddata which contains species name given by user
    bdDataFilteredBySpecies <- shiny::reactive(SelectedbdData(
      radioBtn_search_byName = inputList$radioBtn_searchByName,
      radioBtn_search_byNameID = "Vernacular Name",
      searchByVerOrSciName = inputList$searchByVerOrSciName,
      selectedByVerOrSciName = inputList$selectedBySciOrVerName
    )) %>% bindCache(inputList$searchByVerOrSciName, inputList$selectedBySciOrVerName)

    # added cache results to the function leafletPlot_m for performance improvement

    leafletPlot_m <- memoise::memoise(leafletPlot, cache = session$cache)

    output$map <- leaflet::renderLeaflet({
      if (!is.null(bdDataFilteredBySpecies()) && bdDataFilteredBySpecies() %>% nrow() != 0) {
        leafletPlot_m(bdDataFilteredBySpecies())
      }
    }) %>%
      shiny::bindCache(inputList$searchByVerOrSciName, inputList$selectedBySciOrVerName)
  })
}
