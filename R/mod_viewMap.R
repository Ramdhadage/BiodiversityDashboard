#' viewMap UI Function
#' @description  This module will show searched species on map and show its occurrences
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import leaflet shinycssloaders
#' @noRd

mod_viewMap_ui <- function(id) {
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

mod_viewMap_server <- function(id, searchSpeciesIds) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # added req so that null plots are not generated
    req(searchSpeciesIds$scientificNameId)
    req(searchSpeciesIds$vernacularNameId)
    # select from bddata which contains species name given by user

    bdDataFilteredBySpecies <- shiny::reactive(SelectedbdData(
      searchedVerncularNameID = searchSpeciesIds$vernacularNameId,
      searchedScientificNameID = searchSpeciesIds$scientificNameId
    )) %>% bindCache(searchSpeciesIds$vernacularNameId, searchSpeciesIds$scientificNameId)

    # added cache results to the function leafletPlot_m for performance improvement

    leafletPlot_m <- memoise::memoise(leafletPlot, cache = session$cache)

    output$map <- leaflet::renderLeaflet({
      if (bdDataFilteredBySpecies() %>% nrow() != 0) {
        leafletPlot_m(bdDataFilteredBySpecies())
      }
    }) %>%
      shiny::bindCache(searchSpeciesIds$vernacularNameId, searchSpeciesIds$scientificNameId)
  })
}
