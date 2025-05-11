#' View Map UI Function
#'
#' @description This module will show searched species on map and display its occurrences
#'
#' @param id A character string that defines the namespace for the module
#'
#' @import leaflet shinycssloaders
#'
#' @return A UI definition that can be passed to the [shinyUI()] function
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

#' View Map Server Function
#'
#' @description Server logic for the view map module
#'
#' @param id A character string that defines the namespace for the module
#' @param inputList A reactive list containing user inputs:
#'   \itemize{
#'     \item{searchByVerOrSciName}{Search term for vernacular or scientific name}
#'     \item{selectedBySciOrVerName}{Selected species from search results}
#'     \item{radioBtn_searchByName}{Radio button selection for search type}
#'   }
#'
#' @import memoise htmlwidgets leaflet shiny
#' @importFrom shiny moduleServer reactive req bindCache
#'
#' @return A [moduleServer()] function
#' @noRd
mod02_viewMap_server <- function(id, inputList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # added req so that null plots are not generated
    req(inputList$searchByVerOrSciName)
    req(inputList$selectedBySciOrVerName)

    # select from bddata which contains species name given by user
    bdDataFilteredBySpecies <- shiny::reactive(selected_bd_data(
      radio_btn_search_by_name = inputList$radioBtn_searchByName,
      radio_btn_search_by_name_id = "Vernacular Name",
      search_by_ver_or_sci_name = inputList$searchByVerOrSciName,
      selected_by_ver_or_sci_name = inputList$selectedBySciOrVerName
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
