#' mod_timelineVisualization UI Function
#'
#' @description mod_timelineVisualization module will plot monthly occurences
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import echarts4r shinycssloaders
#' @importFrom shiny NS tagList
mod_timelineVisualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Occurrences per Month",
      width = 12,
      solidHeader = TRUE,
      status = "success",
      echarts4r::echarts4rOutput(ns("occ_perMonth")) %>%
        shinycssloaders::withSpinner(type = 8, size = 0.5, color = "#999999")
    )
  )
}

#' mod_timelineVisualization Server Functions
#'
#' @noRd
mod_timelineVisualization_server <- function(id, searchSpeciesIds) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bs4Dash::useAutoColor()
    # added req so that null plots are not generated

    req(searchSpeciesIds$scientificNameId)
    req(searchSpeciesIds$vernacularNameId)

    # select from bddata which contains species given by user

    bdDataFilteredBySpecies <- shiny::reactive(SelectedbdData(
      searchedVerncularNameID = searchSpeciesIds$vernacularNameId,
      searchedScientificNameID = searchSpeciesIds$scientificNameId
    ))

    # added cache results to the function monthlyOccurence_m for performance improvement
    monthlyOccurence_m <- memoise::memoise(monthlyOccurence, cache = session$cache)
    # occurences per month
    occ_bdData <- reactive(
      monthlyOccurence_m(data = bdDataFilteredBySpecies())
    )

    # added cache results to the function barplot_m for performance improvement

    barplot_m <- memoise::memoise(barplot, cache = session$cache)
    output$occ_perMonth <- echarts4r::renderEcharts4r(
      if (bdDataFilteredBySpecies() %>% nrow() != 0) {
        # barchart for occurences per month

        barplot_m(
          data = occ_bdData(),
          xaxisLabel = "Months",
          yaxisLabel = "Occurences",
          title = ""
        )
      }
    ) %>%
      shiny::bindCache(searchSpeciesIds$vernacularNameId, searchSpeciesIds$scientificNameId)
  })
}
