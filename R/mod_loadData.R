#' loadData UI Function
#'
#' @description mod_loadData will load the give user to choose/search species by there scientific names or vernacular name
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
mod_loadData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Occurences",
      width = 12,
      maximizable = TRUE,
      solidHeader = TRUE,
      status = "success",
      # added null select list control to update in server side for better performance
      selectSpecies_ui(
        id = ns("ver_name"),
        label = "Search Species by Vernacular Name: ",
        choices = NULL
      ),
      # add null select input which is depend on vernacular species name and will give high performance.

      selectSpecies_ui(
        id = ns("sci_name"),
        label = "Search Species by Scientific Name: ",
        choices = NULL
      )
    )
  )
}

#' loadData Server Functions
#' @param searchSpeciesIds  is a reactive list which contains Scientific Names and Vernacular Name of species selected by user

mod_loadData_server <- function(id, searchSpeciesIds = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # added cache results to the function updateSpecies for performance improvement
    updateSpecies_m <- memoise::memoise(updateSpecies, cache = session$cache)
    # update choices for vernacular name
    updateSpecies_m(
      session = session,
      id = "ver_name",
      choices = unique(load_bdData()$vernacularName)
    )
    shiny::observe({
      if (input$ver_name != "") {

        # updating vernacular species Name id list element  by vernacular name id, storing it for using in another modules

        searchSpeciesIds$vernacularNameId <- input$ver_name

        # added cache results to the function SpeciesbyScientificName_m for performance improvement

        SpeciesbyScientificName_m <- memoise::memoise(SpeciesbyScientificName, cache = session$cache)

        # Choices from scientific name species, distinct() select unique values and pull() convert data.frame to vector

        SpeciesbyScientificName_m <- SpeciesbyScientificName_m(input$ver_name)

        # update Scientific Species name bassed on vernacular species  name

        updateSpecies(
          session = session,
          id = "sci_name",
          choices = SpeciesbyScientificName_m
        )
      }
    }) %>% shiny::bindEvent(input$ver_name)
    #  updating searchSpeciesIds$scientificNameId with scientific name id
    observe({
      # updating scientific species Name id by scientific name id, storing it for using in another modules
      if (input$sci_name != "") {
        searchSpeciesIds$scientificNameId <- input$sci_name
      }
    }) %>% shiny::bindEvent(input$sci_name)
  })
}
