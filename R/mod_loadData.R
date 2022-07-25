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
      radioGroupButtons(
        inputId = ns("search_byName"),
        label = "Search Species by:",
        choices = c(
          "Vernacular Name",
          "Scientific Name"
        ),
        checkIcon = list(
          yes = icon("ok",
            lib = "glyphicon"
          )
        ),
        justified = TRUE
      ),
      uiOutput(ns("mini_ui1")),
      uiOutput(ns("mini_ui2"))
    )
  )
}

#' loadData Server Functions
#' @param searchSpeciesIds  is a reactive list which contains Scientific Names and Vernacular Name of species selected by user

mod_loadData_server <- function(id, searchSpeciesIds = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$mini_ui1 <- shiny::renderUI({
      if (input$search_byName == "Vernacular Name") {
        selectSpecies_ui(
          id = ns("ver_name"),
          label = "Search Species by Vernacular Name: ",
          choices = unique(load_bdData()$vernacularName)
        )
      } else {
        # added null select list control to update in server side for better performance
        selectSpecies_ui(
          id = ns("sci_name"),
          label = "Search Species by Scientific Name:",
          choices = unique(load_bdData()$scientificName)
        )
      }
    })

    output$mini_ui2 <- shiny::renderUI({

      # added null select list control to update in server side for better performance

      if (input$search_byName == "Vernacular Name") {
        # added null select list control to update in server side for better performance
        selectSpecies_ui(
          id = ns("selected_sci_name"),
          label = "Scientific Name of Searched Species:",
          choices = NULL
        )
      } else {
        # added null select list control to update in server side for better performance
        selectSpecies_ui(
          id = ns("selected_ver_name"),
          label = "Vernacular Name of Searched Species: ",
          choices = NULL
        )
      }
    })

    # added cache results to the function updateSpecies for performance improvement

    updateSpecies_m <- memoise::memoise(updateSpecies, cache = session$cache)

    # added cache results to the function SpeciesbyScientificName_m for performance improvement

    DistinctChoices_m <- memoise::memoise(DistinctChoices, cache = session$cache)

    # Added combined reactive values for addining in binding event
    bindEventExpression <- reactive(list(input$ver_name, input$search_byName, input$sci_name))
    shiny::observe({
      if (input$search_byName == "Vernacular Name" & !is.null(input$ver_name)) {
        # if (!is.null(input$ver_name)){


        # updating vernacular species Name id list element  by vernacular name id, storing it for using in another modules

        searchSpeciesIds$vernacularNameId <- input$ver_name

        # Choices from scientific name species, distinct() select unique values and pull() convert data.frame to vector

        SpeciesbyScientificName <- DistinctChoices_m(
          filteredNameID = input$ver_name,
          selectedColumnName = "scientificName",
          searchColumnName = "vernacularName"
        )

        # update Scientific Species name based on vernacular species  name

        updateSpecies_m(
          session = session,
          id = "selected_sci_name",
          choices = SpeciesbyScientificName
        )
      } else if (input$search_byName == "Scientific Name" & !is.null(input$sci_name)) {

        # updating vernacular species Name id list element  by vernacular name id, storing it for using in another modules

        searchSpeciesIds$scientificNameId <- input$sci_name

        # Choices from scientific name species, distinct() select unique values and pull() convert data.frame to vector

        SpeciesbyvernacularName <- DistinctChoices_m(
          filteredNameID = input$sci_name,
          selectedColumnName = "vernacularName",
          searchColumnName = "scientificName"
        )

        # update Scientific Species name based on vernacular species  name

        updateSpecies_m(
          session = session,
          id = "selected_ver_name",
          choices = SpeciesbyvernacularName
        )
      }
    }) %>% shiny::bindEvent(bindEventExpression())

    bindEventExpression_selectedSpecies <- reactive(list(input$selected_sci_name, input$selected_ver_name))
    #  updating searchSpeciesIds$scientificNameId with scientific name id
    observe({
      if (input$search_byName == "Vernacular Name" & !is.null(input$selected_sci_name)) {

        # updating scientific species Name id by scientific name id, storing it for using in another modules

        searchSpeciesIds$scientificNameId <- input$selected_sci_name
      } else if (input$search_byName == "Scientific Name" & !is.null(input$selected_ver_name)) {

        # updating scientific species Name id by scientific name id, storing it for using in another modules

        searchSpeciesIds$vernacularNameId <- input$selected_ver_name
      }
    }) %>% shiny::bindEvent(bindEventExpression_selectedSpecies())
  })
}
