#' Load Inputs UI Function
#'
#' @description
#' This module creates input controls for species selection. It displays a radio button
#' for choosing between scientific names and vernacular names, then based on the selection
#' creates a dynamic searchable dropdown for the corresponding species names.
#'
#' @param id A character string used to identify the namespace for the module
#'
#' @return A Shiny UI element
#'
#' @importFrom shiny NS tagList
mod01_loadInputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Occurences",
      width = 12,
      maximizable = TRUE,
      solidHeader = TRUE,
      status = "success",
      shinyWidgets::radioGroupButtons(
        inputId = ns("search_byName"),
        label = "Search Species by:",
        choices = c(
          "Vernacular Name",
          "Scientific Name"
        ),
        checkIcon = list(
          yes = icon("ok",lib = "glyphicon")
        ),
        justified = TRUE
      ),
      uiOutput(ns("mini_ui1")),
      uiOutput(ns("mini_ui2"))
    )
  )
}

#' Load Inputs Server Function
#'
#' @description
#' Server logic for the loadInputs module. Creates dynamic dropdowns based on user
#' selection of search method (vernacular or scientific name) and updates the parent
#' module's input list accordingly.
#'
#' @param id A character string matching the ID used in the UI function
#' @param inputList A reactive list that will be updated with user selections
#'
#' @return None (called for side effects)
#'
#' @import shiny
mod01_loadInputs_server <- function(id, inputList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a HTML Output Tags for searchable drop down for search species by Scientific or Vernacular name based on Search by species radio button.
    output$mini_ui1 <- shiny::renderUI({
      # Create a searchable drop down for search species by Scientific or Vernacular name based on Search by species radio button.
      dropdownBasedOnRadioBtn(
        radioBtn_search_byName = input$search_byName, radioBtn_search_byNameID = "Vernacular Name",
        id1 = ns("ver_name"), label1 = "Search Species by Vernacular Name: ", choices1 = unique(load_bdData()$vernacularName),
        id2 = ns("sci_name"), label2 = "Search Species by Scientific Name:", choices2 = unique(load_bdData()$scientificName)
      )
    })

    # Creates HTML Output tags for searchable drop down for selected species by Scientific or Vernacular name based on Search species by Scientific or Vernacular Name.
    output$mini_ui2 <- shiny::renderUI({
      # Create searchable drop down for selected species by Scientific or Vernacular name based on Search species by Scientific or Vernacular Name.
      dropdownBasedOnRadioBtn(
        radioBtn_search_byName = input$search_byName, radioBtn_search_byNameID = "Vernacular Name",
        id1 = ns("selected_sci_name"), label1 = "Matching Scientific Name of Searched Species: ",
        choices1 = NULL, id2 = ns("selected_ver_name"), label2 = "Matching Vernacular Name of Searched Species: ",
        choices2 = NULL
      )
    })

    observe({
      # Always update the radio button value, even if NULL
      inputList$radioBtn_searchByName <- input$search_byName

      # Update searchByVerOrSciName based on radio button selection
      if (identical(input$search_byName, "Vernacular Name")) {
        inputList$searchByVerOrSciName <- input$ver_name
      } else if (identical(input$search_byName, "Scientific Name")) {
        inputList$searchByVerOrSciName <- input$sci_name
      }

      # Update selectedBySciOrVerName based on radio button selection
      if (identical(input$search_byName, "Vernacular Name") && !is.null(input$selected_sci_name)) {
        inputList$selectedBySciOrVerName <- input$selected_sci_name
      } else if (identical(input$search_byName, "Scientific Name") && !is.null(input$selected_ver_name)) {
        inputList$selectedBySciOrVerName <- input$selected_ver_name
      }
    })
    # Added combined reactive values for adding in binding event
    bindEventExpression <- reactive(list(input$ver_name, input$search_byName, input$sci_name))

    observeEvent(bindEventExpression(), {
      updateDropdownBasedOnRadioBtnValues(
        radioBtn_search_byName = input$search_byName,
        radioBtn_search_byNameID_ver = "Vernacular Name", id_ver_name = input$ver_name,
        scientificName_str = "scientificName", vernacularName_str = "vernacularName", id_selected_sci_name = input$selected_sci_name,
        radioBtn_search_byNameID_sci = "Scientific Name", id_sci_name = input$sci_name,
        id_selected_ver_name = input$selected_ver_name, session = session
      )
    })

    # Also observe changes to the selected names and update inputList
    observeEvent(list(input$selected_sci_name, input$selected_ver_name), {
      if (identical(input$search_byName, "Vernacular Name") && !is.null(input$selected_sci_name)) {
        inputList$selectedBySciOrVerName <- input$selected_sci_name
      } else if (identical(input$search_byName, "Scientific Name") && !is.null(input$selected_ver_name)) {
        inputList$selectedBySciOrVerName <- input$selected_ver_name
      }
    })
  })
}
