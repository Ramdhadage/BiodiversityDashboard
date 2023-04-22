#' loadInputs UI Function
#'
#' @description mod01_loadInputs is first module which creates radio button for species selection based on scientific names and vernacular names, then based on user choice create dynamic searchable dropbox for species scienific and vernacular name.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
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

#' dynamicUI Server Functions
#'
#' @import shiny
mod01_loadInputs_server <- function(id, inputList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a HTML OUtput Tags for searchable drop down for search species by Scientific or Vernacular name based on Search by species radio button.
    output$mini_ui1 <- shiny::renderUI({
      # Create a searchable drop down for search species by Scientific or Vernacular name based on Search by species radio button.
      dropdownBasedOnRadioBtn(
        radioBtn_search_byName = input$search_byName, radioBtn_search_byNameID = "Vernacular Name",
        id1 = ns("ver_name"), label1 = "Search Species by Vernacular Name: ", choices1 = unique(load_bdData()$vernacularName),
        id2 = ns("sci_name"), label2 = "Search Species by Scientific Name:", choices2 = unique(load_bdData()$scientificName)
      )
    })

    # Creates HTML Output tags for searchable drop down for selected species by Scientific or Vernacular name based  on Search species by Scientific or Vernacular Name.
    output$mini_ui2 <- shiny::renderUI({
      # Create searchable drop down for selected species by Scientific or Vernacular name based  on Search species by Scientific or Vernacular Name.
      dropdownBasedOnRadioBtn(
        radioBtn_search_byName = input$search_byName, radioBtn_search_byNameID = "Vernacular Name",
        id1 = ns("selected_sci_name"), label1 = "Matching Scientific Name of Searched Species: ",
        choices1 = NULL, id2 = ns("selected_ver_name"), label2 = "Matching Vernacular Name of Searched Species: ",
        choices2 = NULL
      )
    })
    # Added combined reactive values for adding in binding event
    bindEventExpression <- shiny::reactive(list(input$ver_name, input$search_byName, input$sci_name))
    shiny::observe({
      updateDropdownBasedOnRadioBtnValues(
        radioBtn_search_byName = input$search_byName,
        radioBtn_search_byNameID_ver = "Vernacular Name", id_ver_name = input$ver_name,
        scientificName_str = "scientificName", vernacularName_str = "vernacularName", id_selected_sci_name = input$selected_sci_name,
        radioBtn_search_byNameID_sci = "Scientific Name", id_sci_name = input$sci_name,
        id_selected_ver_name = input$selected_ver_name, session = session
      )

      # Updating InptList with updated inputvalues as species name and Scientific and Vernacular Name
      inputList$radioBtn_searchByName <- input$search_byName
      req(input$ver_name, input$sci_name) # added req to resolve the issue if_else: `true` must be a vector, not `NULL`
      inputList$searchByVerOrSciName <- if_else(input$search_byName == "Vernacular Name",
        input$ver_name, input$sci_name
      )
      inputList$selectedBySciOrVerName <- if_else(input$search_byName == "Vernacular Name" & !is.null(input$selected_sci_name),
        input$selected_sci_name, input$selected_ver_name
      )
    }) %>% shiny::bindEvent(bindEventExpression())
    # update the input values of search species by Scientific or Vernacular Name into inputList.
    bindEventExpression_selectedSpecies <- reactive(list(input$selected_sci_name, input$selected_ver_name))
    shiny::observeEvent(bindEventExpression_selectedSpecies(), {
      req(input$ver_name, input$sci_name) # added req to resolve the issue if_else: `true` must be a vector, not `NULL`
      inputList$searchByVerOrSciName <- if_else(input$search_byName == "Vernacular Name",
        input$ver_name, input$sci_name
      )
      inputList$selectedBySciOrVerName <- if_else(input$search_byName == "Vernacular Name" & !is.null(input$selected_sci_name),
        input$selected_sci_name, input$selected_ver_name
      )
      inputList$radioBtn_searchByName <- input$search_byName
    })
  })
}
