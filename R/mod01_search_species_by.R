#' Search Species By UI Function
#'
#' @description search_species_by is first module which creates radio button for species selection based on scientific names and vernacular names.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @examples
#' library(shiny)
#' library(BiodiversityDashboard)
#' ui <- fluidPage(
#'   mod01_search_species_by_ui("var")
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
mod01_search_species_by_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dqshiny::autocomplete_input(id = ns("scientific_name"),
                                label = "Scientific Name",
                                placeholder = "Search Scientific Name",
                                max_options = 10,create = TRUE, contains = TRUE,hide_values = FALSE,
                                options = unique(load_bdData()$scientificName)
    ),
    shiny::selectizeInput(inputId = ns("vernacular_name"),
                          label = "Vernacular Name",
                          choices = NULL
    ))
}

mod01_search_species_by_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      vernacular_name_choices <- shiny::reactive({
        shiny::req(input$scientific_name %in% unique(load_bdData()$scientificName))
        load_bdData() %>%
          dplyr::filter(scientificName == input$scientific_name) %>%
          dplyr::select(vernacularName) %>% dplyr::distinct() %>% dplyr::pull()
      })

      observeEvent(input$scientific_name,{
        if(input$scientific_name %in% unique(load_bdData()$scientificName)){
          shiny::updateSelectizeInput(session = session,
                                      inputId = "vernacular_name",
                                      choices = vernacular_name_choices() %>% dplyr::first(),
                                      selected = )
        }
      })
      reactive({
        list(
        scientific_name = input$scientific_name,
        vernacular_name = input$vernacular_name
      )
      })
    }
  )
}
