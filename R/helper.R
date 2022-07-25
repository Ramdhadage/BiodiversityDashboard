# Business Logic Functions ------------------------------------------------

#' @description  SelectedbdData is subset of biodiversity data corresponding to search by user search for species by their vernacularName and scientificName.
#' @param searchedVerncularNameID and searchedScientifNameID are input ids of Search Species by Vernacular Name and Search Species by scientific Name

SelectedbdData <- function(searchedVerncularNameID,
                           searchedScientificNameID) {
  tryCatch(
    expr = {
      load_bdData() %>%
        dplyr::filter(vernacularName == searchedVerncularNameID &
          scientificName == searchedScientificNameID)
    },
    error = function(e) {
      message("invalid parameter specified")
    }
  )
}

#' @description monthlyOccurence will count the occurence per month in chronological order
#' @param data is a data.frame which contain date format column, datecolumn is a date format column

monthlyOccurence <- function(data) {
  tryCatch(
    expr = {
      data %>%
        dplyr::mutate(event_month = months(base::as.Date(eventDate))) %>%
        group_by(event_month) %>%
        select(event_month) %>%
        count() %>%
        ungroup() %>%
        arrange(factor(event_month, levels = month.name))
    },
    error = function(e) {
      message("please enter non-null data")
    }
  )
}


#' @description barplot function will plot  a bar chart using echarts4r libary.
#' @param data, xAxisValues, yAxisValues,xaxisLabel,yaxisLabel,title are paramaeters required to plot bar chart
#' @import echarts4r

barplot <- function(data, xaxisLabel, yaxisLabel, title) {
  data %>%
    echarts4r::e_charts(event_month) %>%
    echarts4r::e_bar(n, legend = FALSE) %>%
    echarts4r::e_axis_labels(
      x = xaxisLabel,
      y = yaxisLabel
    ) %>%
    e_labels(position = "insideTop", fontSize = 8) %>%
    echarts4r::e_x_axis(axisLabel = list(
      interval = 0, rotate = 45, fontsize = 10
    )) %>%
    echarts4r::e_y_axis(
      nameLocation = "middle",
      nameGap = 30
    ) %>%
    echarts4r::e_title(title, left = "center", textStyle = list(fontWeight = "normal")) %>%
    echarts4r::e_title("Drag the slider to access specific part of the plot",
      left = "center", top = "85%",
      textStyle = list(fontSize = 10, fontWeight = "bolder", color = "lightblue")
    ) %>%
    echarts4r::e_grid(height = "60%") %>%
    echarts4r::e_color("green") %>%
    echarts4r::e_hide_grid_lines() %>%
    echarts4r::e_datazoom(x_index = c(0, 1)) %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      start = 0,
      end = 100
    ) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataZoom") %>%
    echarts4r::e_toolbox_feature(feature = "dataView") %>%
    echarts4r::e_toolbox_feature(feature = "restore") %>%
    echarts4r::e_toolbox_feature(feature = "magicType", type = c("line", "bar")) %>%
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(
      " function(param){
      return('Occurrance Month:' + param.value[0] +
      '<br />Occurences Count:' + param.value[1] )
      }
      "
    ))
}

#' @description lealetPlot function will map a selected obeservation by user and distribution of its occurences.
#' @param data used to plot the map
#' @import leaflet
leafletPlot <- function(data) {
  leaflet::leaflet(
    data = data
  ) %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(
      lat = ~decimalLatitude, lng = ~decimalLongitude,
      popup = paste0(
        "locality: ",
        data$locality,
        "<br>",
        "coordinate Uncertainty In Meters: ",
        data$coordinateUncertaintyInMeters,
        "<br>",
        "occurrence Status: ",
        data$occurrenceStatus,
        "<br>",
        "basis Of Record: ",
        data$basisOfRecord
      ),
      label = paste0(
        "locality: ",
        data$locality,
        " coordinate Uncertainty In Meters: ",
        data$coordinateUncertaintyInMeters,
        " occurrence Status: ",
        data$occurrenceStatus,
        " basis Of Record: ",
        data$basisOfRecord
      ),
      clusterOptions = markerClusterOptions()
    )
}

#' @description SpeciesbyScientificName will Choose species from scientific name species given Vernacular Name species, distinct() select unique values and pull() convert data.frame to vector
#' @param filteredNameID is Search Species by Vernacular Name or Scientific Name,
#'  selectedColumnName is selected column, SearchColumnName colum from user is seaching the species.

DistinctChoices <- function(filteredNameID, searchColumnName, selectedColumnName) {
  load_bdData() %>%
    dplyr::filter(.data[[searchColumnName]] == filteredNameID) %>%
    dplyr::select(.data[[selectedColumnName]]) %>%
    dplyr::distinct() %>%
    dplyr::pull()
}
#  UI helper functions -----------------------------------------------------------
# Added customize theme for app styling

freshTheme <- fresh::create_theme(
  fresh::bs4dash_vars(
    navbar_light_color = "#75B375",
    navbar_light_active_color = "#75B375",
    navbar_light_hover_color = "#75B375"
  ),
  fresh::bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF",
    text_light = "#00755c"
  ),
  fresh::bs4dash_layout(
    main_bg = "#FFF"
  ),
  fresh::bs4dash_status(
    primary = "#00755c", danger = "#BF616A", light = "#00755c", success = "#00755C"
  ),
  fresh::bs4dash_color(
    gray_900 = "#343a40"
  )
)


#' This file will create helper functions required for structure buiding
#' @import shiny bs4Dash shinyWidgets waiter fresh
#' @param title, subtitle, context for header , exra informaton and to set context respectivly.
createPage <- function(title, subtitle, context) {
  bs4Dash::dashboardPage(
    preloader = list(html = tagList(waiter::spin_1(), "Loading ...")),
    bs4Dash::dashboardHeader(title = dashboardBrand(
      title = title,
      color = "primary",
      href = "",
      image = ""
    ), status = "success"),
    bs4Dash::dashboardSidebar(disable = TRUE),
    bs4Dash::dashboardBody(
      # fresh::use_theme(freshTheme),
      shiny::fluidRow(
        column(
          4,
          mod_loadData_ui("loadData")
        ),
        column(
          8,
          bs4Dash::tabsetPanel(
            type = "pills",
            shiny::tabPanel(
              title = "Map",
              mod_viewMap_ui("map")
            ),
            shiny::tabPanel(
              title = "Timeline Visualization",
              mod_timelineVisualization_ui("timeline_plot")
            )
          )
        )
      )
    ),
    fullscreen = TRUE,
    freshTheme = freshTheme,
    dark = TRUE,
    scrollToTop = TRUE
  )
}

#' @description selectSpecies_ui will create a searchbox and dropdown for selecting species byVernacular name and by scientific name
#' @param id, label, choices used to give input id, label and list of all choices for selecting species byVernacular name and by scientific name

selectSpecies_ui <- function(id, label, choices) {
  shiny::selectizeInput(
    inputId = id,
    label = label,
    choices = choices
  )
}


# Server Helper functions -------------------------------------------------


#' @description updateSpecies will update a searchbox and dropdown for selecting species byVernacular name and by scientific name in the server
#' @param session, id, choices used to give input session, input id, and list of all choices for selecting species byVernacular name and by scientific name

updateSpecies <- function(session, id, choices) {
  updateSelectizeInput(
    session = session,
    inputId = id, choices = choices, server = TRUE
  )
}
