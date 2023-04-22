# Business Logic Functions ------------------------------------------------

#' @description  SelectedbdData is subset of biodiversity data corresponding to search by user search for species by their vernacularName and scientificName.
#' @param searchedVerncularNameID and searchedScientifNameID are input ids of Search Species by Vernacular Name and Search Species by scientific Name

SelectedbdData <- function(radioBtn_search_byName, radioBtn_search_byNameID,
                           searchByVerOrSciName,
                           selectedByVerOrSciName) {
  tryCatch(
    expr = {
      if (radioBtn_search_byName == radioBtn_search_byNameID) {
        load_bdData() %>%
          dplyr::filter(vernacularName == searchByVerOrSciName &
            scientificName == selectedByVerOrSciName)
      } else {
        load_bdData() %>%
          dplyr::filter(vernacularName == selectedByVerOrSciName &
            scientificName == searchByVerOrSciName) # in this section we are selecting species by scientific name.
      }
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
#' @param data, xAxisValues, yAxisValues,xaxisLabel,yaxisLabel,title,are paramaeters required to plot bar chart. flg_darkMode is boolean variable for dark mode.
#' @import echarts4r

barplot <- function(data, xaxisLabel, yaxisLabel, title, flg_darkMode = FALSE) {
  barplot <- data %>%
    echarts4r::e_charts(event_month) %>%
    echarts4r::e_bar(n, legend = FALSE) %>%
    echarts4r::e_axis_labels(
      x = xaxisLabel,
      y = yaxisLabel
    ) %>%
    e_labels(position = "insideTop", fontSize = 8) %>%
    echarts4r::e_x_axis(
      nameTextStyle = list(color = "white"),
      axisLabel = list(
        interval = 0, rotate = 45, fontsize = 10, color = "white"
      )
    ) %>%
    echarts4r::e_y_axis(
      nameLocation = "middle",
      nameGap = 30,
      nameTextStyle = list(color = "white"),
      axisLabel = list(fontsize = 10, color = "white")
    ) %>%
    echarts4r::e_title(title, left = "center", textStyle = list(fontWeight = "normal", color = "white")) %>%
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
    echarts4r::e_toolbox_feature(feature = "saveAsImage", iconStyle = list(borderColor = "lightgreen")) %>%
    echarts4r::e_toolbox_feature(feature = "dataZoom", iconStyle = list(borderColor = "lightgreen")) %>%
    echarts4r::e_toolbox_feature(feature = "dataView", iconStyle = list(borderColor = "lightgreen")) %>%
    echarts4r::e_toolbox_feature(feature = "restore", iconStyle = list(borderColor = "lightgreen")) %>%
    echarts4r::e_toolbox_feature(feature = "magicType", type = c("line", "bar"), iconStyle = list(borderColor = "lightgreen")) %>%
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(
      " function(param){
      return('Occurrance Month:' + param.value[0] +
      '<br />Occurences Count:' + param.value[1] )
      }
      "
    ))
  if (!flg_darkMode) {
    barplot <- barplot %>%
      echarts4r::e_x_axis(
        nameTextStyle = list(color = "black"),
        axisLabel = list(
          interval = 0, rotate = 45, fontsize = 10, color = "black"
        )
      ) %>%
      echarts4r::e_y_axis(
        nameLocation = "middle",
        nameGap = 30,
        nameTextStyle = list(color = "black"), axisLabel = list(fontsize = 10, color = "black")
      ) %>%
      echarts4r::e_title(title, left = "center", textStyle = list(fontWeight = "normal", color = "black")) %>%
      echarts4r::e_title("Drag the slider to access specific part of the plot",
        left = "center", top = "85%",
        textStyle = list(fontSize = 10, fontWeight = "bolder", color = "black")
      ) %>%
      echarts4r::e_datazoom(x_index = c(0, 1), backgroundColor = "lightgreen") %>%
      echarts4r::e_toolbox_feature(feature = "saveAsImage", iconStyle = list(borderColor = "black")) %>%
      echarts4r::e_toolbox_feature(feature = "dataZoom", iconStyle = list(borderColor = "black")) %>%
      echarts4r::e_toolbox_feature(feature = "dataView", iconStyle = list(borderColor = "black")) %>%
      echarts4r::e_toolbox_feature(feature = "restore", iconStyle = list(borderColor = "black")) %>%
      echarts4r::e_toolbox_feature(feature = "magicType", type = c("line", "bar"), iconStyle = list(borderColor = "black"))
  }
  barplot
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

#' @description DistinctChoices will Choose species from scientific name species given Vernacular Name species, distinct() select unique values and pull() convert data.frame to vector
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


#' This file will create helper functions required for structure building
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
          mod01_loadInputs_ui("inputs")
          # mod_loadData_ui("loadData")
        ),
        column(
          8,
          bs4Dash::tabsetPanel(
            type = "pills",
            shiny::tabPanel(
              title = "Map",
              mod02_viewMap_ui("map")
            ),
            shiny::tabPanel(
              title = "Timeline Visualization",
              mod03_timelineVisualization_ui("timeline_plot")
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

#' @description dropdownBasedOnRadioBtn will create a searchable dropdown for selecting species byVernacular name and by scientific name based on radio button for Search species by Vernacuar Name or Scientific Name
#' @param radioBtn_search_byName andradioBtn_search_byNameID are Search by Name radio button input value and specific choice like Vernacular Name or Scientific Name.
#'  id1, id2, label1, label2, choices1,choices2  used to give input ids, labels and list of all choices for selecting species byVernacular name and by scientific name

dropdownBasedOnRadioBtn <- function(radioBtn_search_byName, radioBtn_search_byNameID, id1, label1, choices1, id2, label2, choices2) {
  if (radioBtn_search_byName == radioBtn_search_byNameID) {
    shiny::selectizeInput(
      inputId = id1,
      label = label1,
      choices = choices1
    )
  } else {
    shiny::selectizeInput(
      inputId = id2,
      label = label2,
      choices = choices2
    )
  }
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
#' @description  updateDropdownBasedOnRadioBtnValues will updates input values of searchable dropdowns selecting species byVernacular name and by scientific name based on radio button for Search species by Vernacuar Name or Scientific Name
#' @param radioBtn_search_byName and radioBtn_search_byNameID_ver,radioBtn_search_byNameID_sci are Search by Name radio button input value and specific choice like Vernacular Name or Scientific Name.
#'  id_ver_name, id_sci_name are input values of Search Species by Vernacular Name or Scientific Name
#'  scientificName_str, vernacularName_str constant string "scientificName" and "vernacularName"
#' id_selected_sci_name, id_selected_ver_name,session are input values of Matching Scientific Name and Matching Vernacular Name and session of shiny app.

updateDropdownBasedOnRadioBtnValues <- function(radioBtn_search_byName,
                                                radioBtn_search_byNameID_ver,
                                                id_ver_name, scientificName_str, vernacularName_str,
                                                id_selected_sci_name, radioBtn_search_byNameID_sci,
                                                id_sci_name, id_selected_ver_name, session) {
  # added cache results to the function updateSpecies for performance improvement

  updateSpecies_m <- memoise::memoise(updateSpecies, cache = session$cache)

  # added cache results to the function SpeciesbyScientificName_m for performance improvement

  DistinctChoices_m <- memoise::memoise(DistinctChoices, cache = session$cache)

  if (radioBtn_search_byName == radioBtn_search_byNameID_ver & !is.null(id_ver_name)) {
    # Choose species from Scientific  name column from bdData species given Vernacular Name species
    SpeciesbyScientificName <- DistinctChoices_m(
      filteredNameID = id_ver_name,
      selectedColumnName = scientificName_str,
      searchColumnName = vernacularName_str
    )

    # update matching Scientific Species name based on vernacular species  name

    updateSpecies_m(
      session = session,
      id = "selected_sci_name",
      choices = SpeciesbyScientificName
    )
  } else if (radioBtn_search_byName == radioBtn_search_byNameID_sci & !is.null(id_sci_name)) {
    req(id_selected_sci_name) # to stop NULL,"", empty vector etc inputs
    # Choose species from Vernacular name column from bdData species given scientific Name species
    SpeciesbyvernacularName <- DistinctChoices_m(
      filteredNameID = id_sci_name,
      selectedColumnName = vernacularName_str,
      searchColumnName = scientificName_str
    )
    # update matching vernacular Species name based on Scientific species name
    updateSpecies_m(
      session = session,
      id = "selected_ver_name",
      choices = SpeciesbyvernacularName
    )
  }
}
