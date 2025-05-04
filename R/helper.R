# Business Logic Functions ------------------------------------------------
#' Filter biodiversity data based on search criteria
#'
#' This function subsets the biodiversity data based on user search criteria
#' for species by their vernacular name and scientific name. The function handles
#' two search modes determined by the radio button selection.
#'
#' @param radioBtn_search_byName Character. The current value of the search mode radio button.
#' @param radioBtn_search_byNameID Character. The ID value that indicates search by vernacular name mode.
#' @param searchByVerOrSciName Character. The primary search term entered by the user.
#' @param selectedByVerOrSciName Character. The secondary selection made by the user.
#'
#' @return A data frame containing the filtered biodiversity data that matches both
#'   the vernacular name and scientific name criteria.
#'
#' @details
#' The function behaves differently based on the search mode:
#' - If radioBtn_search_byName equals radioBtn_search_byNameID, it filters by vernacular name
#'   first (searchByVerOrSciName) and then by scientific name (selectedByVerOrSciName).
#' - Otherwise, it filters by scientific name first (searchByVerOrSciName) and then
#'   by vernacular name (selectedByVerOrSciName).
#' @importFrom dplyr filter
SelectedbdData <- function(radioBtn_search_byName, radioBtn_search_byNameID,
                           searchByVerOrSciName,
                           selectedByVerOrSciName) {
  tryCatch(
    expr = {
      if (radioBtn_search_byName == radioBtn_search_byNameID) {
        # Search by vernacular name first, then scientific name
        load_bdData() %>%
          dplyr::filter(.data$vernacularName == searchByVerOrSciName &
                          .data$scientificName == selectedByVerOrSciName)
      } else {
        # Search by scientific name first, then vernacular name
        load_bdData() %>%
          dplyr::filter(.data$vernacularName == selectedByVerOrSciName &
                          .data$scientificName == searchByVerOrSciName)
      }
    },
    error = function(e) {
      message("Invalid parameter specified")
    }
  )
}
#' Monthly Occurence of Animals
#' @description monthlyOccurence will count the occurence per month in chronological order
#' @param data is a data.frame which contain date format column, datecolumn is a date format column

monthlyOccurence <- function(data) {
  tryCatch(
    expr = {
      data %>%
        dplyr::mutate(event_month = months(base::as.Date(.data$eventDate))) %>%
        group_by(.data$event_month) %>%
        select(.data$event_month) %>%
        count() %>%
        ungroup() %>%
        arrange(factor(.data$event_month, levels = month.name))
    },
    error = function(e) {
      message("please enter non-null data")
    }
  )
}

#' Create Interactive Bar Plot with echarts4r
#'
#' @description
#' This function creates an interactive bar chart visualization using the echarts4r library.
#' The visualization includes interactive features such as data zooming, tooltips, and export options.
#' It supports both light and dark themes through the `flg_darkMode` parameter.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_axis_var x axis variable name from the data.
#' @param y_axis_var y axis variable name from the data.
#' @param xaxisLabel Character string specifying the label for the x-axis.
#' @param yaxisLabel Character string specifying the label for the y-axis.
#' @param title Character string specifying the title of the plot.
#' @param flg_darkMode Logical value (TRUE/FALSE) indicating whether to use dark mode styling.
#'        Default is FALSE (light mode).
#'
#' @return An echarts4r visualization object that can be displayed in a Shiny app or R Markdown document.
#'
#' @details
#' The function provides several interactive features:
#' - Data zooming through a slider at the bottom
#' - Tooltips showing exact values on hover
#' - Option to switch between bar and line representations
#' - Export functionality for saving the chart as an image
#' - Data view for examining the underlying data
#'
#' The styling automatically adjusts based on the `flg_darkMode` parameter, with appropriate
#' color schemes for both light and dark modes.
#'
#' @examples
#' # Create sample data
#' data <- data.frame(
#'   event_month = month.abb[1:12],
#'   n = sample(50:100, 12)
#' )
#'
#' # Create and display the bar plot
#' create_bar_plot(
#'   data = data,
#'   xaxisLabel = "Month",
#'   yaxisLabel = "Count",
#'   title = "Monthly Events",
#'   flg_darkMode = FALSE
#' )
#'
#' @import echarts4r
#' @importFrom htmlwidgets JS
#' @importFrom magrittr %>%
#'
#' @export

create_bar_plot <- function(data, x_axis_var = "event_month", y_axis_var = "n",  xaxisLabel, yaxisLabel, title, flg_darkMode = FALSE) {
  barplot <- data %>%
    echarts4r::e_charts_(x_axis_var) %>%
    echarts4r::e_bar_(y_axis_var, legend = FALSE) %>%
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

#' Leaflet Plot
#' @description lealetPlot function will map a selected obeservation by user and distribution of its occurences.
#' @param data used to plot the map
#' @import leaflet
#' @export
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
#' Extract unique values from a column based on a filter condition
#'
#' This function filters the biodiversity data based on a search value in a specified column,
#' then extracts unique values from another selected column.
#'
#' @param filteredNameID Character. The value to filter by in the search column.
#' @param searchColumnName Character. The name of the column to search for the filteredNameID value.
#' @param selectedColumnName Character. The name of the column from which to extract unique values.
#' @return A character vector of unique values from the selected column.
#' @importFrom dplyr filter select distinct pull
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

#' UI function for the Biodiversity App
#'
#' @description Creates the main dashboard page structure for the Biodiversity App
#'
#' @param title Character string for the dashboard title
#' @param subtitle Character string for the dashboard subtitle
#' @param context Character string to set the app context
#'
#' @return A bs4Dash dashboard page
#'
#' @importFrom shinyWidgets progressBar
#' @import shiny waiter fresh
#'
#' @export

createPage <- function(title, subtitle, context) {
  bs4Dash::dashboardPage(
    preloader = list(html = tagList(waiter::spin_1(), "Loading ...")),
    bs4Dash::dashboardHeader(title = bs4Dash::dashboardBrand(
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

#' Create Species Dropdown Based on Radio Button Selection
#'
#' @description Creates a searchable dropdown for species selection that changes based on the
#'   radio button selection. Can display either vernacular names or scientific names.
#'
#' @param radioBtn_search_byName The current value of the radio button input
#' @param radioBtn_search_byNameID The specific choice value to check against (e.g., "Vernacular" or "Scientific")
#' @param id1 Input ID for the first dropdown option (typically vernacular names)
#' @param label1 Label text for the first dropdown option
#' @param choices1 List of choices for the first dropdown option
#' @param id2 Input ID for the second dropdown option (typically scientific names)
#' @param label2 Label text for the second dropdown option
#' @param choices2 List of choices for the second dropdown option
#'
#' @return A shiny selectizeInput UI element with the appropriate options based on radio button selection
#'
#' @importFrom shiny selectizeInput
#'
#' @export
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

#' Update Species Dropdown
#' @description updateSpecies will update a searchbox and dropdown for selecting species byVernacular name and by scientific name in the server
#' @param session The Shiny session object
#' @param id The input ID of the selectize input to update
#' @param choices A character vector of available species choices to display in the dropdown
#'
#' @return No return value; called for side effects
#'
updateSpecies <- function(session, id, choices) {
  updateSelectizeInput(
    session = session,
    inputId = id, choices = choices, server = TRUE
  )
}
#' Update dropdown inputs based on radio button selection
#'
#' @description
#' Updates searchable dropdown inputs for species selection based on the user's choice
#' of searching by vernacular name or scientific name. When a user selects a species
#' in one dropdown, this function updates the corresponding value in the other dropdown.
#'
#' @param radioBtn_search_byName The current value of the radio button that determines
#'        the search method (vernacular or scientific name)
#' @param radioBtn_search_byNameID_ver The value of the radio button that corresponds to
#'        searching by vernacular name
#' @param id_ver_name The input value from the vernacular name dropdown
#' @param scientificName_str Constant string identifier for scientific name, typically "scientificName"
#' @param vernacularName_str Constant string identifier for vernacular name, typically "vernacularName"
#' @param id_selected_sci_name The input value from the matching scientific name dropdown
#' @param radioBtn_search_byNameID_sci The value of the radio button that corresponds to
#'        searching by scientific name
#' @param id_sci_name The input value from the scientific name dropdown
#' @param id_selected_ver_name The input value from the matching vernacular name dropdown
#' @param session The Shiny session object
#'
#' @import memoise htmlwidgets
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
