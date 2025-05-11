#' Load Biodiversity Data
#'
#' @description This function loads internally stored biodiversity data,
#'   selects variables required for this project, and returns the processed data.
#'   It handles null data with appropriate error messaging.
#'
#' @return A tibble containing the processed biodiversity data or NULL if an error occurs
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom cli cli_alert_danger cli_alert_warning cli_alert_success
#' @param data A data frame. The biodiversity dataset to filter.
#' @export
load_bd_data <- function(data = bdData) {
  tryCatch(
    expr = {
      # Check if data exists in the environment
      if (!exists("data") || is.null(data)) {
        cli::cli_alert_danger("Biodiversity data not found or is NULL")
      }
      bd_data <- data %>%
        dplyr::select(
          tidyselect::all_of(
            c(
              "scientificName",
              "vernacularName",
              "species",
              "decimalLatitude",
              "decimalLongitude",
              "coordinateUncertaintyInMeters",
              "locality",
              "eventDate",
              "occurrenceStatus",
              "basisOfRecord"
            ))) %>%
        dplyr::mutate(
          vernacularName = replace(.data$vernacularName, .data$vernacularName == "", "Not Available")
        )

      cli::cli_alert_success("Successfully loaded biodiversity data with {nrow(bd_data)} records")
      return(bd_data)
    },
    error = function(e) {
      cli::cli_alert_danger("Error loading biodiversity data: {e$message}")
      return(NULL)
    }
  )
}
