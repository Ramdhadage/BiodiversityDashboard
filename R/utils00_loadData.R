#' Load Biodiversiry Data
#' @description  load_bdData function  will use internal stored data select veritable required for this project and return it.
#' @import tibble dplyr
#' @importFrom magrittr %>%
#' @importFrom dplyr %>%

load_bdData <- function() {
  bdData <- bdData %>%
    dplyr::select(
      .data$scientificName, .data$vernacularName, .data$species,
      .data$decimalLatitude, .data$decimalLongitude, .data$coordinateUncertaintyInMeters, .data$locality, .data$eventDate, .data$occurrenceStatus, .data$basisOfRecord
    ) %>%
    dplyr::mutate(vernacularName = replace(.data$vernacularName, .data$vernacularName == "", "Not Available"))
  return(bdData)
}
