#' Load Biodiversiry Data
#' @description  load_bdData function  will use internal stored data select veritable required for this project and return it.
#' @import tibble dplyr
#' @importFrom magrittr %>%
#' @importFrom dplyr %>%

load_bdData <- function() {
  bdData <- bdData %>%
    dplyr::select(
      scientificName, vernacularName, species,
      decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, locality, eventDate, occurrenceStatus, basisOfRecord
    ) %>%
    dplyr::mutate(vernacularName = replace(vernacularName, vernacularName == "", "Not Available"))
  return(bdData)
}
