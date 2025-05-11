library(testthat)
library(dplyr)


test_that("load_bd_data correctly processes valid data", {
  # Create mock data
  data <- tibble::tibble(
    scientificName = c("Panthera leo", "Canis lupus"),
    vernacularName = c("Lion", ""),
    species = c("leo", "lupus"),
    decimalLatitude = c(0.1, 1.2),
    decimalLongitude = c(2.3, 3.4),
    coordinateUncertaintyInMeters = c(10, 20),
    locality = c("Serengeti", "Yellowstone"),
    eventDate = as.Date(c("2020-01-01", "2020-02-02")),
    occurrenceStatus = c("present", "present"),
    basisOfRecord = c("observation", "specimen")
  )


  # Execute
  result <- load_bd_data(data)

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 10)
  expect_equal(result$vernacularName[2], "Not Available")

})

test_that("load_bd_data returns NULL when bdData doesn't exist", {
  # Setup
  # if (exists("data", envir = .GlobalEnv)) {
  #   rm("data", envir = .GlobalEnv)
  # }

  # Capture messages
  messages <- capture_messages(result <- load_bd_data(data1))

  # Verify
  expect_null(result)
})

test_that("load_bd_data returns NULL when data is NULL", {
  # Setup
  data <- NULL
  # Capture messages
  messages <- capture_messages(result <- load_bd_data(data))

  # Verify
  expect_null(result)
})

test_that("load_bd_data handles missing columns gracefully", {
  # Create mock data with missing columns
  data <- tibble::tibble(
    scientificName = c("Panthera leo", "Canis lupus"),
    vernacularName = c("Lion", "Wolf"),
    # Missing species column
    decimalLatitude = c(0.1, 1.2),
    decimalLongitude = c(2.3, 3.4)
    # Missing other columns too
  )


  # Capture messages
  messages <- capture_messages(result <- load_bd_data(data))
  # Strip ANSI formatting
  plain_messages <- fansi::strip_sgr(messages)
  # Verify
  expect_null(result)
  expect_match(plain_messages, "Error loading biodiversity data:")

})


test_that("load_bd_data handles empty data frame correctly", {

  # Create empty data frame with required columns
  data <- tibble::tibble(
    scientificName = character(0),
    vernacularName = character(0),
    species = character(0),
    decimalLatitude = numeric(0),
    decimalLongitude = numeric(0),
    coordinateUncertaintyInMeters = numeric(0),
    locality = character(0),
    eventDate = as.Date(character(0)),
    occurrenceStatus = character(0),
    basisOfRecord = character(0)
  )


  # Execute
  result <- load_bd_data(data)

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 10)

})

test_that("load_bd_data works with different data types", {

  # Create mock data with various data types
  data <- tibble::tibble(
    scientificName = as.factor(c("Panthera leo", "Canis lupus")),
    vernacularName = c("Lion", ""),
    species = c("leo", "lupus"),
    decimalLatitude = c(0.1, 1.2),
    decimalLongitude = c(2.3, 3.4),
    coordinateUncertaintyInMeters = as.integer(c(10, 20)),
    locality = c("Serengeti", "Yellowstone"),
    eventDate = c("2020-01-01", "2020-02-02"),  # String instead of Date
    occurrenceStatus = as.logical(c(TRUE, FALSE)),
    basisOfRecord = c("observation", "specimen")
  )


  # Execute
  result <- load_bd_data(data)

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$vernacularName[2], "Not Available")

})


