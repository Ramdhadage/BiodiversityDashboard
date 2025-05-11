library(testthat)
library(dplyr)
library(checkmate)

test_that("selected_bd_data filters by vernacular name first when specified", {
  # Create test data
  test_data <- data.frame(
    vernacularName = c("Cat", "Dog", "Cat", "Bird"),
    scientificName = c("Felis catus", "Canis familiaris", "Felis silvestris", "Aves")
  )

  # Test
  result <- selected_bd_data(
    data = test_data,
    radio_btn_search_by_name = "vernacular",
    radio_btn_search_by_name_id = "vernacular",
    search_by_ver_or_sci_name = "Cat",
    selected_by_ver_or_sci_name = "Felis catus"
  )

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$vernacularName, "Cat")
  expect_equal(result$scientificName, "Felis catus")
})

test_that("selected_bd_data filters by scientific name first when specified", {
  # Create test data
  test_data <- data.frame(
    vernacularName = c("Cat", "Dog", "Cat", "Bird"),
    scientificName = c("Felis catus", "Canis familiaris", "Felis silvestris", "Aves")
  )

  # Test
  result <- selected_bd_data(
    data = test_data,
    radio_btn_search_by_name = "scientific",
    radio_btn_search_by_name_id = "vernacular",
    search_by_ver_or_sci_name = "Felis catus",
    selected_by_ver_or_sci_name = "Cat"
  )

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$vernacularName, "Cat")
  expect_equal(result$scientificName, "Felis catus")
})

test_that("selected_bd_data correctly handles invalid parameters", {
  # Create test data
  test_data <- data.frame(
    vernacularName = c("Cat", "Dog"),
    scientificName = c("Felis catus", "Canis familiaris")
  )

  # Test NULL data
  expect_error(
    selected_bd_data(
      data = NULL,
      radio_btn_search_by_name = "vernacular",
      radio_btn_search_by_name_id = "vernacular",
      search_by_ver_or_sci_name = "Cat",
      selected_by_ver_or_sci_name = "Felis catus"
    ),
    regexp = "Assertion on 'data' failed"
  )

  # Test missing columns
  bad_data <- data.frame(
    common_name = c("Cat", "Dog"),
    latin_name = c("Felis catus", "Canis familiaris")
  )

  expect_error(
    selected_bd_data(
      data = bad_data,
      radio_btn_search_by_name = "vernacular",
      radio_btn_search_by_name_id = "vernacular",
      search_by_ver_or_sci_name = "Cat",
      selected_by_ver_or_sci_name = "Felis catus"
    ),
    regexp = "Assertion on 'c\\(\"vernacularName\", \"scientificName\"\\)' failed"
  )
})

test_that("selected_bd_data returns empty data frame when no matches found", {
  # Create test data
  test_data <- data.frame(
    vernacularName = c("Cat", "Dog"),
    scientificName = c("Felis catus", "Canis familiaris")
  )

  # Test
  result <- selected_bd_data(
    data = test_data,
    radio_btn_search_by_name = "vernacular",
    radio_btn_search_by_name_id = "vernacular",
    search_by_ver_or_sci_name = "Bird",
    selected_by_ver_or_sci_name = "Aves"
  )

  # Verify
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

