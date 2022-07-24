library(shinytest)
library(testthat)

testServer(expr = {
  cat(" searchSpeciesIds should initiates with null")
  expect_equal(searchSpeciesIds$vernacularNameId, NULL)
  expect_equal(searchSpeciesIds$vernacularNameId, NULL)
})
