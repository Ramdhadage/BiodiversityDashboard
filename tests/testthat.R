# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(BiodiversityDashboard)

# test_check("BiodiversityDashboard")
test_dir(
  "./tests/testthat",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(),
  # Display the regular progress output and throw an error if any test error is found
  reporter = c("progress", "fail")
)
