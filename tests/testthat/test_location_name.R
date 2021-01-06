context("eq_location_name")

test_that("location names has lowercase at second letter", {
  data <- data.frame(LOCATION_NAME = "FINLAND:  KORVATUNTURI")
  location_name <- eq_location_clean(data)
  expect_true(grepl("[a-z]", substr(location_name, 2, 2) ))
})
