context("eq_create_label")

test_that("if the number of deaths are missing, label has not row for them", {
  data <- data.frame(LOCATION_NAME = "FINLAND", EQ_PRIMARY = "0.0", TOTAL_DEATHS = NA)
  label <- NULL
  label <- eq_create_label(data)
  expect_false(grepl("deaths", label))
})
