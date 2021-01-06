context("eq_clean_data")
library(dplyr)

dt <- structure(list(COUNTRY = rep("MAA", 5),
                     LOCATION_NAME = c("Ankkalinna", "Mordor", "Korvatuntunturi", "Atlantis", "Oz"),
                     YEAR = c(1899, 1999, 2000, 2011, 2020),
                     MONTH = c(12, 1, 4, 6, 8),
                     DAY = c(1, 22, 14, 21, 3),
                     EQ_PRIMARY = c("8.3", "5.7", "1.1", "3.4", "6.6"),
                     LATITUDE = c("38.381", "37.670", "41.789", "43.883", "44.329"),
                     LONGITUDE = c("13.701", "15.267", "14.872", "11.961", "11.451"),
                     DEATHS = c(4, 55, NA, 2, 78)),
                .Names = c("COUNTRY", "LOCATION_NAME", "YEAR", "MONTH", "DAY", "EQ_PRIMARY", "LATITUDE", "LONGITUDE", "DEATHS"),
                row.names = c(NA, 5L), class = "data.frame")

test_that("latitude is measurable and less than 91 degrees", {
  latitude <- max(na.omit(eq_clean_data(dt) %>% select("LATITUDE")))
  expect_lt(latitude, 91)
})
