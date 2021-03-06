context("geom_timeline")
library(ggplot2)

dt <- structure(list(COUNTRY = rep("MAA", 5),
                     LOCATION_NAME = c("Ankkalinna", "Mordor", "Korvatuntunturi", "Atlantis", "Oz"),
                     DATE = structure(c(1264774400, 1378993600, 1477993700, 1577993800, 1687993900),
                                      class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                     EQ_PRIMARY = c(8.3, 5.7, 1.1, 3.4, 6.6),
                     LATITUDE = c(38.381, 37.670, 41.789, 43.883, 44.329),
                     LONGITUDE = c(13.701, 15.267, 14.872, 11.961, 11.451),
                     DEATHS = c(4, 55, NA, 2, 78)),
                .Names = c("COUNTRY", "LOCATION_NAME", "DATE", "EQ_PRIMARY", "LATITUDE", "LONGITUDE", "DEATHS"),
                row.names = c(NA, 5L), class = "data.frame")


obj <- ggplot(dt) +
  geom_timeline( aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS))

test_that("geom_timeline data has DATE variable", {
  expect_true("DATE" %in% names(obj$data))
})

