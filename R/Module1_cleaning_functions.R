globalVariables(c("DAY", "DEATHS", "EQ_PRIMARY", "LATITUDE", "LOCATION_NAME", "LONGITUDE", "MONTH",
                  "TOTAL_DEATHS", "YEAR", "popup_text"))


#' Clean location name
#'
#' This function cleans a location name.
#'
#' @param data Data with location name variable "LOCATION_NAME".
#'
#' @return This function returns location name as title case and without any text before ":".
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' data <- data.frame(LOCATION_NAME = "FINLAND:  KORVATUNTURI")
#'  eq_location_clean(data)
#' }
#'
#' @export
eq_location_clean <- function(data){
  data %>%
    dplyr::mutate(LOCATION_NAME = tools::toTitleCase(tolower(gsub(".*:. ", "", LOCATION_NAME))))
}

#' Clean earthquake data
#'
#' This function cleans earthquake data provided by
#' National Centers for Environmental Information.
#'
#' @param data Earthquake data.
#'
#' @return This function returns cleaned data with new date variable
#'   and converts key variables used in package functions as numeric.
#'
#' @examples \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'    eq_clean_data()
#' }
#'
#' @references  National Centers for Environmental Information,
#' National Oceanic and Atmospheric Administration (NOAA)
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#'
#' @note
#' Variable names as default YEAR, MONTH, DAY, LATITUDE, LONGITUDE, EQ_PRIMARY and DEATHS
#'
#' @export
eq_clean_data <- function(data){
  data %>%
    dplyr::filter(YEAR > 0 & !is.na(DAY) & !is.na(MONTH)) %>%
    dplyr::mutate(DATE = ISOdatetime(year = YEAR, month = MONTH, day = DAY, hour = 0, min = 0, sec = 0, tz = "GMT"),
                  LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY), DEATHS = as.numeric(DEATHS)) %>%
    eq_location_clean()
}
