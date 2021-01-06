#'Make label to leaflet map
#'
#'This function creates leaflet map labels
#' @param data dataset with variables LOCATION_NAME, EQ_PRIMARY (magnitude)
#'             and TOTAL_DEATHS for number of deaths
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' # With several countries and popup text
#' data <- data.frame(LOCATION_NAME = "FINLAND", EQ_PRIMARY = "0.0", TOTAL_DEATHS = NA)
#' eq_create_label(data)
#' }
#' @export
eq_create_label <- function(data){
  # Make popup_text
  data <- data %>%
    dplyr::mutate(popup_text = "",
           popup_text = ifelse(!is.na(LOCATION_NAME),
                               paste(popup_text,
                                     "<b>Location:</b>", LOCATION_NAME, "<br />"),
                               popup_text),
           popup_text = ifelse(!is.na(EQ_PRIMARY),
                               paste(popup_text,
                                     "<b>Magnitude:</b>", EQ_PRIMARY, "<br />"),
                               popup_text),
           popup_text = ifelse(!is.na(TOTAL_DEATHS),
                               paste(popup_text,
                                     "<b>Total deaths:</b>", TOTAL_DEATHS, "<br />"),
                               popup_text))
  return(data$popup_text)
}

#' Plot map with earthquakes
#'
#' This function plots map with earthquakes. One or several countries
#' can be chosen.
#'
#' @param annot_col Annotation. Can be any variable from dataset.
#'                  Argument "popup_text" returns location name,
#'                  magnitude and numbrer of deaths, if available.
#'
#' @return Map plot with interactive labels.
#'
#' @importFrom dplyr mutate_at
#' @importFrom leaflet addCircleMarkers addTiles leaflet
#' @importFrom magrittr %>%
#'
#' @param data Dataset with variables LOCATION_NAME, EQ_PRIMARY (magnitude)
#'             and TOTAL_DEATHS for number of deaths.
#' @param annot_col Column name to be used for annotations.
#'
#
#' @examples \dontrun{
#' # With several countries and popup text
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'  eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "ITALY" | COUNTRY == "TURKEY" & lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#' }.
#'
#' @export
eq_map <- function(data, annot_col){

  # Mutate other than latitude, longitude and eq_primary to characters
  # (popup doesn't seem to work with numeric variables)
  vars <- names(data)
  vars <- grep("LATITUDE|LONGITUDE|EQ_PRIMARY", vars, invert = TRUE)
  data <- data %>% dplyr::mutate_at(vars, as.character)

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data, radius = ~ EQ_PRIMARY, weight = 1,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup =  ~ eval(parse(text = annot_col)))

}
