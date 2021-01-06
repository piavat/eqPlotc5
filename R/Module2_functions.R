

#' Modify plot theme
#'
#' Plot theme is modified from theme_minimal from qqplot2 package.
#'
#' @importFrom ggplot2 theme theme_minimal element_blank element_line
#' @export
theme_eq <- ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom", panel.grid.major.x = element_blank(), panel.grid.minor.x =element_blank(), axis.line.x = element_line(colour = "black"))



#' ggproto
#' @format NULL
#' @usage NULL
#' @export
geomTimeline <- ggplot2::ggproto("geomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(colour = "grey",
                                                            fill = "grey",
                                                            size = 0.5,
                                                            shape = 21,
                                                            alpha = 0.5,
                                                            y = 0.2),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {
                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)

                                   points <- pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     size = grid::unit(coords$size, "mm"),
                                     pch = coords$shape,

                                     gp = grid::gpar(size = coords$size,
                                                     col = scales::alpha(data$colour, data$alpha),
                                                     fill = scales::alpha(data$fill, data$alpha))
                                   )
                                 }
)



#' Make time line points
#'
#' This function makes plot time line of earthquakes in selected time range with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency). The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification in which case multiple time lines will be plotted for each level of the factor (e.g. country).
#'

#' @param mapping List of aesthetic created by aes() to use for plot.
#' @param data The data to be displayed in this layer. NULL, the default, is the data inherited from the plot data as specified in the call to ggplot().
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param ... Other arguments passed on to layer.
#' @param position Position adjustment.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' @param inherit.aes Will aesthetics be inherited (TRUE/FALSE). If FALSE, overrides the default aesthetics, rather than combining with them.
#
#'
#' @return PointsGrob object.
#'
#' @importFrom ggplot2 layer
#'
#' @examples \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'eq_clean_data() %>%
#'  dplyr::filter((COUNTRY == "USA" ) & (lubridate::year(DATE) >= 2000 &
#'                                         lubridate::year(DATE) <= 2020)) %>%
#'  ggplot() +
#'  theme_eq + ggplot2::labs(size = "Richter scale value", fill = "# deaths") +
#'  geom_timeline( aes(x = DATE, size = EQ_PRIMARY, fill = DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {


    ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geomTimeline,
    position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

###############################################################################################################

#' ggproto
#' @format NULL
#' @usage NULL
#' @export
geomTimelineLabel <- ggplot2::ggproto("geomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      optional_aes = c("y", "size"),
                                      default_aes = ggplot2::aes(col = "black",
                                                        just = 0,
                                                        n_max = 5,
                                                        angle = 45,
                                                        alpha = 0.5,
                                                        y = 0.2),

                                      draw_panel = function(data, panel_scales, coord, n_max) {

                                        coords <- coord$transform(data, panel_scales)

                                        n_max <- unique(coords$n_max)

                                        # Print only selected amount of labels by group
                                          coords <- coords %>%
                                            dplyr::group_by(group) %>%
                                            dplyr::arrange(desc(size), by_group = TRUE) %>%
                                            dplyr::mutate(Order = 1:length(group)) %>%
                                            dplyr::filter(Order<=n_max)


                                        # Combine text and line
                                        txt <- textGrob(
                                                        y = unit(coords$y+0.055, "npc"),
                                                        x = unit(coords$x, "npc"),
                                                        label = coords$label,
                                                        just = coords$just,
                                                        rot = coords$angle,
                                                        check.overlap = FALSE,
                                                        gp = gpar(col = scales::alpha("black", 1)),
                                                        vp = NULL)

                                        linex <- segmentsGrob(x0 = coords$x,
                                                              x1 = coords$x,
                                                              y0 = coords$y,
                                                              y1 = unit(coords$y+0.05, "npc"),
                                                              gp = gpar(lwd = 1, col = scales::alpha(data$colour, data$alpha)))
                                        gTree(children = gList(txt, linex))
                                      }
)


#' Make time line labels
#'
#' This function adds annotations to the timeline plot. There is an option to subset to `n_max` to select the number of annotated earthquakes ordered by magnitude.
#'
#' @importFrom ggplot2 aes ggproto Geom
#' @importFrom dplyr arrange filter group_by mutate
#' @importFrom grid gpar gTree segmentsGrob textGrob unit
#'
#' @param mapping List of aesthetic created by aes() to use for plot.
#' @param data  The data to be displayed in this layer. NULL, the default, is the data inherited from the plot data as specified in the call to ggplot().
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment.
#' @param ... Other arguments passed on to layer.
#' @param nudge_x Horizontal adjustment to nudge labels by.
#' @param nudge_y Vertical adjustment to nudge labels by.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' @param inherit.aes Will aesthethics be inherited (TRUE/FALSE). If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param n_max Number of labels, ordered by magnitude. Default is 5.
#'
#' @importFrom ggplot2 layer
#'
#' @examples \dontrun{
#'readr::read_delim("signif.txt", delim = "\t") %>%
#'  eq_clean_data() %>%
#'  dplyr::filter((COUNTRY == "USA" | COUNTRY == "CHINA") & (lubridate::year(DATE) >= 2000 &
#'                                                             lubridate::year(DATE) <= 2015)) %>%
#'  ggplot() +
#'  geom_timeline( aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS)) +
#'  geom_timeline_label( aes(x = DATE, y = COUNTRY, label = LOCATION_NAME,
#'  n_max = 1, size = EQ_PRIMARY)) +
#'  theme_eq
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., nudge_x = 0,
                                nudge_y = 0, na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, n_max){
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = geomTimelineLabel,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}
