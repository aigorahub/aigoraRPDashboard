#' Spider Plot Position Grid lines
#'
#' @param
#' @return
#' @export
#'
#' @examples
#'
funcCircleCoords <-
  function (center = c(0, 0),
            r = 1,
            npoints = 100)
  {
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }


#' Add Grid lines
#'
#' @param
#' @return
#' @export
#'
#' @examples
#'
add_grid_to_ggradar <- function(p, grid.val) {

  grid.min <- 0
  grid.max <- 150

  centre.y <- grid.min - ((1 / 9) *
                            (grid.max - grid.min))

  add_grid_data <-
    funcCircleCoords(c(0, 0), grid.val + abs(centre.y), npoints = 360)

  gridline.linetype <- 1
  gridline.colour <- "grey"
  grid.line.width <- 0.5
  gridline.label.offset <- -0.1
  grid.label.size <- 4
  font.radar <- font_name

  add_grid_label_data <- data.frame(x = gridline.label.offset,
                                    y = grid.val + abs(centre.y),
                                    text = as.character(grid.val))
  output <- p + ggplot2::geom_path(
    data = add_grid_data,
    ggplot2::aes(x = x, y = y),
    lty = gridline.linetype,
    colour = gridline.colour,
    size = grid.line.width
  ) + ggplot2::geom_text(
    ggplot2::aes(x = x, y = y, label = grid.val),
    data = add_grid_label_data,
    size = grid.label.size * 0.8,
    hjust = 1,
    family = font.radar
  )

  output

}

#' Spider Plot
#'
#' @param radar_plot_data
#' @param  font_name
#' @param font_size
#' @param  background_fill
#'
#' @return
#' @export
#'
#' @examples
#'
make_radar_plot <- function(radar_plot_data,
                            font_name,
                            font_size,
                            background_fill,
                            values.radar = c(0, 75, 150)) {
  # browser()
  radar_plot_data %>%
    # mutate_at (vars ("Mean"), ~ Mean/150) %>%
    dplyr::select(Product, Variables, Mean) %>%
    tidyr::pivot_wider(names_from = Variables, values_from = Mean) %>%
    # mutate(across(where(is.numeric), ~(. - min(.))/(max(.) - min(.)))) %>%
    # mutate(across(where(is.numeric), replace_na, 0.5)) %>%
    ggradar::ggradar(
      font.radar = font_name,
      values.radar = values.radar,
      grid.min = 0,
      grid.mid = values.radar[[round(length(values.radar)/2)]],
      grid.max = values.radar[[length(values.radar)]],
      # centre.y = 0,   # center of plot control
      plot.extent.x.sf = 1,
      plot.extent.y.sf = 1.2,
      grid.label.size = 4,
      group.line.width = 0.8,
      group.point.size = 1.5,
      axis.label.size = 3.5,
      legend.text.size = font_size,
      legend.position = "bottom",
      label.gridline.min = TRUE,
      label.gridline.mid = TRUE,
      label.gridline.max = TRUE,
      background.circle.colour = background_fill,
      gridline.max.linetype = 1,
      gridline.mid.linetype = 1,
      gridline.min.linetype = 1,
      gridline.max.colour = "gray",
      gridline.mid.colour = "gray",
      gridline.min.colour = "gray",
      gridline.label.offset = -0.1,   # Change labels position
      axis.label.offset = 1.15
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::coord_equal(clip = "off")
}

