# Gridded geometry

#' Make grid of geometries
#'
#' Function to make a grid of randomly inserted geometries
#'
#' @param geometries A list of the input geometries, each of which is a data frame with:
#'     * "x": x coordinate of the geometry's vertices.
#'     * "y": y coordinate of the geometry's vertices.
#'     * "s": the length of the edges between the vertices.
#' @param dimensions Vector of dimensions of the grid in number of geometries per side. For example, `c(4, 4)` will create a 4x4 grid.
#' @param seed Set seed to random number generator. Default: NULL.
#'
#' @return A data frame with geometry grid coordinates
#'
#' @examples {
#' # Create square
#' square <-  tibble::tibble(
#'   x = c(0, 1, 1, 0, 0),
#'   y = c(0, 0, 1, 1, 0),
#'   s = c(1, 1, 1, 1, 0)
#' )
#'
#' # Create triangle
#' triangle <- tibble::tibble(
#'   x = c(0, 1, 0, 0),
#'   y = c(0, 0, 1, 0),
#'   s = c(1, 1, sqrt(2), 0),
#' )
#'
#' geometries <- list(square, triangle)
#'
#' ggrid <- create_geometry_grid(geometries, c(3, 3))
#'
#' }
#'

create_geometry_grid <- function(geometries,
                                 dimensions,
                                 seed = NULL) {

  # Set seed
  if(!is.null(seed)) {

    set.seed(seed)

  }

  # Sample geometries from the list of options
  sampled_geometries <- sample(1:length(geometries), size = dimensions[1] * dimensions[2], replace = T)

  # Determine grid positions
  positions <- matrix(data = 1:(dimensions[1] * dimensions[2]), nrow = dimensions[2], ncol = dimensions[1])

  row_col_positions <- purrr::map_dfr(.x = 1:(dimensions[1] * dimensions[2]),
                                      .f = ~{

                                        tibble::tibble(
                                          id = as.character(.x),
                                          row = which(positions == .x, arr.ind = TRUE)[1],
                                          col = which(positions == .x, arr.ind = TRUE)[2]
                                        )

                                      })

  # Store names of shapes
  shape_names <- tibble::tibble(
    id = as.character(1:length(sampled_geometries)),
    name = names(geometries[sampled_geometries])
  )

  # Convert geometry coordinates to fit with their right grid position
  gridded_geometries <- dplyr::bind_rows(unname(geometries)[sampled_geometries], .id = "id") %>%
    dplyr::left_join(row_col_positions, by = "id") %>%
    dplyr::left_join(shape_names, by = "id") %>%
    dplyr::mutate(x = .data$x + .data$col - 1,
                  y = .data$y - .data$row - 1)

  return(gridded_geometries)

}

#' Make grid of circular arcs
#'
#' Function to make a grid of semi-circles, full circles, and circular arcs of various size
#'
#' @param dimensions Vector of dimensions of the grid in number of geometries per side. For example, `c(4, 4)` will create a 4x4 grid.
#' @param seed Set seed to random number generator. Default: NULL.
#'
#' @return A data frame with circle grid coordinates
#'
#' @examples {
#'
#' cgrid <- create_circle_grid(c(9, 9))
#'
#' }
#'

create_circle_grid <- function(dimensions,
                               seed = NULL) {

  # Set seed
  if(!is.null(seed)) {

    set.seed(seed)

  }

  # Create circles and circular arcs
  circles <- purrr::map_dfr(.x = 1:(dimensions[1] * dimensions[2]),
                            .f = ~{

                              # Sample start point
                              start_point <- sample(x = seq(from = 0,
                                                            to = 2 * pi,
                                                            length.out = 1000),
                                                    size = 1)

                              # Sample arc length
                              arc_length <- sample(x = c((1/3), (1/2), (2/3), 1),
                                                   size = 1) * 2 * pi

                              # Calculate end point
                              end_point <- start_point + arc_length

                              # Determine 200 coordinates along arc
                              tibble::tibble(
                                id = as.character(.x),
                                rad = seq(start_point, end_point, length.out = 200),
                                x = 0.5 + 0.48 * cos(rad),
                                y = 0.5 + 0.48 * sin(rad)
                              ) %>%
                                # Add centre point and start point to complete the circular arc
                                dplyr::add_row(id = as.character(.x),
                                               rad = NA_real_,
                                               x = c(0.5, 0.5 + 0.48 * cos(start_point)),
                                               y = c(0.5, 0.5 + 0.48 * sin(start_point)))

                            })

  # Determine grid positions
  positions <- matrix(data = 1:(dimensions[1] * dimensions[2]), nrow = dimensions[2], ncol = dimensions[1])

  row_col_positions <- purrr::map_dfr(.x = 1:(dimensions[1] * dimensions[2]),
                                      .f = ~{

                                        tibble::tibble(
                                          id = as.character(.x),
                                          row = which(positions == .x, arr.ind = TRUE)[1],
                                          col = which(positions == .x, arr.ind = TRUE)[2]
                                        )

                                      })

  # Convert circle coordinates to fit with their right grid position
  gridded_circles <- circles %>%
    dplyr::left_join(row_col_positions, by = "id") %>%
    dplyr::mutate(x = .data$x + .data$col - 1,
                  y = .data$y - .data$row - 1)

  return(gridded_circles)

}

#' Plot grid
#'
#' Display the gridded geometries
#'
#' @param geometries A data frame of geometries in a grid
#'     * "x": x coordinate of the geometry's vertices.
#'     * "y": y coordinate of the geometry's vertices.
#'     * "s": the length of the edges between the vertices.
#'     * "id": the id of the geometry.
#'     * "row": the row in which the geometry is positioned.
#'     * "col" the column in which the geometry is positioned.
#' @param h_shapes Character string of geometry type (shape) to highlight.
#' @param h_rotate TRUE/FALSE. Rotate highlighted geometries?
#' @param g_col Character string indicating the colour(s) of the geometries. If multiple colours are provided, the colours are randomly assigned to the geometries.
#' @param bg_col Character string indicating the colour of the plot background.
#' @param h_col Character string indicating the colour of the highlighted geometries.
#' @param ... Other arguments passed on to ggplot2::ggplot()
#'

plot_grid <- function(geometries,
                      h_shapes = NULL,
                      h_rotate = FALSE,
                      g_col,
                      bg_col,
                      h_col,
                      ...) {

  # Assign colours to geometries
  colours <- tibble::tibble(id = unique(geometries$id),
                            colour_id = as.character(sample(1:length(g_col), size = length(unique(geometries$id)), replace = TRUE)))

  geometries <- geometries %>%
    dplyr::left_join(colours, by = "id")

  # Selected shapes to highlight [optional]
  if(!is.null(h_shapes)) {

    highlighted_shapes <- geometries %>%
      dplyr::filter(name %in% {{h_shapes}})

    # Rotate highlighted shapes
    if(h_rotate) {

      highlighted_shapes <- highlighted_shapes %>%
        dplyr::group_by(.data$id) %>%
        # dplyr::mutate(dplyr::across(.cols = c("x", "y"),
        #                             .fns = ~{
        #
        #                               dplyr::case_when(.x == min(.x) ~ .x + 0.2,
        #                                                .x == max(.x) ~ .x - (.data$s * 0.1))
        #
        #                             })) %>%
        dplyr::mutate(rads = (-runif(1, -20, 20))*pi/180, # rotation in radians
                      x_or = (max(.data$x) + min(.data$x)) / 2, # original centre x
                      y_or = (max(.data$y) + min(.data$y)) / 2, # original centre y
                      x = ((.data$x - .data$x_or) * cos(.data$rads)) - ((.data$y - .data$y_or) * sin(.data$rads)) + .data$x_or,
                      y = ((.data$y - .data$y_or) * cos(.data$rads)) + ((.data$x - .data$x_or) * sin(.data$rads)) + .data$y_or)

    }

  }

  # Plot
  p <- ggplot2::ggplot(data = geometries,
                       mapping = ggplot2::aes(x = x,
                                              y = y,
                                              group = id)) +
    ggplot2::geom_polygon(colour = NA,
                          mapping = ggplot2::aes(fill = colour_id),
                          show.legend = FALSE,
                          ...) +
    {if(!is.null(h_shapes)) ggplot2::geom_path(mapping = ggplot2::aes(x = x,
                                                                         y = y,
                                                                         group = id),
                                                  data = highlighted_shapes,
                                                  colour = h_col,
                                                  #alpha = 0.5,
                                                  size = 1) } +
    ggplot2::scale_fill_manual(values = g_col) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = NA))

  return(p)

}
