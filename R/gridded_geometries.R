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
#' @param h_jitter TRUE/FALSE. Jitter highlighted geometries?
#' @param g_col Character string indicating the colour(s) of the geometries. If multiple colours are provided, the colours are randomly assigned to the geometries.
#' @param bg_col Character string indicating the colour of the plot background.
#' @param h_col Character string indicating the colour of the highlighted geometries.
#' @param ... Other arguments passed on to ggplot2::ggplot()
#'

plot_grid <- function(geometries,
                      h_shapes = NULL,
                      h_jitter = FALSE,
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

    # Jitter highlighted shapes
    if(h_jitter) {

      highlighted_shapes <- highlighted_shapes %>%
        #dplyr::group_by(.data$id) %>%
        dplyr::mutate(x = x + runif(1, -0.25, 0.25),
                    y = y + runif(1, -0.25, 0.25))

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
    {if(!is.null(h_shapes)) ggplot2::geom_polygon(mapping = ggplot2::aes(x = x,
                                                                         y = y,
                                                                         group = id),
                                                  data = highlighted_shapes,
                                                  colour = h_col,
                                                  fill = NA,
                                                  alpha = 0.5,
                                                  size = 1) } +
    ggplot2::scale_fill_manual(values = g_col) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = NA))

  return(p)

}
