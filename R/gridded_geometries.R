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
#' @param arc_lengths Numeric vector of possible proportional arc lengths. For example `1/3` will generate 1/3rd of a circle.
#' @param circle_sizes Numeric vector of possible circle sizes (i.e., radius).
#' @param circle_detail Numeric value indicating the detail of the circle; i.e., the number of points to draw.
#' @param complete_arc TRUE/FALSE. Complete arc or not? Complete arcs are more suitable when plotting them as filled polygons, whereas incomplete arcs are more suitable when plotting them as open paths.
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
                               arc_lengths = c((1/3), (1/2), (2/3), 1),
                               circle_sizes = 0.48,
                               circle_detail = 200,
                               complete_arc = TRUE,
                               seed = NULL) {

  # Set seed
  if(!is.null(seed)) {

    set.seed(seed)

  }

  # Create circles and circular arcs
  circles <- purrr::map_dfr(.x = 1:(dimensions[1] * dimensions[2]),
                            #.y = rep(circle_sizes, dimensions[1] * dimensions[2]),
                            .f = ~{

                              # Sample start point
                              start_point <- sample(x = seq(from = 0,
                                                            to = 2 * pi,
                                                            length.out = 1000),
                                                    size = 1)

                              # Sample arc length
                              arc_length <- sample(x = arc_lengths,
                                                   size = 1) * 2 * pi

                              # Calculate end point
                              end_point <- start_point + arc_length

                              # Set id
                              id <- as.character(.x)

                              # Determine coordinates along arc for each circle size
                              purrr::map_dfr(.x = circle_sizes,
                                             .f = ~{

                                               circ <- tibble::tibble(
                                                 id = paste(id, .x, sep = "_"),
                                                 rad = seq(from = start_point,
                                                           to = end_point,
                                                           length.out = circle_detail),
                                                 x = 0.5 + .x * cos(rad),
                                                 y = 0.5 + .x * sin(rad)
                                               )

                                               if(complete_arc) {

                                                 # Add centre point and start point to complete the circular arc
                                                 circ <- circ %>%
                                                   dplyr::add_row(id = paste(id, .x, sep = "_"),
                                                                  rad = NA_real_,
                                                                  x = c(0.5, 0.5 + .x * cos(start_point)),
                                                                  y = c(0.5, 0.5 + .x * sin(start_point)))

                                               }

                                               circ

                                             })

                            })

  # Determine grid positions
  positions <- matrix(data = 1:(dimensions[1] * dimensions[2]), nrow = dimensions[2], ncol = dimensions[1])

  row_col_positions <- purrr::map2_dfr(.x = rep(1:(dimensions[1] * dimensions[2]), each = length(circle_sizes)),
                                       .y = rep(circle_sizes, dimensions[1] * dimensions[2]),
                                       .f = ~{

                                         tibble::tibble(
                                           id = paste(.x, .y, sep = "_"),
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
#' @param geom Which geom to plot the geometries with: "polygon" (default) or "path"?
#' @param h_shapes Character string of geometry type (shape) to highlight.
#' @param h_rotate TRUE/FALSE. Rotate highlighted geometries?
#' @param g_col Character string indicating the colour(s) of the geometries. If multiple colours are provided, the colours are randomly assigned to the geometries.
#' @param bg_col Character string indicating the colour of the plot background.
#' @param h_col Character string indicating the colour of the highlighted geometries.
#' @param ... Other arguments passed on to ggplot2::ggplot()
#'

plot_grid <- function(geometries,
                      geom = "polygon",
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
    {if(geom == "polygon") ggplot2::geom_polygon(mapping = ggplot2::aes(fill = colour_id),
                                                 show.legend = FALSE,
                                                 colour = NA,
                                                 ...) } +
    {if(geom == "path") ggplot2::geom_path(mapping = ggplot2::aes(colour = colour_id),
                                           show.legend = FALSE,
                                           lineend = "round",
                                           ...) } +
    {if(!is.null(h_shapes)) ggplot2::geom_path(mapping = ggplot2::aes(x = x,
                                                                      y = y,
                                                                      group = id),
                                               data = highlighted_shapes,
                                               colour = h_col,
                                               #alpha = 0.5,
                                               size = 1) } +
    {if(geom == "polygon") ggplot2::scale_fill_manual(values = g_col) } +
    {if(geom == "path") ggplot2::scale_colour_manual(values = g_col) } +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = NA))

  return(p)

}

#' Create and plot path arcs and solid circles
#'
#' Function to make and plot a series of differently sized circles and circular arcs that randomly vary in colour, line type, line width and alpha level.
#'
#' @param path_arc_number Number of path arcs to draw.
#' @param path_arc_colours Vector of colours to colour the path arcs.
#' @param solid_arc_colours Vector of colours to colour the solid circles.
#' @param background_fill Character string indicating the colour of the plot background.
#' @param plot_margin Numeric indicating the margin around the circles and arcs.
#' @param alpha_increment Numeric indicating the transparency increment to be added to the path arcs and solid circles. Solid circles are always more transparent than the paths.
#' @param h_col Character string indicating the colour of the highlighted geometries.
#'
#' @examples {
#'
#' # Determine colour palettes
#' col_set1 <- c("#855a5c", "#618985", "#cc8b86", "#7d4f50", "#2e5266", "#5296a5", "#7b886f")
#' col_set2 <- c("#e09f3e", "#9e2a2b", "#0892a5", "#81adc8")
#'
#' # Draw arcs
#' draw_arcs(path_arc_number = 19,
#'           path_arc_colours = col_set1,
#'           solid_arc_colours = col_set2)
#'
#' }
#'

draw_arcs <- function(path_arc_number = 25,
                      path_arc_colours,
                      solid_arc_colours,
                      background_fill = NA,
                      plot_margin = 0.1,
                      alpha_increment = 0) {

  # Create arc paths
  arcs <- purrr::map_dfr(.x = sample(x = seq(0.45, 0.02, length.out = 100), size = path_arc_number),
                         .f = ~{

                           # Sample start point
                           start_point <- sample(x = seq(from = 0,
                                                         to = 2 * pi,
                                                         length.out = 1000),
                                                 size = 1)

                           # Calculate end point
                           end_point <- start_point + ((3/5) * 2 * pi)

                           # Determine coordinates along arc for each arc
                           tibble::tibble(
                             arc_size = .x,
                             id = as.character(.x),
                             rad = seq(from = start_point,
                                       to = end_point,
                                       length.out = 500),
                             x = 0.5 + .x * cos(rad),
                             y = 0.5 + .x * sin(rad)

                           )

                         })

  # Assign colours and line types to arcs
  arc_cols <- path_arc_colours
  line_types <- c("solid", "dotted", "dashed", "solid", "dotdash", "longdash", "twodash")
  line_sizes <- seq(0.5, 2, by = 0.05)
  line_alphas <- seq(0.6, 1, by = 0.05)

  arcs_df <- dplyr::left_join(arcs,
                              tibble::tibble(id = unique(arcs$id),
                                             colour_id = as.character(sample(1:length(arc_cols),
                                                                             size = length(unique(arcs$id)),
                                                                             replace = TRUE)),
                                             line_id = as.character(sample(1:length(line_types),
                                                                           size = length(unique(arcs$id)),
                                                                           replace = TRUE)),
                                             size_id = as.character(sample(1:length(line_sizes),
                                                                           size = length(unique(arcs$id)),
                                                                           replace = TRUE)),
                                             alpha_id = as.character(sample(1:length(line_alphas),
                                                                            size = length(unique(arcs$id)),
                                                                            replace = TRUE))),
                              by = "id")

  # Create solid arcs
  solid_cols <- sample(solid_arc_colours, 4, replace = FALSE)
  solid_start <- sample(x = seq(from = 0,
                                to = 2 * pi,
                                length.out = 1000),
                        size = 1)

  solid_arcs <- tibble::tibble(
    rad = seq(from = 0, to = 2 * pi, length.out = 500),
    id = "0.52",
    x = 0.5 + as.numeric(id) * cos(rad),
    y = 0.5 + as.numeric(id) * sin(rad),
    fill = "1"
  ) %>%
    dplyr::add_row(rad = seq(from = solid_start, to = solid_start + pi, length.out = 500),
                   id = "0.42",
                   x = 0.5 + as.numeric(id) * cos(rad),
                   y = 0.5 + as.numeric(id) * sin(rad),
                   fill = "2") %>%
    dplyr::add_row(rad = seq(from = solid_start + pi, to = solid_start + (2 * pi), length.out = 500),
                   id = "0.34",
                   x = 0.5 + as.numeric(id) * cos(rad),
                   y = 0.5 + as.numeric(id) * sin(rad),
                   fill = "3") %>%
    dplyr::add_row(rad = seq(from = solid_start, to = solid_start + pi, length.out = 500),
                   id = "0.26",
                   x = 0.5 + as.numeric(id) * cos(rad),
                   y = 0.5 + as.numeric(id) * sin(rad),
                   fill = "4")


  p <- ggplot2::ggplot(data = arcs_df,
                       mapping = ggplot2::aes(x = x,
                                              y = y,
                                              group = id)) +
    ggplot2::geom_polygon(data = solid_arcs[solid_arcs$fill == "1",],
                          mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 group = id,
                                                 fill = fill),
                          colour = NA,
                          alpha = 0.1 + alpha_increment,
                          show.legend = FALSE) +
    ggplot2::geom_polygon(data = solid_arcs[solid_arcs$fill != "1",],
                          mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 group = id,
                                                 fill = fill),
                          colour = NA,
                          alpha = 0.35 + alpha_increment,
                          show.legend = FALSE) +
    ggplot2::geom_path(mapping = ggplot2::aes(colour = colour_id,
                                              linetype = line_id,
                                              linewidth = size_id,
                                              alpha = alpha_id),
                       lineend = "round",
                       show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = solid_cols) +
    ggplot2::scale_colour_manual(values = arc_cols) +
    ggplot2::scale_linetype_manual(values = line_types) +
    ggplot2::scale_linewidth_manual(values = line_sizes) +
    ggplot2::scale_alpha_manual(values = line_alphas) +
    ggplot2::coord_equal(xlim = c(0 - plot_margin, 1 + plot_margin),
                         ylim = c(0 - plot_margin, 1 + plot_margin)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_fill,
                                                           colour = NA))

  return(p)

}
