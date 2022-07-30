# Polygon deformation
# Credits to danielle navarro (https://art-from-code.netlify.app/)

#' Insert new vertex into polygons' edges
#'
#' Function to deform polygons by inserting new vertices and splitting edges
#'
#' @param polygon A list of vertices, each of which is a list of at least three items:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#'     * "s": the length of the edges between the vertices.
#' @param noise A numeric value to add noise to the location along the edge where the new vertex will be inserted.
#'
#' @return A list of vertices of the new polygon
#'
#' @examples {
#'
#' # Create polygon
#' # Note: add the origin twice, to open and close the polygon
#' square <-  tibble::tibble(
#'  x = c(0, 1, 1, 0, 0),
#'  y = c(0, 0, 1, 1, 0),
#'  s = c(1, 1, 1, 1, 0)
#' )
#'
#' new_square <- insert_vertex(square)
#'
#' }

insert_vertex <- function(polygon,
                          noise) {

  # Sample a vertex
  vertex_id <- sample_vertex(polygon)

  # Get length of selected vertex
  vertex_length <- polygon[[vertex_id]]$s

  # Determine coordinates of previous and next vertices
  prev_x <- polygon[[vertex_id]]$x
  prev_y <- polygon[[vertex_id]]$y

  next_x <- polygon[[vertex_id + 1]]$x
  next_y <- polygon[[vertex_id + 1]]$y

  # Determine location of the new vertex
  # Add noise, proportional to the length of the original edge

  new_x <- (prev_x + next_x) / 2 + runif(n = 1,
                                         min = -(vertex_length * noise) / 2,
                                         max = (vertex_length * noise) / 2)

  new_y <- (prev_y + next_y) / 2 + runif(n = 1,
                                         min = -(vertex_length * noise) / 2,
                                         max = (vertex_length * noise) / 2)

  # Add new vertex
  new_vertex <- list(
    x = new_x,
    y = new_y,
    s = calculate_edge_length(x = c(new_x, next_x),
                              y = c(new_y, next_y))
  )

  # Update original edge length
  polygon[[vertex_id]]$s <- calculate_edge_length(x = c(prev_x, new_x),
                                                  y = c(prev_y, new_y))

  # New polygon
  new_polygon <- c(
    polygon[1:vertex_id],
    list(new_vertex),
    polygon[-(1:vertex_id)]
  )

  return(new_polygon)

}

#' Sample a polygon's vertex
#'
#' Function to select a random vertex of a polygon. This function can be used to deform the polygon; for instance, by inserting a new vertices.
#'
#' @param polygon A list of vertices, each of which is a list of at least three items:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#'     * "s": the length of the edges between the vertices.
#'
#' @return Row number of the vertex
#'

sample_vertex <- function(polygon) {

  sample(x = length(polygon),
         size = 1,
         prob = purrr::map_dbl(.x = polygon,
                               .f = ~.x$s))

}

#' Calculate edge length
#'
#' Function to calculate the length of a new edge.
#'
#' @param x A numeric vector of the x coordinates of the edge's two adjacent vertices.
#' @param y A numeric vector of the y coordinates of the edge's two adjacent vertices.
#'
#' @return Edge length, a numeric vector
#'

calculate_edge_length <- function(x,
                                  y) {

  sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)

}


#' Evolve polygon
#'
#' Function to evolve a polygon by deforming it iteratively, inserting new vertices and edges along the way.
#'
#' @param polygon A list of vertices, each of which is a list of at least three items:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#'     * "s": the length of the edges between the vertices.
#' @param iterations An integer value that determines the number of times we repeat the process of polygon deformation.
#' @param noise A numeric value, which adds noise to the location along the edge where the new vertex will be inserted.
#' @param seed Set seed to random number generator. Default: NULL.
#'
#' @return A tibble of the x and y coordinates of the new polygon
#'

evolve_polygon <- function(polygon,
                           iterations,
                           noise,
                           seed = NULL) {

  # Set seed
  if(!is.null(seed)) {

    set.seed(seed)

  }

  # Evolve polygon
  for(i in 1:iterations) {

    polygon <- insert_vertex(polygon, noise)

  }

  evolved_polygon <- polygon %>%
    purrr::transpose() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(.fn = unlist))

  return(evolved_polygon)

}

#' Evolve multi-polygon
#'
#' Function to evolve a multi-polygon by deforming it iteratively, inserting new vertices and edges along the way.
#'
#' @param polygon A list of vertices, each of which is a list of at least three items:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#'     * "s": the length of the edges between the vertices.
#' @param n An integer value that determines the number of polygons that go through the process of of polygon deformation.
#' @param progess_bar TRUE/FALSE. Add a progress bar to track the evolution of the multi-polygon?
#' @param seed Set seed to random number generator. Default: NULL.
#' @param ... Other arguments passed on to `evolve_polygon()`
#'
#' @return A tibble of the x and y coordinates of the new polygon
#'

evolve_multipolygon <- function(polygon,
                                n,
                                progress_bar = TRUE,
                                seed = NULL,
                                ...) {

  # Set seed
  if(!is.null(seed)) {

    set.seed(seed)

  }

  # Create progress bar
  if(progress_bar) {

    pb <- progress::progress_bar$new(total = n)

  }


  # Evolve multi-polygon
  multi_polygons <- list()

  for(i in 1:n) {

    if(progress_bar) {

      pb$tick()

    }

    multi_polygons[[i]] <- evolve_polygon(polygon, ...)

  }

  multi_polygons <- dplyr::bind_rows(multi_polygons, .id = "id")

  # multi_polygons <-  purrr::map(.x = 1:n,
  #                               iterations = params$iterations,
  #                               noise = params$noise,
  #                               .f = ~{
  #
  #                                 if(progress_bar) {
  #
  #                                   pb$tick()
  #
  #                                 }
  #
  #                                 polygon %>%
  #                                   evolve_polygon(...)
  #
  #                               }) %>%
  #   dplyr::bind_rows(.id = "id")

 return(multi_polygons)

}

#' Plot polygon
#'
#' Display the polygon minimally: white edges (and vertices) on a dark grey background
#'
#' @param polygon A data frame or tibble with at least two variables:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#' @param show_vertices TRUE or FALSE, show vertices or not?
#' @param v_col Character string indicating the colour of the vertices
#' @param e_col Character string indicating the colour of the edges
#' @param bg_col Character string indicating the colour of the plot background
#' @param v_size Numeric value indicating the size of the vertices
#' @param ... Other arguments passed on to ggplot2::ggplot()
#'

show_polygon <- function(polygon,
                         show_vertices = TRUE,
                         v_col = "white",
                         e_col = "white",
                         bg_col = "grey10",
                         v_size = 2,
                         ...) {

  p <- ggplot2::ggplot(data = polygon,
                  mapping = ggplot2::aes(x = x,
                                         y = y)) +
    ggplot2::geom_polygon(colour = e_col,
                          fill = NA,
                          ...) +
    {if(show_vertices) ggplot2::geom_point(colour = v_col,
                                           size = v_size) } +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col, colour = NA))

  return(p)

}


#' Plot multipolygon
#'
#' Display the multipolygon
#'
#' @param polygon A data frame or tibble with at least two variables:
#'     * "x": x coordinate of the polygon's vertices
#'     * "y": y coordinate of the polygon's vertices
#'     * "id": unique identifier of each polygon, used as a grouping variable in ggplot2::ggplot()
#' @param fill Character string indicating the colour of the polygons
#' @param alpha Numeric value between [0,1] indicating the transparency of the polygons
#' @param bg_col Character string indicating the colour of the plot background
#' @param ... Other arguments passed on to ggplot2::ggplot()
#'

show_multipolygon <- function(polygon,
                              fill = "white",
                              alpha = 0.1,
                              bg_col = "grey7",
                              ...) {

  p <-  ggplot2::ggplot(data = polygon,
                        mapping = ggplot2::aes(x = x, y = y, group = id)) +
    ggplot2::geom_polygon(colour = NA,
                          alpha = alpha,
                          fill = fill,
                          ...) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = NA))

  return(p)

}
