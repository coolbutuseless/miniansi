
globalVariables(c('x1', 'y1', 'x2', 'y2', 'ymin', 'ymax'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Make edge information structure from polygon vertex coords
#'
#' @param xs,ys vertex coords
#'
#' @return data.frame
#'
#' @importFrom utils head tail
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_edges <- function(xs, ys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure we have a closed loop
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N <- length(xs)
  if (!(xs[1] == xs[N] && ys[1] == ys[N])) {
    xs <- c(xs, xs[1])
    ys <- c(ys, ys[1])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Basic edge structure: the 2 vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  edges <- data.frame(
    x1 = head(xs, -1),
    y1 = head(ys, -1),
    x2 = tail(xs, -1),
    y2 = tail(ys, -1)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enhanced edge structure
  #  - ymin,ymax the extents of this edge
  #  - x the x coordinate of ymin
  #  - igrad inverse gradient (used to incremeent the x as we step through the
  #          y scanlines)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  edges <- transform(
    edges,
    ymin  = pmin(y1, y2),
    ymax  = pmax(y1, y2),
    x     = ifelse(y2 > y1, x1, x2),
    igrad = (x2 - x1)/(y2 - y1)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # order by increasing ymin
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  edges <- edges[order(edges$ymin),]

  edges
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Internal debuggin: Dump a matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dump_mat <- function(mat) {
  stopifnot(inherits(mat, 'matrix'))
  cat(apply(mat, 1, paste, collapse = ""), sep="\n")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Internal deubbing: Init matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init_mat <- function(ys, xs) {
  nrow <- max(ys)
  ncol <- max(xs)

  mat <- matrix('.', nrow = nrow, ncol = ncol)

  for (i in seq_along(xs)) {
    mat[ys[i], xs[i]] <- 'X'
  }
  mat
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill a polygon
#'
#' @param xs,ys coordinates of vertices (integers)
#' @param self R6 object with a \code{$set_pixels()} method. May be NULL
#' @param fill fill value. usually a character.
#' @param debug debug mode. default: FALSE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_polygon <- function(self, xs, ys, fill, debug = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure we only have integers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xs <- round(as.integer(xs))
  ys <- round(as.integer(ys))

  stopifnot(length(xs) == length(ys))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Internal debugging: Create a viewing matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (debug) {
    mat <- init_mat(ys, xs)
    dump_mat(mat)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set pixels for each of the the vertices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(self)) {
    self$set_pixels(xs, ys, fill)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the edge lists
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_edges <- make_edges(xs, ys)
  active    <- NULL


  if (nrow(all_edges) < 3) {
    return(NULL)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Draw any horizontal edges and remove from list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hedges    <- subset(all_edges, y1 == y2)
  all_edges <- subset(all_edges, y1 != y2)

  for (i in seq_len(nrow(hedges))) {
    hedge_row <- hedges[i, ]
    ys <- hedge_row$y1
    xs <- hedge_row$x1:hedge_row$x2

    if (debug) {
      mat[ys, xs] <- fill
    }

    if (!is.null(self)) {
      self$set_pixels(xs, ys, fill)
    }
  }


  if (debug) dump_mat(mat)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each 'y' scanline
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (yscan in min(all_edges$ymin):max(all_edges$ymax)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add new active edges to 'active', and remove from 'all_edges'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_active <- subset(all_edges, ymin == yscan)
    all_edges  <- subset(all_edges, ymin >  yscan)

    active     <- rbind(active, new_active)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove any active edges which the scanline has passed.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    active <- subset(active, ymax > yscan)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if all edges have been processed
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (nrow(all_edges) == 0 && nrow(active) == 0) {
      break
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Arrange active list by x
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    active <- active[order(active$x),]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For each pair of edges, draw the scanline between them
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (pidx in seq(1, nrow(active), by=2)) {
      x1 <- as.integer(round(active$x[pidx    ]))
      x2 <- as.integer(round(active$x[pidx + 1]))
      ys <- yscan
      xs <- x1:x2

      if (debug) {
        mat[ys, xs] <- fill
      }

      if (!is.null(self)) {
        self$set_pixels(xs, ys, fill)
      }
    }

    if (debug) {
      cat("\n")
      dump_mat(mat)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Increment the x position of the active edges
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    active$x <- active$x + active$igrad

  }

  if (debug) {
    mat
  } else {
    NULL
  }
}




if (FALSE) {
  xs <- c(5, 20, 20)
  ys <- c(5,  2, 10)


  xs <- c(5,  2, 10)
  ys <- c(5, 20, 20)

  # U shape
  xs <- c(5, 10, 10, 15, 15, 20, 20,  5)
  ys <- c(5,  5, 15, 15,  5,  5, 20, 20)


  mat <- polygon_fill(self = NULL, xs, ys, fill = '*')
  dump_mat(mat)
}









