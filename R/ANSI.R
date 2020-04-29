

"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an R colour to an internal colour representation.
#'
#' Currently hex colour with alpha
#'
#' @param colour string e.g. 'red'. '#234567'
#'
#' @return internal representation of colour
#'
#' @importFrom grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_R_colour_to_internal_colour <- function(colour) {
  rgba <- col2rgb(colour, alpha = TRUE)
  hex  <- rgb(rgba[1], rgba[2], rgba[3], rgba[4], maxColorValue = 255)
  hex
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ANSI R6 class
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ANSI <- R6::R6Class(
  "ANSI",

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field width,height dimensions
    #' @field bg,fg,chr matrices representing background, foreground and characters
    #' @field font_aspect font aspect ratio
    #' @field bg_col,text_colour initial colours
    #' @field ansi_bits bit depth for ANSI representation.
    #' @field max_height maximum height value which will plotted given the current
    #'        font_aspect
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    width       = NULL,
    height      = NULL,
    bg          = NULL,
    fg          = NULL,
    chr         = NULL,
    font_aspect = NULL,
    bg_col      = NULL,
    text_colour = NULL,
    ansi_bits   = NULL,
    max_height  = NULL,

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description  Initialise object
    #'
    #' @param width,height dimensions
    #' @param background,text_colour initial colours
    #' @param ansi_bits bit depth for ANSI representation. Possible values: 8, 24. Default:8
    #' @param font_aspect font aspect ratio. default: 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(width = 20, height = 20, background = 'white',
                          text_colour = 'black',
                          ansi_bits = 8, font_aspect = 1) {
      self$width       <- width
      self$height      <- height
      self$max_height  <- as.integer(round(height / font_aspect))
      self$font_aspect <- font_aspect
      self$bg_col      <- background
      self$text_colour <- text_colour
      self$ansi_bits   <- ansi_bits

      self$reset()

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Reset to a blank canvas
    #'
    #' @param background,text_colour defaut bg/fg colours. default: NULL (use the colour set
    #'        when initialised)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    reset = function(background = NULL, text_colour = NULL) {
      bg_col      <- background %||% self$bg_col
      text_colour <- text_colour %||% self$text_colour

      bg_col      <- convert_R_colour_to_internal_colour(bg_col)
      text_colour <- convert_R_colour_to_internal_colour(text_colour)

      self$bg  <- matrix(bg_col      , nrow = self$height, ncol = self$width)
      self$fg  <- matrix(text_colour , nrow = self$height, ncol = self$width)
      self$chr <- matrix(' '         , nrow = self$height, ncol = self$width)

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Set pixels in the bg
    #'
    #' @param xs,ys vectors of integer coordinates
    #' @param bg,fg,chr values to set.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_pixels = function(xs, ys, bg = NULL, fg = NULL, chr = NULL) {


      if (length(xs) == 1 && length(ys) > 1) {
        xs <- rep(xs, length(ys))
      }
      if (length(ys) == 1 && length(xs) > 1) {
        ys <- rep(ys, length(xs))
      }

      stopifnot(length(xs) == length(ys))


      xs <- as.integer(round(xs))
      ys <- as.integer(round(ys * self$font_aspect))

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Be extra paranoid about plotting paste extents
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      bad_ii <- xs < 1 | xs > self$width | ys < 1 | ys > self$height

      xs <- xs[!bad_ii]
      ys <- ys[!bad_ii]

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Set all values
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      idx <- cbind(ys, xs)

      if (!is.null(bg) && !is.na(bg)) {
        bg <- convert_R_colour_to_internal_colour(bg)
        if (!endsWith(bg, '00')) {
          self$bg[idx] <- bg
        }
      }

      if (!is.null(fg) && !is.na(fg)) {
        fg <- convert_R_colour_to_internal_colour(fg)
        if (!endsWith(fg, '00')) {
          self$fg[idx] <- fg
        }
      }

      if (!is.null(chr) && !is.na(chr)) {
        self$chr[idx] <- chr
      }


      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a line
    #'
    #' @param x1,y1,x2,y2 line extents
    #' @param colour colour to set
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    line = function(x1, y1,  x2,  y2, colour) {

      xdelta <- abs(x2 - x1)
      ydelta <- abs(y2 - y1)

      if (xdelta > ydelta) {
        x <- x1:x2
        y <- seq(y1, y2, length.out = length(x))
      } else {
        y <- y1:y2
        x <- seq(x1, x2, length.out = length(y))
      }

      self$set_pixels(xs = x, ys = y, bg = colour)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a filled rectangle
    #'
    #' @param x1,y1,x2,y2 rect extents
    #' @param colour colour to set
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect_filled = function(x1, y1, x2, y2, colour) {

      coords <- expand.grid(x1:x2, y1:y2)

      self$set_pixels(xs = coords[,1], ys = coords[,2], bg = colour)

      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a rectangular outline
    #'
    #' @param x1,y1,x2,y2 rect extents
    #' @param colour colour to set
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect_outline = function(x1, y1, x2, y2, colour) {

      x1 <- as.integer(round(x1))
      x2 <- as.integer(round(x2))
      y1 <- as.integer(round(y1))
      y2 <- as.integer(round(y2))

      ysteps <- abs(y1 - y2) + 1
      xsteps <- abs(x1 - x2) + 1

      self$set_pixels(xs = rep(x1, ysteps), ys = y1:y2          , bg = colour)
      self$set_pixels(xs = rep(x2, ysteps), ys = y1:y2          , bg = colour)
      self$set_pixels(xs = x1:x2          , ys = rep(y1, xsteps), bg = colour)
      self$set_pixels(xs = x1:x2          , ys = rep(y2, xsteps), bg = colour)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a polyline
    #'
    #' @param xs,ys points around polygon
    #' @param colour colour
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polyline = function(xs, ys, colour) {

      stopifnot(length(xs) == length(ys))
      N <- length(xs)

      for (i in seq_len(N - 1L)) {
        self$line(xs[i], ys[i], xs[i+1], ys[i+1], colour)
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a polygon
    #'
    #' @param xs,ys points around polygon
    #' @param colour,fill colour for outline an fill
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon = function(xs, ys, colour, fill) {

      stopifnot(length(xs) == length(ys))
      N <- length(xs)

      # Ensure polygon is closed
      if (!(xs[1] == xs[N] && ys[1] == ys[N])) {
        xs <- c(xs, xs[1])
        ys <- c(ys, ys[1])
        N  <- N + 1L
      }

      # Check if the fill colour is invisible, so we could avoid doing the
      # expensive polygon fill calculation
      ifill <- convert_R_colour_to_internal_colour(fill)
      if (is.na(fill) || endsWith(ifill, '00')) {
        # skip the fill operation
      } else {
        fill_polygon(self, xs, ys, ifill)
      }


      # Draw the outline
      self$polyline(xs, ys, colour)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a stroked/filled rectangle
    #'
    #' @param x1,y1,x2,y2 rect extents
    #' @param colour colour to set
    #' @param fill colour to set
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect = function(x1, y1, x2, y2, colour, fill) {
      self$rect_filled (x1, y1, x2, y2, fill)
      self$rect_outline(x1, y1, x2, y2, colour)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Draw a stroked/filled rectangle
    #'
    #' Reference: \url{http://members.chello.at/~easyfilter/bresenham.html}
    #'
    #' @param xc,yc circle centre
    #' @param r radius
    #' @param colour colour to set
    #' @param fill colour to set
    #' @param font_aspect font_aspect override to have circles plot as circles
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    circle = function(xc, yc, r, colour, fill, font_aspect = 0.45) {

      rorig <- r

      font_distort <- font_aspect / self$font_aspect

      if (r <= 1.5) {
        self$set_pixels(xc, yc, bg = icolour)
        return(invisible(self))
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Fill the centre.
      # split the process of centre fill and border drawing, as rounding
      # will mean that sometimes one overwrites the other.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x <- -r
      y <- 0
      err <- 2 - 2*r
      while (x < 0) {
        self$line(xc + x, yc + y * font_distort,  xc - x,  yc + y * font_distort, fill)
        self$line(xc + x, yc - y * font_distort,  xc - x,  yc - y * font_distort, fill)

        r <- err
        if (r <= y) {
          y   <- y + 1
          err <- err + y * 2 + 1
        }
        if (r > x || err > y) {
          x <- x + 1
          err <- err + x * 2 + 1
        }
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Draw the border
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      r <- rorig
      x <- -r
      y <- 0
      err <- 2 - 2*r
      while (x < 0) {
        self$set_pixels(xc - x, yc + y * font_distort, bg = colour);   #    I. Quadrant
        self$set_pixels(xc - y, yc - x * font_distort, bg = colour);   #   II. Quadrant
        self$set_pixels(xc + x, yc - y * font_distort, bg = colour);   #  III. Quadrant
        self$set_pixels(xc + y, yc + x * font_distort, bg = colour);   #   IV. Quadrant

        r <- err
        if (r <= y) {
          y   <- y + 1
          err <- err + y * 2 + 1
        }
        if (r > x || err > y) {
          x <- x + 1
          err <- err + x * 2 + 1
        }
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add text
    #'
    #' @param x,y location
    #' @param text string
    #' @param angle currently only supports 0 or 90.
    #' @param colour Text colour
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text = function(x, y, text, colour = 'black', angle = 0) {

      n   <- nchar(text)
      str <- strsplit(text, '')[[1]]

      if (x == 0) { x <- 1 }
      if (y == 0) { y <- 1 }

      if (angle == 0) {
        for (i in seq_along(str)) {
          xi <- x + i - 1
          if (xi < 1 || xi > self$width) { next }
          self$set_pixels(xi, y, fg = colour, chr = str[i])
        }
      } else {
        for (i in seq_along(str)) {
          yi <- y + i - 1
          if (yi < 1 || yi > self$width) { next }
          self$set_pixels(x, yi, fg = colour, chr = str[i])
        }
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Character representation in ASCII onlyl
    #'
    #' @param pow factor for non-linear intensity mapping
    #' @param char_lookup_table character table. default: 1. Choices: 1, 2, 3
    #'
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character_ascii = function(pow = 1, char_lookup_table = 1, ...) {
      mat <- colmat2char(self$bg, pow = pow, char_lookup_table = char_lookup_table)

      idx <- self$chr != ' '
      mat[idx] <- self$chr[idx]

      collapse_matrix_to_string(mat)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Character representation with ANSI colouring
    #'
    #' @param plain_ascii Map colour intensity to ASCII characters only. No
    #'        ANSI output.  Default: FALSE
    #' @param pow factor for non-linear intensity mapping. Only used if
    #'        \code{plain_ascii=TRUE}
    #' @param char_lookup_table character table. default: 1. Choices: 1, 2, 3 Only used if
    #'        \code{plain_ascii=TRUE}
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(plain_ascii = FALSE, pow = 1, char_lookup_table = 1, ...) {

      if (plain_ascii) {
        return(self$as_character_ascii(pow = pow, char_lookup_table = char_lookup_table, ...))
      }

      # Find dark background and set the corresponding foreground to white
      # instead of the default black
      intensity <- rowMeans(t(col2rgb(self$bg)))
      intensity <- apply(t(col2rgb(self$bg)), 1, function(x) {
        (0.3 * x[1] + 0.59*x[2] + 0.11 * x[3])/255
      })
      chr <- self$chr
      idx <- intensity < 0.4 & chr != ' '
      chr[idx] <- paste0(col2fg('white'), chr[idx], reset_code)

      # smash together the bg, fg and text layers
      if (self$ansi_bits == 24) {
        bg <- col2bg24(self$bg)
        fg <- col2fg24(self$fg)
      } else {
        bg <- col2bg(self$bg)
        fg <- col2fg(self$fg)
      }
      mat <- paste0(bg, fg, self$chr)

      # Make sure the output matrix is the correct size
      dim(mat) <- dim(self$bg)

      # return a single string
      collapse_matrix_to_string(mat, row_end = reset_code)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save to file
    #'
    #' @param filename filename
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename, ...) {
      writeLines(self$as_character(), filename)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Output to screen
    #'
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...), "\n")

      invisible(self)
    }
  )

)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Collapse a character matrix to a string representation
#'
#' @param mat matrix
#' @param row_end character with which to end each row
#'
#' @return single string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collapse_matrix_to_string <- function(mat, row_end = '') {
  res <- apply(mat, 1, paste, collapse = "")
  res <- paste0(res, row_end)
  res <- paste(res, collapse = "\n")
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Character representation of an ANSI object
#'
#' @param x ANSI object
#' @param ... other arguments passed on
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.ANSI <- function(x, ...) {
  x$as_character(...)
}




if (FALSE) {

  ansi <- ANSI$new(width = 80, height = 20, background = 'grey45', text_colour = 'hotpink')

  ansi$polygon(c(10, 10, 20), c(10, 20, 20), colour='red', fill = 'white')

  ansi

  xs <- c(5, 10, 10, 15, 15, 20, 20,  5)
  ys <- c(5,  5, 15, 15,  5,  5, 20, 20)

  ansi$polygon(xs, ys, colour='green', fill = 'hotpink')
  ansi


  # ansi$rect_outline(5, 5, 15, 15, colour = 'red', fill = 'grey100')
  ansi$rect_outline(5, 5, 15, 15, colour = 'red')
  ansi

  ansi$set_pixels(1:5, 1:5, bg = 'yellow')
  ansi

  ansi$line(10, 0, 0, 10, 'green')
  ansi

  ansi$circle(20, 10, r = 14, colour = 'red', fill = 'black')
  ansi

  ansi$text(40, 10, "Hello", angle = 0)
  # system.time({
  #   print(ansi)
  # })

  ansi


  ansi$save("crap.txt")
}










