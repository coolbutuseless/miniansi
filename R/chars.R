

char_tables <- list(
  c('#', '*', 'O', 'X', 'x', 'o', 'a', '-', '+', '~', ':', '.', ' '),
  rev(strsplit(" .:-=+*#%@", '')[[1]]),
  strsplit('$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,"^`\'. ', '')[[1]]
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour to character lookup
#'
#' @param rcolour vector of R colours e.g. 'red', '#123456'
#' @param pow factor for skewing the colour conversion processes
#' @param char_lookup_table character table. default: 1. Choices: 1, 2, 3
#'
#' @return character string. 1 character for each colour to match the intensity
#'         of the given colour
#'
#' @importFrom grDevices col2rgb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2char <- function(rcolour, pow = 1, char_lookup_table = 1) {
  col <- col2rgb(rcolour)
  grey <- (0.3 * col[1,] + 0.59 * col[2,] + 0.11 * col[3,])/255
  grey <- grey ^ pow
  char_lookup <- char_tables[[char_lookup_table]]
  char_lookup[as.integer(grey * (length(char_lookup) - 1) + 1)]
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour matrix to character conversion
#'
#' @param mat matrix of R colours e.g. 'red', '#123456'
#' @param pow factor for skewing the colour conversion processes
#' @param char_lookup_table character table. default: 1. Choices: 1, 2, 3
#'
#' @return Return a matrix as the same size of the input but replaced by a single
#'         character for each colour representing the grey level intensity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colmat2char <- function(mat, pow = 1, char_lookup_table = 1) {
  res <- col2char(mat, pow, char_lookup_table = char_lookup_table)
  dim(res) <- dim(mat)
  res
}

