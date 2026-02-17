#' Package of my utility functions
#'
#' @name myutils
NULL


# ***********************************
# IO Clipboard ----
# ***********************************

#' Read clipboard as a data frame by using `utils::read.table()`
#'
#' @param header If TRUE, retrieved data contains a header
#' @return A data frame containing clipboard data
#'
#' @export
#' @md
rcbmat <- function(header, ...) {
  if (missing(header)) { header = FALSE }
  utils::read.table(file="clipboard", header=header, ...)
}

#' Copy table data to clipboard with `utils::write.table()`
#'
#' @param data data.frame or matrix
#' @param header xx
#' @param sep the field separator string
#' @param size clipboard size
#' @param row.names yy
#'
#' @export
#' @md
wcbmat <- function(data, header, sep="\t", size=128, row.names=FALSE, qmethod="double",
                  col.names=ifelse(header && row.names, NA, header), ...) {
  if (missing(header)) { header = FALSE }

  if (typeof(size) != "double" && typeof(size) != "integer") {
    stop("type of 'size' must be 'double' or 'integer'.")
  }
  fn = sprintf("clipboard-%d", size)

  utils::write.table(data, file=fn, sep=sep, row.names=row.names,
                     col.names=col.names, qmethod=qmethod, ...)
}


# Plot ----

core_col32 <- function(col, alpha) {
  rgb_ = col2rgb(col, alpha=TRUE)
  ifelse(alpha >= 0,
         rgb(rgb_[1], rgb_[2], rgb_[3], alpha, maxColorValue=255),
         rgb(rgb_[1], rgb_[2], rgb_[3], maxColorValue=255))
}

#' Add alpha level to color
#' @param cols   vector of color name or hex (e.g. red, #123456)
#' @param alphas vector of alpha levels (from 0 to 255) [optional]
#' @return       vector of color codes
#' @export
col32 <- function(cols, alphas) {
  if (missing(alphas)) { alphas = -1 }
  mapply(myutils:::core_col32, cols, alphas, USE.NAMES=FALSE)
}

#' Add a error bar
#' @param x0,y0  coordinates of points from which to draw
#' @param x0,y1  coordinates of points to which to draw. At least one must the supplied
#' @param col    color of error bar
#' @param length length of the edges of the error bar (in inches)
p_errorBar <- function(x0, y0, x1, y1, col="black", length=0.1) {
  arrows(x0, y0, x1, y1, angle=90, code=3, length=length, col=col)
}

#' Add a error bar along X axis
#' @param x,y coordinates of the center point of the error bar
#' @param err value of error
#' @param col    color of error bar
#' @param length length of the edges of the error bar (in inches)
#' @seealso [p_errorBar()]
#' @export
p_errorBarX <- function(x, y, err, col="black", length=0.1) {
  myutils:::errorBar(x-err, y, x+err, y, col, length)
}

#' Add a error bar along Y axis
#' @param x,y coordinates of the center point of the error bar
#' @param err value of error
#' @param col    color of error bar
#' @param length length of the edges of the error bar (in inches)
#' @seealso [p_errorBar()]
#' @export
p_errorBarY <- function(x, y, err, col="black", length=0.1) {
  myutils:::errorBar(x, y-err, x, y+err, col, length)
}

