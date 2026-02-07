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
rcbmat = function(header, ...) {
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
wcbmat = function(data, header, sep="\t", size=128, row.names=FALSE, qmethod="double",
                  col.names=ifelse(header && row.names, NA, header), ...) {
  if (missing(header)) { header = FALSE }

  if (typeof(size) != "double" && typeof(size) != "integer") {
    stop("type of 'size' must be 'double' or 'integer'.")
  }
  fn = sprintf("clipboard-%d", size)

  utils::write.table(data, file=fn, sep=sep, row.names=row.names,
                     col.names=col.names, qmethod=qmethod, ...)
}




