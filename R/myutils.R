#' Package of my utility functions
#'
#' @name myutils
NULL


# prefix
#   p - plot
#   v - vector

# ***********************************
# IO Clipboard ----
# ***********************************

#' Read clipboard as a data frame by using `utils::read.table()`
#'
#' @param header If TRUE, retrieved data contains a header
#' @param ...    optionals
#' @return       A data frame containing clipboard data
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
#' @param qmethod description
#' @param col.names description
#' @param ...    optionals
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

# * * * * * * * * * * * * * * * ----
# Data ----
# * * * * * * * * * * * * * * *

#' Read matrix data from a csv file.
#' @param csvfile path of a csvfile
#' @param skip    number of lines 
#' @export
read.matrix <- function(csvfile, skip=0) {
  csv = read.csv(filename, header=FALSE, skip=skip)
  x = csv[-1,1]
  y = as.numeric(csv[1,-1])
  z = csv[-1,-1]
  list(x=x, y=y, z=z, csv=csv)
}

#' Get the closest value(s) to `target`
#' @param v      a vector
#' @param target a number
#' @return       closest value(s) to `target`
v_closestValue <- function(v, target) {
  unique( v[ v_indexClosestValue(v, target) ] )
}

v_indexClosestValue <- function(v, target) {
  diff <- abs(v - target)
  which( diff == min(diff) )
}

#' Create a matrix of information of cumulative distribution.
#'
#' If you want to create a graph of cumulative distribution, you should use
#' ecdf() function.
#'   > fCP <- ecdf( c(3,76,58,24,100,1) ); plot(fCP)
#'
#' @param values     vector
#' @param decreasing If TRUE, the values are ordered from largest to smallest (optional)
#' @return           matrix
#' @export
v_cum_probability = function(values, decreasing=FALSE) {
  len <- length(values)
  cbind(X = sort(values, decreasing=decreasing), cum.prob = (1:len)/len)
}

v_cp_guessX = function(cp_mat, cp) {
  diff_cp = abs(cp_mat[,"cum.prob"] - cp)

  rn = which(diff_cp == min(diff_cp))
  if (min(diff_cp) == 0) {
    return( as.numeric(cp_mat[rn,"X"]) )
  }

  if (length(rn) == 1) {
    rn2 = ifelse(cp_mat[rn,"cum.prob"] > cp, rn-1, rn+1)
  } else if (length(rn) == 2) {
    rn2 = rn[2]
    rn = rn[1]
  } else {
    stop()
  }

  p = sort(cp_mat[rn:rn2,"cum.prob"])
  x = sort(cp_mat[rn:rn2,"X"])

  x[1] + (x[2] - x[1]) * (cp - p[1]) / (p[2] - p[1])
}

# ***************
# Matrix
# ***************

#' Reverse an order of columns of a matrix
#' @param mat matrix
#' @export
m_crev <- function(mat) {
  if (!is.matrix(mat)) { stop("mat is not a matrix") }
  mat[,ncol(mat):1]
}

#' Reverse an order of rows of a matrix
#' @param mat matrix
#' @export
m_rrev <- function(mat) {
  if (!is.matrix(mat)) { stop("mat is not a matrix") }
  mat[nrow(mat):1,]
}

# ***************
# * Data Frame
# ***************
df_orderBy <- function(df, colname, decreasing=FALSE) {
  if (missing(colname)) { stop("'colname' is not specified.") }
  df[order(df[,colname], decreasing=decreasing),]
}




