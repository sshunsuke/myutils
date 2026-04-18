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




# * * * * * * * * * * * * * * * ----
# Plot (p) ----
# * * * * * * * * * * * * * * *

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
p_col32 <- function(cols, alphas) {
  if (missing(alphas)) { alphas = -1 }
  mapply(core_col32, cols, alphas, USE.NAMES=FALSE)
}

#' Convert a value to a hex color with a color pallet
#' @param v          vector of value(s)
#' @param vmin,vmax  minimum and maximum of `v`
#' @param f_pal      color pallet (function)
#' @param n          number of steps
#' @param log10_scale xxx
#'
#' @export
p_value2col <- function(v, vmin, vmax, f_pal=heat.colors, n = 256, log10_scale = FALSE) {
  # 範囲チェック
  if (any(v < vmin | v > vmax, na.rm = TRUE)) {
    stop("some values are out of range")
  }

  if (log10_scale && (vmin <= 0 || vmax <= 0)) {
    stop("log10 scale requires vmin > 0 and vmax > 0")
  }

  pal = f_pal(n)
  #pal <- heat.colors(n)
  #pal <- hcl.colors(n, palette = "Temps", alpha = NULL, rev = FALSE, fixup = TRUE)

  # Normalize
  if (!log10_scale) {
    # linear scale
    t <- (v - vmin) / (vmax - vmin)
  } else {
    # log10 scale
    t <- (log10(v) - log10(vmin)) /
      (log10(vmax) - log10(vmin))
  }

  # インデックス化
  idx <- round(t * (n - 1)) + 1

  pal[idx]
}


## templates ----


#' Create a blank plot
#' @param xRange,yRange    Ranges of plot
#' @param axes             Draw axes when TRUE
#' @param log              a character string ("", "x", "y", or "xy")
#' @param xlab,ylab        Label name of x or y axis
#' @param main             Overall title
#' @param xaxs,yaxs        Extra space
#' @param las              the style of axis labels (default = 1)
#' @param cex.axis         Font size of axis annotation (relative to the current setting of cex)
#' @param cex.lab          Font size of labels
#' @export
p_blank <- function(xRange, yRange, axes=TRUE, log="", xlab="", ylab="", main="", 
                    xaxs='r', yaxs='r', las=0, cex.axis=1, cex.lab=1) {
  
  plot(xRange, yRange, type='n', log=log, xlab=xlab, ylab=ylab, main=main, 
       xaxs=xaxs, yaxs=yaxs, xaxt="n", yaxt="n", cex.lab=cex.lab)
  if (axes) { axis(1, cex.axis=cex.axis); axis(2, las=las, cex.axis=cex.axis) }
}

#' Blank plot with axes that fits within the original data range
#' @param xRange,yRange    Ranges of plot
#' @param axes             Draw axes when TRUE
#' @param log              a character string ("", "x", "y", or "xy")
#' @param xlab,ylab        Label name of x or y axis
#' @param main             Overall title
#' @param las              the style of axis labels (default = 1)
#' @param cex.axis         Font size of axis annotation (relative to the current setting of cex)
#' @param cex.lab          Font size of labels
#' @export
p_blank_fit <- function(xRange, yRange, axes=TRUE, log="", xlab="", ylab="", main="",
                        las=1, cex.axis=1.25, cex.lab=1) {
  p_blank(xRange, yRange, axes=axes, log=log, xlab=xlab, ylab=ylab, main=main, 
          xaxs='i', yaxs='i', las=las, cex.axis=cex.axis, cex.lab=cex.lab)
  
  #plot(xRange, yRange, type='n', xaxs='i', yaxs='i', xaxt="n", yaxt="n",
  #     main=main, xlab=xlab, ylab=ylab, cex.lab=cex.lab)
  #if (axes) { axis(1, cex.axis=cex.axis); axis(2, las=las, cex.axis=cex.axis) }
}



#' Create a Excel-like blank plot
#'
#' @param xRange,yRange ranges of plot
#' @param xlab,ylab     label names
#' @param cex.axis,cex.lab cexs of axis and lab
#'
#' @export
p_blank_excel <- function(xRange, yRange, xlab="", ylab="", cex.axis=1.25, cex.lab=1.25) {
  plot(xRange, yRange, type="n", tck=0.03, xaxs='i', yaxs='i', las=1,
       xlab=xlab, ylab=ylab, cex.axis=cex.axis, cex.lab=cex.lab)
}




## draw error bar(s) ----

#' Add a error bar
#' @param x0,y0  coordinates of points from which to draw
#' @param x1,y1  coordinates of points to which to draw. At least one must the supplied
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

