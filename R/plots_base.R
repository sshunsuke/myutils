# * * * * * * * * * * * * * * * ----
# Plot (p) ----
# このファイルだけプロジェクトに追加すれば機能する
# * * * * * * * * * * * * * * *

## Colors  ----

p_core_col32_ <- function(col, alpha) {
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
  mapply(p_core_col32_, cols, alphas, USE.NAMES=FALSE)
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


## Templates ----


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

## Polygon   ----

p_polygon_upper_lower <- function(x, y_upper, y_lower, col="#00000028", lty=0) {
  polygon(c(x, rev(x)), c(y_upper, rev(y_lower)), col=col, lty=lty)
}

p_polygon_error <- function(x, y, err_percentage, col="#00000028", lty=0) {
  y_upper = y * (100 + err_percentage) / 100
  y_lower = y * (100 - err_percentage) / 100
  p_polygon_upper_lower(x, y_upper, y_lower, col=col, lty=lty)
}


## Examples  ----

p_ex_timeline <- function() {
  x <- as.POSIXct( c("2012-02-12 10:0:0", "2012-02-12 12:30:0", "2012-02-12 15:0:0", "2012-02-12 20:0:0") )
  y <- c(2, 5, 7, 3)
  xRange <- c( x[1], tail(x, 1) )
  yRange <- c( 0, 10 )
  
  plot(xRange, yRange, type='n', xaxt="n", main="Scatter 3 (axis-time)", xlab="Time", ylab="Data")
  axis.POSIXct(1, at=seq(xRange[1], xRange[2], by="2 hour"), format="%H:%M")
  abline(v = seq(x[1], tail(x, 1), by="2 hour"), col = "#dddddd", lty = 1, lwd = 0.7)
  lines(x, y)
  points(x, y, pch=19)
}

p_ex_two_y_axes <- function() {
  x <- c(0, 10, 20, 30, 50)
  y1 <- c(0, 1, 2, 4, 8)
  y2 <- c(900, 450, 225, 900, 900)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(oma=c(2,2,2,2))
  
  plot(x, y1, xlim=c(0,50), ylim=c(0,10), type="l", xaxt="n", yaxt="n",
       col="red", xlab="x", ylab="axis y1 (red)", main="2 axes (Y)")
  axis(1, 0:5*10)
  axis(2, 0:5*2)
  
  par(new=T)
  
  plot(x, y2, xlim=c(0,50), ylim=c(0,1000), type="l", xaxt="n", yaxt="n",
       col="blue", xlab="", ylab="", lwd=2)
  axis(4)
  mtext("axis y2 (blue)", side=4, line=3)
}

p_ex_two_x_axes <- function() {
  x1 <- c(0.1, 0.1, 0.2, 0.4, 0.8, 0.8)
  x2 <- c(420, 300, 120, 130, 50, 20)
  y  <- c(0, 2, 4, 6, 8, 10)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(oma=c(0,1,2,1))
  
  plot(x1, y, xlim=c(0,1), ylim=c(0,10), type="l", xaxt="n", yaxt="n",
       col="red", xlab="axis x1 (red)", ylab="y", main="2 axes (X)")
  axis(1, 0:5*0.2)
  axis(2, 0:5*2)
  
  par(new=T)
  
  plot(x2, y, xlim=c(0,500), ylim=c(0,10), type="l", xaxt="n", yaxt="n",
       col="blue", xlab="", ylab="", lwd=2)
  axis(3)
  mtext("axis x2 (blue)", side=3, line=3)
}

p_ex_hist_withCum <- function(x=rnorm(1000), ratio=FALSE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(oma=c(2,1,2,3))
  h <- hist(x)
  par(new=T)
  y <- c(0, cumsum(h$count))
  if (ratio) { y <- y / tail(y, n=1) }
  plot(h$breaks, y, type="l", xaxt="n", yaxt="n", xlab="", ylab="")
  axis(4)
  mtext("Cum.Frequency", side=4, line=3)
}
