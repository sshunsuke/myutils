# ***********************************
# Flow in (circular) Pipe       ----
# ***********************************

#' Darcy-Weisbach equation
#' @param fD       Darcy friction factor
#' @param density  density
#' @param velocity velocity
#' @param D        pipe diameter
#' @return         dP per unit length
#' @export
DarcyWeisbach <- function(fD, density, velocity, D) {
  fD * density * (velocity ^ 2) / (2 * D)
}

fD_laminar <- function(Re) { 64 / Re }

#' Blasius correlation to calculate Darcy friction factor
#' @param Re Reynolds number
#' @return Darcy friction factor
fD_Blasius <- function(Re, C=0.3164) { C / (Re ^ 0.25) }


fD_Colebrook <- function(roughness, D, Re, tol=1e-8, itMax=10, warn=TRUE) {
  core_ <- function(roughness, D, Re) {
    if (Re <= 4000 && (warn == TRUE)) { warning("Re <= 4000 !") }

    fun <- function(fD) {
      (1 / sqrt(fD)) + 2 * log10( roughness / D / 3.71 + 2.51 / Re / sqrt(fD))
    }

    # Derivative of fun().
    dFun <- function(fD) {
      - fD^(-3/2) * (1/2 + 2.51 / log(10) / (2.51 / Re / sqrt(fD) + roughness / 3.71 / D) / Re)
    }

    # Newton-Raphson method
    fD_0 <- myutils:::fD_Blasius(Re)
    fD_n <- UTIL$newtonRaphson(fun, dFun, fD_0, tol=tol, itMax=itMax)
    fD_n
  }
  mapply(core_, roughness, D, Re)
}


