#' Internal Constants in myutils
#'
#' Internal constants defined in 'myutils' package. Three colons (`myutils:::`) are necessary to access them.
#'
#' Physical constants:
#' \itemize{
#'   \item g : Gravity Acceleration (= 9.8 m/s2)
#'   \item R : Gas Constant (J/K/mol)
#'   \item kB : Boltzmann constant (J/K)
#'   \item Pstp : Standard Temperature (= 100000 Pa)
#'   \item Tstp : Standard Pressure    (= 273.15 K)
#'   \item Psatp : Standard Ambient Temperature
#'   \item Tsatp : Standard Ambient Pressure
#'   \item Pntp : Normal Temperature
#'   \item Tntp : Normal Pressure
#' }
#'
#' @name myutils_internal_constants
NULL

g <- 9.8        # Gravity Acceleration (m/s2)
R <- 8.3144621  # Gas Constant (J/K-mol)
# Na <- 6.022140857 * 10^23     # Avogadro constant
kB = 1.38064852 * 10^(-23)   # Boltzmann constant (J/K)

# STP (Standard Temperature and Pressure) - old
Pstp = 100000    # (Pa)
Tstp = 273.15    # (K)

# SATP (Standard Ambient Temperature and Pressure)
Psatp = 101325    # (Pa)
Tsatp = 298.15    # (K)

# NTP (Normal Temperature and Pressure)
Pntp = 101325     # (Pa)
Tntp = 293.15     # (K)

#' Get a list of internal physical_constants
#' @export
get_physical_constants <- function() {
  list(g=g, R=R, kB=kB,
       Pstp=Pstp, Tstp=Tstp, Psatp=Psatp, Pntp=Pntp, Tntp=Tntp)
}

# * * * * * ----

#' Drag force
#' 
#' @param CD      drag coefficient
#' @param rho_f   fluid density   - kg/m3
#' @param v       (slip) velocity - m/s
#' @param A_ref   reference area (projected area) - m2
#' @return        drag force - N
#' 
#' @export
FD <- function(CD, rho_f, v, A_ref) {
  0.5 * CD * rho_f * (v^2) * A_ref
}

#' Drag coefficient of a spherical object
#' 
#' @param d_s    diameter of a sphere - m
#' @param v_f    velocity - m/s
#' @param rho_f  density of fluid - kg/m3
#' @param mu_f   fluid viscosity - Pa-s
#' @param retall description
#' 
#' @export
CD_sphere <- function(d_s, v_f, rho_f, mu_f, retall=FALSE) {
  Re_p <- d_s * v_f * rho_f / mu_f
  
  # Clift & Gauvin (1971)
  CD <- 24/Re_p * (1 + 0.15* Re_p^0.687) + 0.42 / (1+4.25*10^4 * Re_p^(-1.16))  
  
  if (retall) {
    CD <- list(CD=CD, Re_p=Re_p)
  }
  CD
}

CD_sphere_PH <- function(d_s, v_f, rho_f, mu_f, retall=FALSE) {
  Re_p <- d_s * v_f * rho_f / mu_f

  CD_l <- 24 / Re_p                # laminar (Stokes)
  CD_i <- 18.5 / (Re_p^0.6)        # (Pan & Hanratty)
  CD_t <- rep(0.5, length(Re_p))   # turbulent
  
  CD <- ifelse(Re_p < 1.92, CD_l,
               ifelse(Re_p < 500, CD_i, CD_t))
  if (retall) {
    CD <- list(CD=CD, CD_l=CD_l, CD_i=CD_i, CD_t=CD_t, Re_p=Re_p)
  }
  CD
}






