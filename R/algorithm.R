# ***********************************
# Find a root of a function ----
# ***********************************

#' Newton-Raphson method
#'
#' @param fun  function returning a real value
#' @param dFun derivative of `fun`
#' @param fD_0 initial value of the argument of `fun`
#' @param tol  desired accuracy (convergence tolerance)
#' @param itMax the maximum number of iterations
#' @return root of `fun`
#'
#' @export
#' @md
newtonRaphson = function(fun, dFun, fD_0, tol=1e-10, itMax=10) {
  it <- 0
  fD_n <- fD_0
  d <- fun(fD_n) / dFun(fD_n)

  while (abs(d) >= tol) {
    it <- it + 1
    if (it > itMax) {
      stop("Calculation did not converge.")
    }

    d <- fun(fD_n) / dFun(fD_n)
    fD_n <- fD_n - d
  }

  fD_n
}



# Greatest common divisor & Least Common Multiple ----

## gcd  ----

#' Calculate greatest common divisor by using Euclidean algorithm
#'
#' @param a a natural number
#' @param b a natural number
#' @return greatest common divisor
#' @export
gcd <- (function(){
  function(...){ Reduce(function(a, b){
    while(a %% b != 0){
      tmp <- b
      b <- a %% b
      a <- tmp
      #cat(sprintf("a=%.1f, b=%.1f\n", a, b))
    }
    return(b)
  }, c(...)) }
})()

## lcm ----



#' Least Common Multiple
#' @param a a natural number
#' @param b a natural number
#' @return least common multiple
#' @export
lcm <- (function(){
  function(...){ Reduce(function(a, b){
    a * b / myutils:::gcd(a,b)
  }, c(...)) }
})()
