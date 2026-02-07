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
#' @seealso
#' * [bisection()]
#' * [uniroot()]
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



#' Bisection Method
#' @param f  function returning a real value
#' @param rangeFrom minimum value of range
#' @param rangeFrom maximum value of range
#' @return root
bisection <- function(f, rangeFrom, rangeTo, itMax = 100, tol = 1e-7) {
  # Check arguments.
  if (rangeFrom >= rangeTo) {
    stop("'rangeFrom' must be smaller than 'rangeTo'.")
  } else if (f(rangeFrom) * f(rangeTo) >= 0) {
    stop('the sign of f(rangeFrom) must be different from that of f(rangeTo)')
  }

  a <- rangeFrom
  b <- rangeTo
  c <- a

  for (i in 1:itMax) {
    c <- (a + b) / 2 # Calculate midpoint

    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the
    # function and return the root.
    if (abs(f(c)) < tol) {
      break
    }

    # If another iteration is required,
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    if (sign(f(c)) == sign(f(a))) {
      a <- c
    } else {
      b <- c
    }
  }

  if (i >= itMax) {
    print('Too many iterations')
  }

  return(c)
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
