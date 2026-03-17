###################################################################/
# Name:        uniroot.vectorized
# Description: vectorized version of the Brent Root  algorithm
#              useful for when solving many similar optimization problems
#              where evaluation of the objective can be calculated far more
#              efficiently when vectorized
# Args:        f     - function to optimize over with single input (vector) and outputs a vector of values
#              lower - vector of left hand boundary
#              upper - vector of right hand boundary
# Return:
# Author:      Rob Hinch
###################################################################/
#' Vectorised uniroot
#'
#' A vectorised version of the Brent root algorithm. Useful for solving many
#' similar optimisation problems where evaluation of the objective function can
#' be calculated more efficiently when vectorised
#' 
#' @param f  the objective function to optimise over. Must take a single vector
#'   as input and output a vector of values
#' @param lower vector of left hand end points of the intervals to optimise over
#' @param upper vector of right hand end points of the intervals to optimise over
#' @param tol   the desired accuracy (convergence tolerance)
#' @param itmax the maximum number of iterations
#' @param eps   XXXXX TODO
#' 
#' @return vector of roots for each interval \code{[lower, upper ]}
#' 
#' @examples
#' f <- function( x ) ( x^2 - 1 ) * ( x^2 - 2 )
#' lower <- c( -2,  -1.2,   0, 1.2 )
#' upper <- c( -1.2,   0, 1.2,   2 )
#' uniroot.vectorized( f, lower, upper )
#' 
#' @export

uniroot.vectorized = function( f, lower, upper, tol = 1e-8, itmax = 100, eps = 1e-10 ){
  # check the initial bracket
  f_lower <- f( lower )
  f_upper <- f( upper )
  
  if ( any( sign( f_lower * f_upper ) == 1 ) )
    stop( "all roots must be bracketed" )
  
  # main bracketing loop
  d  <- rep( NA, length( lower ) )
  e  <- rep( NA, length( lower ) )
  c  <- upper
  fc <- f_upper
  for( ii in 1:itmax )
  {
    flip <- ( fc > 0 & f_upper > 0 ) | (fc < 0 &  f_upper < 0 )
    c    <- ifelse( flip, lower, c )
    fc   <- ifelse( flip, f_lower, fc )
    d    <- ifelse( flip, upper-lower, d )
    e    <- ifelse( flip, d, e )
    
    closer <- abs( fc ) < abs( f_upper )
    lower      <- ifelse( closer, upper, lower )
    upper      <- ifelse( closer, c, upper )
    c      <- ifelse( closer, lower, c )
    f_lower     <- ifelse( closer, f_upper, f_lower )
    f_upper     <- ifelse( closer, fc, f_upper )
    fc     <- ifelse( closer, f_lower, fc )
    
    # convergence check
    tol1 <- 2 * eps * abs(upper) + 0.5 * tol
    xm   <- 0.5 * (c-upper)
    
    if( min( ( abs( xm ) < tol1 ) | f_upper == 0 ) )
      return( upper )
    
    # Attempt inverse quadratic interpolation
    s <- f_upper/f_lower
    q <- f_lower/fc
    r <- f_upper/fc
    p <- ifelse( lower == c, 2 * xm * s, s * ( 2 * xm * q * (q-r) - (upper-lower) * (r-1) ) )
    q <- ifelse( lower == c, 1-s, ( q - 1 ) * ( r - 1 ) * (s - 1 ) )
    
    # Check whether in bounds.
    q <- ifelse( p > 0, -q, q )
    p <- abs( p )
    
    # accept interpolation
    accept <- ( abs(e)  >= tol1 ) &
      ( abs(f_lower) > abs(f_upper) ) &
      ( 2 * p   < pmin( 3 * xm * q - abs( tol1*q ), abs( e*q ) ) )
    e <- ifelse( accept, d, xm )
    d <- ifelse( accept, p/q, xm )
    
    # Move last best guess to lower.
    lower  <- upper
    f_lower <- f_upper
    upper  <- ifelse( abs( d ) > tol1, upper + d, upper + tol1 * sign( xm ) )
    f_upper <- f( upper )
  }
  
  stop( "exceeding maximum iterations" )
  return( upper )
}

###################################################################/
# Name:        optimise.vectorized
# Description: vectorized version of the Brent Minimisation algorithm
#              useful for when solving many similar optimization problems
#              where evaluation of the objective can be calculated far more
#              efficiently when vectorized
# Args:        f  - function to optimize over with single input (vector) and outputs a vector of values
#              a  - vector of left hand boundary
#              v  - vector of best initial guesses
#              b  - vector of right hand boundary
# Return:      TRUE/FALSE
# Author:      Rob Hinch
###################################################################/
optimise.vectorized = function( f, a, v, b, tol = 1e-6, maximum = F, itmax = 100 )
{
  # constants
  cgold = 0.3819660
  zeps  = 1e-10
  
  # initial data check
  if( sum( a > v ) > 0 | sum( v > b ) > 0 )
    stop( "The initial value and boundary must saisfy a<v<b" );
  
  lv = length( v )
  if( length( a ) != lv | length( b ) != lv )
    stop( "The initial value and boundaries must be of the same length" )
  
  # initial set up
  w = v
  x = v
  e = 0
  d = 0
  fx = f( x )
  if( maximum )
    fx = -fx
  fv = fx
  fw = fx
  
  # check the function return
  if( length( fx ) != lv )
    stop( "The initial value and function return must be of the same length")
  # main loop
  for( iter in 1:itmax )
  {
    xm   = 0.5 * ( a + b )
    
    # stop when all are good, calculate relative tolerances
    tol1 = tol * abs( x ) + zeps
    tol2 = tol1 * 2
    if( sum( abs( x - xm ) > ( tol2 - 0.5 * ( b - a ) ) ) == 0 )
      break
    
    # fit parabola for all
    r = ( x - w ) * ( fx - fv )
    q = ( x - v ) * ( fx - fw )
    p = ( x - v ) * q - ( x - w ) * r
    q = 2 * ( q - r )
    p = -p * sign( q )
    q = abs( q )
    etemp =e
    e  = d
    dd = p / ( q + zeps );  # avoid divide by 0
    u  = x + dd
    d  = dd + ( u - a < tol2 | b - u < tol2 ) * ( ( xm - x > 0 ) * tol - dd )
    
    # next find golden ratio point
    eg = -x + a + ( x < xm ) * ( b - a )
    dg = eg * cgold
    
    # decide whether to pick the paroabola  min or golden ratio point
    absEtemp = abs( etemp )
    gr = ( absEtemp > tol1 )  & ( 2 * abs( p ) < q *  absEtemp ) & ( dd > (a-x) ) & ( dd < ( b - x ) )
    e  = eg + gr * ( e -eg)
    d  = dg + gr * ( d - dg )
    
    # move to the next point, if distance is smaller than tolerance then move by tolerance
    u = x + d + ( abs( d ) < tol1 ) * ( sign( d ) * tol1 - d )
    
    # single function evaluation at next point
    fu = f( u )
    if( maximum )
      fu = - fu
    
    # finally book-keeping to see which point to update
    cond1 = fu >= fx
    cond2 = u < x
    cond3 = fu > fw & w != x
    cond4 = cond1 | cond2
    cond5 = !( cond1 & cond2 )
    cond6 = cond1 | !cond2
    cond7 = !cond1 | cond2
    cond8 = cond1 & cond3
    
    a  = x + cond4 * ( -x + u + cond5 * ( a - u ) )
    b  = x + cond6 * ( -x + u + cond7 * ( b - u ) )
    v  = w + cond8 * ( u - w )
    fv = fw + cond8 * ( fu - fw )
    w  = x + cond1 * ( -x + u + cond3 * ( w -u ) )
    fw = fx + cond1 * ( -fx + fu + cond3 * ( fw -fu ) )
    x  = u + cond1 * ( -u + x )
    fx = fu + cond1 * ( -fu + fx )
  }
  if( maximum )
    return( list( maximum = x, objective = - fx ) )
  else
    return( list( minimum = x, objective = fx ) )
}