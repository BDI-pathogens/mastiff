# TODO: Documentation for the base class (Possibly inherit from PRESTO doc
# branch)

.get_required_args <- function( func ) {
  args  <- formals( func )
  rArgs <- unlist( lapply( args, function( x ) ifelse( length(x)==1, x == "", FALSE ) ) )
  if( !length( rArgs ) )
    return( c() )
  rArgs <- names( args )[ which( rArgs ) ] 
  rArgs <- rArgs[ which( rArgs != "..." ) ]
  return( rArgs )
}

###################################################################################/
# utils.class
#
# add interfaces to R6 class infrastructure
# Author: Rob Hinch
###################################################################################/
##### NOTE: All derived R6 classes using interfaces should include this file via
##### Roxygen using the include tag: #' @include R6_util_class.R to update the
##### collate field in DESCRIPTION
utils.class = function(
    classname = NULL,
    public    = list(),
    private   = list(),
    active    = list(),
    inherit   = NULL,
    interfaces = list(),
    lock_objects = TRUE,
    class      = TRUE,
    portable   = TRUE,
    lock_class = FALSE,
    cloneable  = TRUE,
    parent_env = parent.frame()
)
{
  # check to see an inherited class has been created by utils.class
  if( !is.null( inherit ) ){
    if( inherit$inherit != "utils.class.parent" )
      stop( "inherited classes must be created by utils.class (i.e. must inherited utils.class.class)" )
  } else{
    inherit = utils.class.class
  }
  
  # create an environment in the parent_env which just contains the name of the inherited generator
  envir = new.env( parent = parent_env )
  utils.class.parent = inherit
  assign( "utils.class.parent", utils.class.parent, envir = envir )
  
  # add interfaces to R6 class
  if( !is.list( interfaces ) ) interfaces = list( interfaces )
  
  # if inheriting a class, we need to include all the inherited methods
  publicMethods  <- public 
  privateMethods <- private 
  activeMethods  <- active 
  if( !is.null( inherit$public_methods ) )
    publicMethods  <- utils::modifyList( inherit$public_methods, publicMethods )
  if( !is.null( inherit$private_methods ) )
    privateMethods <- utils::modifyList( inherit$private_methods, privateMethods )
  if( !is.null( inherit$active ) )
    activeMethods  <- utils::modifyList( inherit$active, activeMethods )
  
  interfaceNames = c()
  nInterfaces    = length( interfaces )
  if( nInterfaces ){
    publicNames  <- names( publicMethods )
    privateNames <- names( privateMethods )
    activeNames  <- names( activeMethods )
    
    for( k in 1:nInterfaces ){
      if( interfaces[[ k ]]$inherit != "utils.class.interface.class" )
        stop( "interfaces must be inherited from utils.class.interface.class" )
      
      iName = interfaces[[ k ]]$classname
      
      # check public methods first
      for( iPublic in list( interfaces[[ k ]]$public_methods, interfaces[[ k ]]$public_fields ) )
        if( !is.null( iPublic ) )
          if( length( iPublic ) )
            for( j in 1:length( iPublic ) ){
              iMethName =names( iPublic )[ j ]
              if( iMethName == "clone" )
                next();
              if( !( iMethName %in% publicNames ) )
                stop( sprintf( "must implement public method %s on interface %s", iMethName, iName ) )
              args  <- formalArgs( publicMethods[[ iMethName ]] )
              iArgs <- formalArgs( iPublic[[ iMethName ]] )
              r_args  <- .get_required_args( publicMethods[[ iMethName ]] )
              r_iArgs <- .get_required_args( iPublic[[ iMethName ]] )
              if( length( r_iArgs ) ) {
                if( !all( r_iArgs %in% args ) )
                  stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
              }
              if( length( r_args ) ) {
                if( !all( r_args %in% iArgs ) )
                  stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
              }
            }
      
      # check private methods
      for( iPrivate in list( interfaces[[ k ]]$private_methods, interfaces[[ k ]]$private_fields ) )
        if( !is.null( iPrivate ) )
          if( length( iPrivate ) )
            for( j in 1:length( iPrivate ) ){
              iMethName = names( iPrivate )[ j ]
              if( !( iMethName %in% privateNames ) )
                stop( sprintf( "must implement private method %s on interface %s", iMethName, iName ) )
              args  <- formalArgs( privateMethods[[ iMethName ]] )
              iArgs <- formalArgs( iPrivate[[ iMethName ]] )
              r_args  <- .get_required_args( privateMethods[[ iMethName ]] )
              r_iArgs <- .get_required_args( iPrivate[[ iMethName ]] )
              if( length( r_iArgs ) ) {
                if( !all( r_iArgs %in% args ) )
                  stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
              }
              if( length( r_args ) ) {
                if( !all( r_args %in% iArgs ) )
                  stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
              }
            }
      
      # check active methods
      iActive = interfaces[[ k ]]$active
      if( !is.null( iActive ) )
        if( length( iActive ) )
          for( j in 1:length( iActive ) )
          {
            iMethName = names( iActive )[ j ]
            if( !( iMethName %in% activeNames ) )
              stop( sprintf( "must implement active field %s on interface %s", iMethName, iName ) )
            args  <- formalArgs( activeMethods[[ iMethName ]] )
            iArgs <- formalArgs( iActive[[ iMethName ]] )
            r_iArgs <- .get_required_args( iActive[[ iMethName ]] )
            if( length( r_iArgs ) ) {
              if( !all( r_iArgs %in% args ) )
                stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
            }
            if( length( r_args ) ) {
              if( !all( r_args %in% iArgs ) )
                stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
            }
          }
      
      interfaceNames[ length( interfaceNames ) + 1 ] = iName
    }
  }
  private$.INTERNAL_INTERFACES = c( inherit$private_fields$.INTERNAL_INTERFACES, interfaceNames )
  active$interfaces = function() return( private$.INTERNAL_INTERFACES )
  
  return( R6::R6Class( classname    = classname,
                       public       = public,
                       private      = private,
                       active       = active,
                       inherit      = utils.class.parent,
                       lock_objects = lock_objects,
                       class        = class,
                       portable     = portable,
                       lock_class   = lock_class,
                       cloneable    = cloneable,
                       parent_env   = envir ) )
}

###################################################################################/
# utils.class.class ####
#
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.class = R6::R6Class(
  "utils.class.class",
  private = list(
    .INTERNAL_INTERFACES = c()
  ),
  active = list(
    interfaces = function( val ){
      if( is.null( val ) ){
        return( private$.INTERNAL_INTERFACES )
      } else {
        stop( "cannot update interface list manually" )
      }
    }
  )
)

###################################################################################/
# utils.class.interface.class ####
#
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.interface.class = R6::R6Class(
  "utils.class.interface.class",
  public = list(
    is.interface  = function() return( TRUE )
  )
)

###################################################################################/
# utils.class.interface
# add interfaces to R6 class infrastructure
###################################################################################/
utils.class.interface = function(
    interfacename = NULL,
    public = list(),
    private = list(),
    active = list()
)
{
  return( R6::R6Class( interfacename,
                       public  = public,
                       private = private,
                       active  = active,
                       inherit = utils.class.interface.class ) )
}

###################################################################################/
# utils.class.interface.implements
# checks to see if an interface has been implemented
# check private internal variable directly to prevent accidental name mismatches
###################################################################################/
utils.class.interface.implements = function(
    object,
    interfaceName
)
{
  if( !R6::is.R6( object ) | !inherits( object, "utils.class.class") )
    stop( "object must be from a class generated by utils.class()" )
  
  if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
    stop( "object must be from a class generated by utils.class()" )
  
  return( length( intersect( object$.__enclos_env__$private$.INTERNAL_INTERFACES, interfaceName ) ) == 1 )
}

################################################################################

# ###################################################################################/
# # utils.class
# #
# # add interfaces to R6 class infrastructure
# ###################################################################################/
# ##### NOTE: All derived R6 classes using interfaces should include this file via
# ##### Roxygen using the include tag: #' @include R6_util_class.R to update the
# ##### collate field in DESCRIPTION
# utils.class = function(
    #     classname = NULL,
#     public    = list(),
#     private   = list(),
#     active    = list(),
#     inherit   = NULL,
#     interfaces = list(),
#     lock_objects = TRUE,
#     class      = TRUE,
#     portable   = TRUE,
#     lock_class = FALSE,
#     cloneable  = TRUE,
#     parent_env = parent.frame()
# )
# {
#   # check to see an inherited class has been created by utils.class
#   if( !is.null( inherit ) )
#   {
#     if( inherit$inherit != "utils.class.parent" )
#       stop( "inherited classes must be created by utils.class (i.e. so have inherited utils.class.class)" )
#   }
#   else
#     inherit = utils.class.class
#   
#   # create an environment in the parent_env which just contains the name of the inherited generator
#   envir = new.env( parent = parent_env )
#   utils.class.parent = inherit
#   assign( "utils.class.parent", utils.class.parent, envir = envir )
#   
#   # add interfaces to R6 class
#   if( !is.list( interfaces ) )
#     interfaces = list( interfaces )
#   
#   # if inheriting a class, we need to include all the inherited methods
#   publicMethods  <- public 
#   privateMethods <- private 
#   activeMethods  <- active 
#   if( !is.null( inherit$public_methods ) )
#     publicMethods  <- utils::modifyList( inherit$public_methods, publicMethods )
#   if( !is.null( inherit$private_methods ) )
#     privateMethods <- utils::modifyList( inherit$private_methods, privateMethods )
#   if( !is.null( inherit$active ) )
#     activeMethods  <- utils::modifyList( inherit$active, activeMethods )
#   
#   interfaceNames = c()
#   nInterfaces    = length( interfaces )
#   if( nInterfaces )
#   {
#     publicNames  <- names( publicMethods )
#     privateNames <- names( privateMethods )
#     activeNames  <- names( activeMethods )
#     
#     for( k in 1:nInterfaces )
#     {
#       if( interfaces[[ k ]]$inherit != "utils.class.interface.class" )
#         stop( "interfaces must be inherited from utils.class.interface.class" )
#       
#       iName = interfaces[[ k ]]$classname
#       
#       # check public methods first
#       for( iPublic in list( interfaces[[ k ]]$public_methods, interfaces[[ k ]]$public_fields ) )
#         if( !is.null( iPublic ) )
#           for( j in 1:length( iPublic ) )
#           {
#             iMethName =names( iPublic )[ j ]
#             if( iMethName == "clone" )
#               next();
#             if( !( iMethName %in% publicNames ) )
#               stop( sprintf( "must implement public method %s on interface %s", iMethName, iName ) )
#             args  <- formalArgs( publicMethods[[ iMethName ]] )
#             iArgs <- formalArgs( iPublic[[ iMethName ]] )
#             if( !all( iArgs %in% args, args %in% iArgs ) )
#               stop( sprintf( "incorrect arguments for public method %s on interface %s", iMethName, iName ) )
#           }
#       
#       # check private methods
#       for( iPrivate in list( interfaces[[ k ]]$private_methods, interfaces[[ k ]]$private_fields ) )
#         if( !is.null( iPrivate ) )
#           for( j in 1:length( iPrivate ) )
#           {
#             iMethName = names( iPrivate )[ j ]
#             if( !( iMethName %in% privateNames ) )
#               stop( sprintf( "must implement private method %s on interface %s", iMethName, iName ) )
#             args  <- formalArgs( privateMethods[[ iMethName ]] )
#             iArgs <- formalArgs( iPrivate[[ iMethName ]] )
#             if( !all( iArgs %in% args, args %in% iArgs ) )
#               stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
#           }
#       
#       # check active methods
#       iActive = interfaces[[ k ]]$active
#       if( !is.null( iActive ) & length( iActive ) > 0 )
#         for( j in 1:length( iActive ) )
#         {
#           iMethName = names( iActive )[ j ]
#           if( !( iMethName %in% activeNames ) )
#             stop( sprintf( "must implement active field %s on interface %s", iMethName, iName ) )
#           args  <- formalArgs( activeMethods[[ iMethName ]] )
#           iArgs <- formalArgs( iActive[[ iMethName ]] )
#           if( !all( iArgs %in% args, args %in% iArgs ) )
#             stop( sprintf( "incorrect arguments for active method %s on interface %s", iMethName, iName ) )
#           
#         }
#       
#       interfaceNames[ length( interfaceNames ) + 1 ] = iName
#     }
#   }
#   private$.INTERNAL_INTERFACES = c( inherit$private_fields$.INTERNAL_INTERFACES, interfaceNames )
#   active$interfaces = function() return( private$.INTERNAL_INTERFACES )
#   
#   return( R6::R6Class( classname    = classname,
#                        public       = public,
#                        private      = private,
#                        active       = active,
#                        inherit      = utils.class.parent,
#                        lock_objects = lock_objects,
#                        class        = class,
#                        portable     = portable,
#                        lock_class   = lock_class,
#                        cloneable    = cloneable,
#                        parent_env   = envir ) )
# }
# 
# ###################################################################################/
# # utils.class.class ####
# #
# # add interfaces to R6 class infrastructure
# ###################################################################################/
# utils.class.class = R6::R6Class(
#   "utils.class.class",
#   private = list(
#     .INTERNAL_INTERFACES = c()
#   ),
#   active = list(
#     interfaces = function( val ) if( is.null( val ) ){
#       return( private$.INTERNAL_INTERFACES )
#     } else {
#         stop( "cannot update interface list manually" )
#       }
#   )
# )
# 
# ###################################################################################/
# # utils.class.interface.class ####
# #
# # add interfaces to R6 class infrastructure
# ###################################################################################/
# utils.class.interface.class = R6::R6Class(
#   "utils.class.interface.class",
#   public = list(
#     is.interface  = function() return( TRUE )
#   )
# )
# 
# ###################################################################################/
# # utils.class.interface
# # add interfaces to R6 class infrastructure
# ###################################################################################/
# utils.class.interface = function(
    #     interfacename = NULL,
#     public = list(),
#     private = list(),
#     active = list()
# ){
#   return( R6::R6Class(
#     interfacename,
#     public  = public,
#     private = private,
#     active  = active,
#     inherit = utils.class.interface.class ) )
# }
# 
# ###################################################################################/
# # utils.class.interface.implements
# # checks to see if an interface has been implemented
# # check private internal variable directly to prevent accidental name mismatches
# ###################################################################################/
# utils.class.interface.implements = function(
    #     object,
#     interfaceName
# ){
#   if( !R6::is.R6( object ) | !inherits( object, "utils.class.class") )
#     stop( "object must be from a class generated by utils.class()" )
#   
#   if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
#     stop( "object must be from a class generated by utils.class()" )
#   
#   return( length( intersect( object$.__enclos_env__$private$.INTERNAL_INTERFACES, interfaceName ) ) == 1 )
# }

###################################################################/
# Name:        utils.uniroot.vectorized
# Description: vectorized version of the Brent Root  algorithm
#              useful for when solving many similar optimization problems
#              where evaluation of the objective can be calculated far more
#              efficiently when vectorized
# Args:        func  - function to optimize over with single input (vector) and outputs a vector of values
#              a     - vector of left hand boundary
#              b     - vector of right hand boundary
# Return:
# Author:      Rob Hinch
###################################################################/
utils.uniroot.vectorized = function( func, a, b, tol = 1e-8, itmax = 100, eps = 1e-10 )
{
  # check the initial bracket
  fa <- func(a)
  fb <- func(b)
  if( max( fa > 0 & fb > 0 ) || max(fa < 0 &  fb < 0 ) )
    stop( "all roots must be bracketed" )
  
  # main bracketing loop
  d  <- rep( NA, length( a ) )
  e  <- rep( NA, length( a ) )
  c  <- b
  fc <- fb
  for( ii in 1:itmax )
  {
    flip <- ( fc > 0 & fb > 0 ) | (fc < 0 &  fb < 0 )
    c    <- ifelse( flip, a, c )
    fc   <- ifelse( flip, fa, fc )
    d    <- ifelse( flip, b-a, d )
    e    <- ifelse( flip, d, e )
    
    closer <- abs( fc ) < abs( fb )
    a      <- ifelse( closer, b, a )
    b      <- ifelse( closer, c, b )
    c      <- ifelse( closer, a, c )
    fa     <- ifelse( closer, fb, fa )
    fb     <- ifelse( closer, fc, fb )
    fc     <- ifelse( closer, fa, fc )
    
    # convergence check
    tol1 <- 2 * eps * abs(b) + 0.5 * tol
    xm   <- 0.5 * (c-b)
    
    if( min( ( abs( xm ) < tol1 ) | fb == 0 ) )
      return( b )
    
    # Attempt inverse quadratic interpolation
    s <- fb/fa
    q <- fa/fc
    r <- fb/fc
    p <- ifelse( a == c, 2 * xm * s, s * ( 2 * xm * q * (q-r) - (b-a) * (r-1) ) )
    q <- ifelse( a == c, 1-s, ( q - 1 ) * ( r - 1 ) * (s - 1 ) )
    
    # Check whether in bounds.
    q <- ifelse( p > 0, -q, q )
    p <- abs( p )
    
    # accept interpolation
    accept <- ( abs(e)  >= tol1 ) &
      ( abs(fa) > abs(fb) ) &
      ( 2 * p   < pmin( 3 * xm * q - abs( tol1*q ), abs( e*q ) ) )
    e <- ifelse( accept, d, xm )
    d <- ifelse( accept, p/q, xm )
    
    # Move last best guess to a.
    a  <- b
    fa <- fb
    b  <- ifelse( abs( d ) > tol1, b + d, b + tol1 * sign( xm ) )
    fb <- func( b )
  }
  
  stop( "exceeding maximum iterations" )
  return( b )
}

###################################################################/
# Name:        utils.optimise.vectorized
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
utils.optimise.vectorized = function( f, a, v, b, tol = 1e-6, maximum = F, itmax = 100 )
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
