##################################################################/
#  distribution.abstract.class
###################################################################/
#' Class: `distribution.abstract.class`
#' @description Base class for all derived distributions
#'
#' @param x          vector of quantiles.
#' @param q          vector of quantiles.
#' @param p          vector of probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' 
#' @field name         The name of the distribution
#' @field param_names  The names of all distribution parameters
#' @field params       Named list of distribution parameters
#' @field interfaces   The list of available class interfaces
#' 
#' @include R6_util_class.R
distribution.abstract.class <- utils.class(
  "distribution.abstract.class",
  private = list(
    .name   = NULL, # Distribution name
    .params = list(), # Named list of distribution parameters
    .param_names = character(), # Character vector of names for params list
    .check_params = function( params ){
      # Verify all input parameters are plausible - should be updated on every
      # derived class
      return( TRUE )
    },
    .staticReturn = function( val, name ){
      # Creates static active binding `name` with value `val` which cannot be
      # updated after class creation
      if( !missing( val ) )
        stop( sprintf( "cannot set %s", name ) )
      
      privateName <- sprintf( ".%s", name )
      return( private[[ privateName ]])
    },
    .updateReturn = function( val, type, param = NULL ){
      privateType <- sprintf( ".%s", type )
      
      if( is.null( param ) ) {
        if( !is.null( val ) )
          private[[ privateType ]] <- val
        return( private[[ privateType ]] )
      }
      
      if( !is.null( val ) ) {
        all <- private[[ privateType ]]
        all[[ param ]] <- val
        private[[ privateType ]] <- all
      }
      return( private[[ privateType ]][[ param ]] )
    }
  ),
  active  = list(
    name         = function( val ) private$.staticReturn( val, "name" ),
    param_names  = function( val ) private$.staticReturn( val, "param_names" ),
    params = function( new_val ){
      if ( missing( new_val ) ) return( private$.params )
      
      nv_names <- names( new_val )
      if ( length( nv_names ) == 0 )
        stop( sprintf( "`$param` must be a named list with elements `%s`",
              paste( private$.param_names, sep = '`, ' ) ) )
      if ( !all( nv_names %in% private$.param_names,
                 private$.param_names %in% nv_names ) )
        stop( sprintf( "`$param` must be a named list with elements `%s`. Input list contained elements `%s`",
                       paste( private$.param_names, sep = '`, ' ),
                       paste( nv_names, sep = '`, ' ) ) )
      
      private$.check_params( new_val )
      private$.params <- new_val
    }
  ),
  public = list(
    ##############################################################################/
    # initialize
    ##############################################################################/
    #' @description Create a new object of class `distribution.abstract.class`
    initialize = function()
      stop( "Object of class `distribution.abstract.class`"),
    ##############################################################################/
    # density
    ##############################################################################/
    #' @description Template base class function for density function of a
    #'   distribution
    d = function( x, log = FALSE )
      stop( "`d` not implemented on derived class"),
    ##############################################################################/
    # cumulative distribution function
    ##############################################################################/
    #' @description Template base class function for cumulative distribution
    #'   function
    p = function( q, lower.tail = TRUE, log.p = FALSE )
      stop( "`p` not implemented on derived class"),
    ##############################################################################/
    # quantile function
    ##############################################################################/
    #' @description Template base class function for quantile function of a
    #'   distribution
    q = function( p, lower.tail = TRUE, log.p = FALSE )
      stop( "`q` not implemented on derived class"),
    ##############################################################################/
    # random variates
    ##############################################################################/
    #' @description Template base class function for sampling random variates
    r = function( n )
      stop( "`r` not implemented on derived class")
  )
)