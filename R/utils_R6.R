# Returns a character vector containing function arguments without a set default
# value set, e.g.
#   .get_required_args( function( x, y = 1 ) NULL )
# returns
#   "x",
# but
#   .get_required_args( function( x, y ) NULL )
# returns
#   c( "x", "y" )

.get_required_args <- function( func ) {
  args  <- formals( func )
  rArgs <- unlist( lapply( args, function( x ) ifelse( length(x)==1, x == "", FALSE ) ) )
  if( !length( rArgs ) )  # Equivalent to if ( length( rArgs ) == 0 )
    return( c() )
  rArgs <- names( args )[ which( rArgs ) ] 
  rArgs <- rArgs[ which( rArgs != "..." ) ]
  return( rArgs )
}


# Helper function to validate that all methods of type `method_type` defined on
# the interface are defined correctly on the class
#
# @param iMethod_list   list of methods defined on the interface to validate
#   against
# @param method_list    list of methods defined on the class to be validated
# @param iName          name of the interface contributing methods iMethod_list
# @param method_type    type of method being validated; used for informative
#   error messages
#
# Variable names with prefix i are related to the interface not the defined
# class.

.validate_interface_methods <- function( method_list, iMethod_list, iName,
                                         error_type = "public method" ){
  # For each method defined on the interface of type `method_type`, check that
  # the class defines a method with the same name and the same set of required
  # arguments
  methNames <- names( method_list )
  for ( iMethod in iMethod_list ){
    if ( !is.null( iMethod ) ){
      for ( iMethName in names( iMethod ) ){
        # clone method must exist on R6 class and does not need to be checked
        if ( iMethName == "clone" ) next
        
        # Check iMethName is defined on class (with any set of arguments)
        if ( !( iMethName %in% methNames ) ){
          stop( sprintf( "must implement %s %s on interface %s",
                         error_type, iMethName, iName))
        }
        
        # Check required arguments for interface public method
        iArgs <- formalArgs( iMethod[[ iMethName ]] )
        r_iArgs <- .get_required_args( iMethod[[ iMethName ]] )
        
        # Check required arguments for new class public method
        args  <- formalArgs( method_list[[ iMethName ]] )
        r_args  <- .get_required_args( method_list[[ iMethName ]] )
        
        if( length( r_iArgs ) ) {
          if( !all( r_iArgs %in% args ) )
            stop( sprintf( "incorrect arguments for %s %s on interface %s",
                           error_type, iMethName, iName ) )
        }
        if( length( r_args ) ) {
          if( !all( r_args %in% iArgs ) )
            stop( sprintf( "incorrect arguments for %s %s on interface %s",
                           error_type, iMethName, iName ) )
        }
      }
    }
  }
}

################################################################################/
# utils.class
#
# Author: Rob Hinch
################################################################################/
##### NOTE: All derived R6 classes using interfaces in mastiff should include
##### this file via Roxygen using the include tag: #' @include R6_util_class.R
##### to update the collate field in DESCRIPTION.
#####
##### Typically this is only for safety, but if a derived class is defined with
##### a name alphabetically before R6_util_class.R and included in another file,
##### the collate order might matter, e.g. R6_a_new_class.R might break the
##### collate order if #' @include R6_a_new_class.R is ever used.

#' Class: utils.class
#' 
#' @description R6 object extending [R6::R6Class()] to include interfaces.
#'
#' @inheritParams R6::R6Class
#' @param interfaces  An optional list of interfaces implemented for the derived
#'   class.
#'
#' @export

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
){
  # check to see an inherited class has been created by utils.class
  if( !is.null( inherit ) ){
    if( inherit$inherit != "utils.class.parent" )
      stop( "inherited classes must be created by utils.class (i.e. must inherit utils.class.class)" )
  } else{
    inherit = utils.class.class
  }
  
  # create an environment in the parent_env which just contains the name of the
  # inherited generator
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
    # publicNames  <- names( publicMethods )
    # privateNames <- names( privateMethods )
    # activeNames  <- names( activeMethods )
    
    for( k in 1:nInterfaces ){
      if( interfaces[[ k ]]$inherit != "utils.class.interface.class" )
        stop( "interfaces must be created by utils.class.interface (i.e. must inherit utils.class.interface.class" )
      
      iName = interfaces[[ k ]]$classname
      
      # Validate public methods
      .validate_interface_methods(
        method_list = publicMethods,
        iMethod_list = list( interfaces[[ k ]]$public_methods,
                             interfaces[[ k ]]$public_fields ),
        iName,
        error_type = "public method"
      )
      
      # Validate private methods
      .validate_interface_methods(
        method_list = privateMethods,
        iMethod_list = list( interfaces[[ k ]]$private_methods,
                             interfaces[[ k ]]$private_fields ),
        iName,
        error_type = "private method"
      )
      
      # Validate active methods
      .validate_interface_methods(
        method_list = activeMethods,
        iMethod_list = list( interfaces[[ k ]]$active ),
        iName,
        error_type = "active field"
      )
      
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

################################################################################/
# utils.class.class
#
# add interfaces to R6 class infrastructure
################################################################################/
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

################################################################################/
# utils.class.interface.class
#
# add interfaces to R6 class infrastructure
################################################################################/
#' Class: `utils.class.interface.class`
#' 
#' @description R6 class acting as base interface class.

utils.class.interface.class = R6::R6Class(
  "utils.class.interface.class",
  public = list(
    ############################################################################/
    # is.interface
    ############################################################################/
    #' @description Logical function indicating whether an object is an
    #'   interface.
    is.interface  = function() return( TRUE )
  )
)

################################################################################/
# utils.class.interface
# add interfaces to R6 class infrastructure
################################################################################/
#' utils.class.interface
#' 
#' Constructor function for objects of class [utils.class.interface.class]
#' 
#' @param interfacename Name of the interface. The interface name is useful
#'   primarily for S3 method dispatch.
#' @inheritParams R6::R6Class
#' 
#' @export

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

################################################################################/
# utils.class.interface.implements
# checks to see if an interface has been implemented
# check private internal variable directly to prevent accidental name mismatches
################################################################################/
#' utils.class.interface.implements
#'
#' @description Checks to see whether interface `interfaceName` has been
#'   implemented on object `object`.
#' 
#' 
#' @param object         R6 object of class `utils.class`.
#' @param interfaceName  Name of an interface to check for `object`.
#'
#' @export

utils.class.interface.implements = function(
    object,
    interfaceName
){
  if( !R6::is.R6( object ) | !inherits( object, "utils.class.class") )
    stop( "object must be from a class generated by utils.class()" )
  
  if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
    stop( "object must be from a class generated by utils.class()" )
  
  return( length( intersect( object$.__enclos_env__$private$.INTERNAL_INTERFACES, interfaceName ) ) == 1 )
}
