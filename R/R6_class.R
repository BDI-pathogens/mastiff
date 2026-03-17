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



################################################################################/
# R6.class
################################################################################/
##### NOTE: All derived R6 classes using interfaces in mastiff should include
##### this file via Roxygen using the include tag: #' @include R6_util_class.R
##### to update the collate field in DESCRIPTION.
#####
##### Typically this is only for safety, but if a derived class is defined with
##### a name alphabetically before R6_util_class.R and included in another file,
##### the collate order might matter, e.g. R6_a_new_class.R might break the
##### collate order if #' @include R6_a_new_class.R is ever used.

#' Class: R6.class
#' 
#' @description R6 object extending [R6::R6Class()] to include interfaces.
#'
#' @inheritParams R6::R6Class
#' @param interfaces  An optional list of interfaces implemented for the derived
#'   class.
#'
#' @export

R6.class = function(
    classname = NULL,
    public    = list(),
    private   = NULL,
    active    = NULL,
    inherit   = NULL,
    interfaces = list(),
    lock_objects = TRUE,
    class      = TRUE,
    portable   = TRUE,
    lock_class = FALSE,
    cloneable  = TRUE,
    parent_env = parent.frame()
){
  ##############################################################################/
  # Validate that all methods defined on an interface are implemented on derived
  # class
  #
  # iMethod_list: list of methods defined on the interface to validate against
  # method_list:  list of methods defined on the class to be validated
  # iName:        name of the interface
  # method_type:  type of method being validated; used for informative error
  # messages
  #
  # Note: Variable names with prefix i are related to the interface not the
  # defined class.
  .validate_interface_method_args <- function( method_list, iMethod_list, iName,
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
    return( invisible() )
  }
  
  # Validate that a pair of interfaces do not require incompatible methods
  #
  # methodList1: list of methods to validate on interface1
  # methodList2: list of methods to validate on intereface2
  # iName1:      name of interface1
  # iName2:      name of interface2
  # method_type: type of method being validated; used for informative
  #   error messages
  .validate_interface_methods <- function( methodList1, methodList2,
                                           iName1,      iName2,
                                           error_type = "public method" ){
    methShared <- intersect( names( methodList1 ),
                             names( methodList2 ) )
    for ( methName in methShared ){
      # clone method must exist on R6 class and does not need to be validated
      if ( methName == "clone" ) next
      
      args1 <- formalArgs( methodList1[[ methName ]] )
      args2 <- formalArgs( methodList2[[ methName ]] )
      
      if ( length( setdiff( args1, args2 ) ) )
        stop( sprintf( "incompatible arguments for %s %s on interfaces %s and %s",
                       error_type, methName,
                       iName1, iName2 ) )
    }
    return( invisible() )
  }
  ##############################################################################/
  
  # check to see an inherited class has been created by R6.class
  if( !is.null( inherit ) ){
    if( inherit$inherit != "R6.class.parent" )
      stop( "inherited classes must be created by R6.class (i.e. must inherit R6.class.class)" )
  } else{
    inherit = R6.class.class
  }
  
  # create an environment in the parent_env which just contains the name of the
  # inherited generator
  envir = new.env( parent = parent_env )
  R6.class.parent = inherit
  assign( "R6.class.parent", R6.class.parent, envir = envir )
  
  # add interfaces to R6 class
  if( !is.list( interfaces ) ) interfaces = list( interfaces )
  if ( !is.null( inherit$private_fields$.INTERNAL_INTERFACES ) ){
    interfaces <- c( inherit$private_fields$.INTERNAL_INTERFACES,
                     interfaces )
  }
  
  # if an interface is included multiple times, keep the first instance only
  interfaces <- interfaces[ !duplicated( interfaces ) ]
  
  # if at least 2 interfaces are defined, check that multiple interfaces don't
  # define incompatible method signatures
  nInterfaces <- length( interfaces )
  if ( nInterfaces >= 2 ){
    for ( idx in 1 : ( nInterfaces - 1 ) ){
      interface1 <- interfaces[[ idx ]]
      for ( jdx in 2 : nInterfaces ){
        interface2 <- interfaces[[ jdx ]]
        .validate_interface_methods(
          methodList1 = c( interface1$public_fields, interface1$public_methods ),
          methodList2 = c( interface2$public_fields, interface2$public_methods ),
          iName1      = interface1$classname,
          iName2      = interface2$classname,
          error_type  = "public method"
        )
      }
    }
  }
  
  # if inheriting a class, we need to include all inherited methods
  if ( is.null( inherit$public_methods ) ){
    publicMethods <- public
  } else {
    publicMethods <- utils::modifyList( inherit$public_methods,
                                        public )
  }
  
  if ( is.null( private ) ) private <- list()
  if ( is.null( inherit$private_methods ) ){
    privateMethods <- private
  } else {
    privateMethods <- utils::modifyList( inherit$private_methods,
                                         private )
  }
  
  if ( is.null( active ) ) active <- list()
  if ( is.null( inherit$active_methods ) ){
    activeMethods <- active
  } else {
    activeMethods <- utils::modifyList( inherit$active,
                                        active )
  }
  
  # Check that new class defines all methods specified by all interfaces with
  # the correct required arguments
  for ( interface in interfaces ){
    if ( interface$inherit != "R6.interface.class")
      stop( "Interfaces must be created by R6.interface (i.e. must inherit R6.interface.class" )
    
    iName <- interface$classname
    
    # Validate public methods
    .validate_interface_method_args(
      method_list = publicMethods,
      iMethod_list = list( interface$public_methods,
                           interface$public_fields ),
      iName,
      error_type = "public method"
    )
    
    # Validate private methods
    .validate_interface_method_args(
      method_list = privateMethods,
      iMethod_list = list( interface$private_methods,
                           interface$private_fields ),
      iName,
      error_type = "private method"
    )
    
    # Validate active methods
    .validate_interface_method_args(
      method_list = activeMethods,
      iMethod_list = list( interface$active ),
      iName,
      error_type = "active field"
    )
  }
  
  # Implicit assumption: If no error has been hit up to this point, all
  # interfaces are feasible for derived class and correctly implemented
  private$.INTERNAL_INTERFACES <- interfaces
  active$interfaces = function() sapply( private$.INTERNAL_INTERFACES,
                                         function( x ) x$classname )
  
  return( R6::R6Class( classname    = classname,
                       public       = public,
                       private      = private,
                       active       = active,
                       inherit      = R6.class.parent,
                       lock_objects = lock_objects,
                       class        = class,
                       portable     = portable,
                       lock_class   = lock_class,
                       cloneable    = cloneable,
                       parent_env   = envir ) )
}

################################################################################/
# R6.class.class
#
# add interfaces to R6 class infrastructure
################################################################################/
R6.class.class = R6::R6Class(
  "R6.class.class",
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
# R6.interface.class
#
# add interfaces to R6 class infrastructure
################################################################################/
#' Class: `R6.interface.class`
#' 
#' @description R6 class acting as base interface class.

R6.interface.class = R6::R6Class(
  "R6.interface.class",
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
# R6.interface
# add interfaces to R6 class infrastructure
################################################################################/
#' R6.interface
#' 
#' Constructor function for an interface for use with [R6.class]
#' 
#' @param interfacename Name of the interface. The interface name is useful
#'   primarily for S3 method dispatch.
#' @inheritParams R6::R6Class
#' 
#' @returns Object of class [R6.interface.class]
#' 
#' @export

R6.interface = function(
    interfacename = NULL,
    public = list(),
    private = list(),
    active = list()
){
  return( R6::R6Class( classname = interfacename,
                       public    = public,
                       private   = private,
                       active    = active,
                       inherit   = R6.interface.class ) )
}

################################################################################/
# R6.class.interface.implements checks to see if an interface has been
# implemented check private internal variable directly to prevent accidental
# name mismatches
################################################################################/
#' R6.interface.implements
#'
#' @description Checks to see whether interface `interfaceName` has been
#'   implemented on object `object`.
#' 
#' 
#' @param object         R6 object of class `R6.class`.
#' @param interfaceName  Name of an interface to check for `object`.
#'
#' @export

R6.interface.implements = function(
    object,
    interfaceName
){
  if( !R6::is.R6( object ) | !inherits( object, "R6.class.class") )
    stop( "object must be from a class generated by R6.class()" )
  
  if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
    stop( "object must be from a class generated by R6.class()" )
  
  return( length( intersect( object$interfaces, interfaceName ) ) == 1 )
}
