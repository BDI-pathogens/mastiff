# TODO: Documentation for the base class (Possibly inherit from PRESTO doc
# branch)

###################################################################################/
# utils.class
#
# add interfaces to R6 class infrastructure
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
  if( !is.null( inherit ) )
  {
    if( inherit$inherit != "utils.class.parent" )
      stop( "inherited classes must be created by utils.class (i.e. so have inherited utils.class.class)" )
  }
  else
    inherit = utils.class.class
  
  # create an environment in the parent_env which just contains the name of the inherited generator
  envir = new.env( parent = parent_env )
  utils.class.parent = inherit
  assign( "utils.class.parent", utils.class.parent, envir = envir )
  
  # add interfaces to R6 class
  if( !is.list( interfaces ) )
    interfaces = list( interfaces )
  
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
  if( nInterfaces )
  {
    publicNames  <- names( publicMethods )
    privateNames <- names( privateMethods )
    activeNames  <- names( activeMethods )
    
    for( k in 1:nInterfaces )
    {
      if( interfaces[[ k ]]$inherit != "utils.class.interface.class" )
        stop( "interfaces must be inherited from utils.class.interface.class" )
      
      iName = interfaces[[ k ]]$classname
      
      # check public methods first
      for( iPublic in list( interfaces[[ k ]]$public_methods, interfaces[[ k ]]$public_fields ) )
        if( !is.null( iPublic ) )
          for( j in 1:length( iPublic ) )
          {
            iMethName =names( iPublic )[ j ]
            if( iMethName == "clone" )
              next();
            if( !( iMethName %in% publicNames ) )
              stop( sprintf( "must implement public method %s on interface %s", iMethName, iName ) )
            args  <- formalArgs( publicMethods[[ iMethName ]] )
            iArgs <- formalArgs( iPublic[[ iMethName ]] )
            if( !all( iArgs %in% args, args %in% iArgs ) )
              stop( sprintf( "incorrect arguments for public method %s on interface %s", iMethName, iName ) )
          }
      
      # check private methods
      for( iPrivate in list( interfaces[[ k ]]$private_methods, interfaces[[ k ]]$private_fields ) )
        if( !is.null( iPrivate ) )
          for( j in 1:length( iPrivate ) )
          {
            iMethName = names( iPrivate )[ j ]
            if( !( iMethName %in% privateNames ) )
              stop( sprintf( "must implement private method %s on interface %s", iMethName, iName ) )
            args  <- formalArgs( privateMethods[[ iMethName ]] )
            iArgs <- formalArgs( iPrivate[[ iMethName ]] )
            if( !all( iArgs %in% args, args %in% iArgs ) )
              stop( sprintf( "incorrect arguments for private method %s on interface %s", iMethName, iName ) )
          }
      
      # check active methods
      iActive = interfaces[[ k ]]$active
      if( !is.null( iActive ) & length( iActive ) > 0 )
        for( j in 1:length( iActive ) )
        {
          iMethName = names( iActive )[ j ]
          if( !( iMethName %in% activeNames ) )
            stop( sprintf( "must implement active field %s on interface %s", iMethName, iName ) )
          args  <- formalArgs( activeMethods[[ iMethName ]] )
          iArgs <- formalArgs( iActive[[ iMethName ]] )
          if( !all( iArgs %in% args, args %in% iArgs ) )
            stop( sprintf( "incorrect arguments for active method %s on interface %s", iMethName, iName ) )
          
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
    interfaces = function( val ) if( is.null( val ) ){
      return( private$.INTERNAL_INTERFACES )
    } else {
        stop( "cannot update interface list manually" )
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
){
  return( R6::R6Class(
    interfacename,
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
){
  if( !R6::is.R6( object ) | !inherits( object, "utils.class.class") )
    stop( "object must be from a class generated by utils.class()" )
  
  if( is.null( object$.__enclos_env__$private$.INTERNAL_INTERFACES ) )
    stop( "object must be from a class generated by utils.class()" )
  
  return( length( intersect( object$.__enclos_env__$private$.INTERNAL_INTERFACES, interfaceName ) ) == 1 )
}