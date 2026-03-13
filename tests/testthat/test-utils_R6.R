test_that( "Test interface and class names", {
  interfaceA <- utils.class.interface( 
    interfacename = "test_interface",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) )
  )
  expect_equal( interfaceA$classname, "test_interface" )
  
  classA <- utils.class(
    classname = "test_class",
    private = list( funcA = function( x ) return( T ) ),
    public  = list( funcB = function( x,y ) return( T ), funcE = function( x ) return( T ) ),
    active  = list( funcC = function( x ) return( T ) ),
    interfaces = list( interfaceA )
  )
  
  expect_equal( classA$classname, "test_class" )
  expect_equal( utils.class.interface.implements( classA$new(), "test_interface"), TRUE ) 
} )

test_that( "Test class requires the methods on the interface", {
  interfaceA <- utils.class.interface(
    interfacename = "test_interface",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) )
  )
  
  # Incorrect private method name
  expect_error( utils.class(
    classname = "test_class",
    private = list( funcD = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) ),
    interfaces = list( interfaceA )
  ) )
  
  # Incorrect public method name
  expect_error( utils.class(
    classname = "test_class",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcD = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) ),
    interfaces = list( interfaceA )
  ) )
  
  # Incorrect public method funcE signature
  expect_error( utils.class(
    classname = "test_class",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x )    return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) ),
    interfaces = list( interfaceA )
  ) )
  
  # Missing public method funcE
  expect_error( utils.class(
    classname = "test_class",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ) ),
    active  = list( funcC = function( x )    return( T ) ),
    interfaces = list( interfaceA )
  ) )
  
  # Incorrect active binding name
  expect_error( utils.class(
    classname = "test_class",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcD = function( x )    return( T ) ),
    interfaces = list( interfaceA )
  ) )
} )

test_that( "Test class with 2 interfcaes", {
  interfaceA <- utils.class.interface( 
    interfacename = "test_interface",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) )
  )
  
  interfaceB <- utils.class.interface( 
    interfacename = "test_interface2",
    public  = list( funcF = function( x )    return( T ) ),
  )
  
  classF <- utils.class(
    classname = "test_class",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ),
                    funcF = function( x )    return( T )  ),
    active  = list( funcC = function( x )    return( T ) ),
    interfaces = list( interfaceA, interfaceB )
  )
  
  expect_equal( R6::is.R6Class( classF ), TRUE ) 
  
  # Missing public method funcE for interfaceA
  expect_error(
    utils.class(
      classname = "test_class",
      private = list( funcA = function( x )    return( T ) ),
      public  = list( funcB = function( x, y ) return( T ), 
                      funcF = function( x )    return( T ) ),
      active  = list( funcC = function( x )    return( T ) ),
      interfaces = list( interfaceA, interfaceB )
    ) )
  
  # Missing public method funcF for interfaceB
  expect_error(
    utils.class(
      classname = "test_class",
      private = list( funcA = function( x )    return( T ) ),
      public  = list( funcB = function( x, y ) return( T ), 
                      funcE = function( x )    return( T ) ),
      active  = list( funcC = function( x )    return( T ) ),
      interfaces = list( interfaceA, interfaceB )
    ) )
})

test_that("Test class method arguments can be defined in any order", {
  interfaceC <- utils.class.interface(
    interfacename = "test_interface3",
    public  = list( funcH = function( x, y ) return( T ) ),
  )
  
  # Define method arguments in the same order as interface
  expect_no_error( utils.class(
    classname = "test_class",
    public  = list( funcH = function( x, y ) return( T ) ),
    interfaces = list( interfaceC )
  )
  )
  
  # Define method arguments in different order to interface
  expect_no_error( utils.class(
    classname = "test_class",
    public  = list( funcH = function( y, x ) return( T ) ),
    interfaces = list( interfaceC )
  ) )
})

test_that("Test class methods match interface arguments exactly", {
  interfaceC <- utils.class.interface(
    interfacename = "test_interface3",
    public  = list( funcH = function( x ) return( T ) ),
  )
  
  # Additional argument y in public method funcH
  expect_error(
    utils.class(
      classname = "test_class",
      public    = list( funcH = function( x, y ) return( T ) ),
      interfaces = list( interfaceC )
    )
  )
  
  expect_error(
    utils.class(
      classname = "test_class",
      public    = list( funcH = function( ) return( T ) ),
      interfaces = list( interfaceC )
    )
  )
})

test_that("Test interface on derived class checks base methods", {
  interfaceA <- utils.class.interface( 
    interfacename = "test_interface",
    private = list( funcA = function( x )    return( T ) ),
    public  = list( funcB = function( x, y ) return( T ),
                    funcE = function( x )    return( T ) ),
    active  = list( funcC = function( x )    return( T ) )
  )
  
  expect_no_error(
    utils.class(
      classname = "test_class",
      private = list( funcA = function( x )    return( T ) ),
      public  = list( funcB = function( x, y ) return( T ),
                      funcE = function( x )    return( T ) ),
      active  = list( funcC = function( x )    return( T ) )
    ) )
} )