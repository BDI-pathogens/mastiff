test_that( "Interface and class classnames are correctly assigned", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    private       = list( funcA = function( xA )     return( T ) ),
    public        = list( funcB = function( xB, yB ) return( T ) ),
    active        = list( funcC = function( xC )     return( T ) )
  )
  expect_equal( interfaceA$classname, "test_interface" )
  
  classA <- R6.class(
    classname  = "test_class",
    private    = list( funcA = function( xA )     return( T ) ),
    public     = list( funcB = function( xB, yB ) return( T ) ),
    active     = list( funcC = function( xC )     return( T ) ),
    interfaces = list( interfaceA )
  )
  expect_equal( classA$classname, "test_class" )
  expect_true( R6.interface.implements( classA$new(), "test_interface" ) )
})

test_that( "A single interface can be passed in without wrapping in a list", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    private       = list( funcA = function( xA )     return( T ) ),
    public        = list( funcB = function( xB, yB ) return( T ) ),
    active        = list( funcC = function( xC )     return( T ) )
  )
  
  expect_no_error(
    R6.class(
      classname  = "test_class",
      private    = list( funcA = function( xA )     return( T ) ),
      public     = list( funcB = function( xB, yB ) return( T ) ),
      active     = list( funcC = function( xC )     return( T ) ),
      interfaces = interfaceA
    )
  )
})

test_that( "Interface forces class to define all methods on interface", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    private       = list( funcA = function( xA )     return( T ) ),
    public        = list( funcB = function( xB, yB ) return( T ),
                          funcC = function( xC )     return( T ) ),
    active        = list( funcD = function( xD )     return( T ) )
  )
  
  # Derived class exactly matches interface
  expect_no_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class has additional methods on top of interface requirements
  expect_no_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ),
                            funcE = function( xE )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ),
                            funcF = function( xF )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ),
                            funcG = function( xG )     return( T) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class does not define private method funcA
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list(),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class does not define public method funcB
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcC = function( xC )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class does not define public method funcC
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class does not define active binding funcD
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list(),
      interfaces = list( interfaceA )
    )
  )
})

test_that( "Interface forces function signature of class' methods to contain required arguments", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    private       = list( funcA = function( xA )     return( T ) ),
    public        = list( funcB = function( xB, yB ) return( T ),
                          funcC = function( xC )     return( T ) ),
    active        = list( funcD = function( xD )     return( T ) )
  )
  
  # Derived class exactly matches interface
  expect_no_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class missing function input on funcA
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function()         return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class missing function input xB on funcB
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA ) return( T ) ),
      public        = list( funcB = function( yB ) return( T ),
                            funcC = function( xC ) return( T ) ),
      active        = list( funcD = function( xD ) return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class is missing argument yB on funcB
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA ) return( T ) ),
      public        = list( funcB = function( xB ) return( T ),
                            funcC = function( xC ) return( T ) ),
      active        = list( funcD = function( xD )    return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class missing function input on funcC
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function()     return( T ) ),
      active        = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
  
  # Derived class missing function input on funcD
  expect_error(
    R6.class(
      classname = "test_class",
      private       = list( funcA = function( xA )     return( T ) ),
      public        = list( funcB = function( xB, yB ) return( T ),
                            funcC = function( xC )     return( T ) ),
      active        = list( funcD = function()     return( T ) ),
      interfaces = list( interfaceA )
    )
  )
})

test_that( "Class may include additional optional arguments compared to interface", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    public        = list( funcA = function( xA ) return( T ) )
  )
  
  # Derived class with optional numeric argument
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, yA = 1 ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  # Derived class with optional logical argument
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, yA = TRUE ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  # Derived class with additional required argument
  expect_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, yA ) return( T ) ),
      interfaces = interfaceA
    )
  )
})

test_that( "Multiple interfaces can be enforced on a class", {
  interfaceA <- R6.interface( 
    interfacename = "test_interface",
    private = list( funcA = function( xA )     return( T ) ),
    public  = list( funcB = function( xB, yB ) return( T ),
                    funcC = function( xC )     return( T ) ),
    active  = list( funcD = function( xD )     return( T ) )
  )
  
  interfaceB <- R6.interface( 
    interfacename = "test_interface2",
    public  = list( funcE = function( xE ) return( T ) ),
  )
  
  # All methods are fully defined on both interfaces
  expect_no_error( 
    R6.class(
      classname = "test_class",
      private = list( funcA = function( xA )     return( T ) ),
      public  = list( funcB = function( xB, yB ) return( T ),
                      funcC = function( xC )     return( T ),
                      funcE = function( xE )     return( T ) ),
      active  = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA, interfaceB )
    )
  )
  
  # Missing public method funcC for interfaceA
  expect_error( 
    R6.class(
      classname = "test_class",
      private = list( funcA = function( xA )     return( T ) ),
      public  = list( funcB = function( xB, yB ) return( T ),
                      funcE = function( xE )     return( T ) ),
      active  = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA, interfaceB )
    )
  )
  
  # Missing public method funcE for interfaceB
  expect_error(
    R6.class(
      classname = "test_class",
      private = list( funcA = function( xA )     return( T ) ),
      public  = list( funcB = function( xB, yB ) return( T ),
                      funcC = function( xC )     return( T ) ),
      active  = list( funcD = function( xD )     return( T ) ),
      interfaces = list( interfaceA, interfaceB )
    )
  )
})

test_that( "Incompatible interfaces throw an error", {
  # If two interfaces define incompatible function signatures, a class calling
  # both interfaces shouldn't be created
  interfaceA <- R6.interface( 
    interfacename = "test_interface",
    public  = list( funcA = function( xA ) return( T ) )
  )
  
  interfaceB <- R6.interface( 
    interfacename = "test_interface",
    public  = list( funcA = function( xB ) return( T ) )
  )
  
  # Can create class with interfaceA alone or interfaceB alone
  expect_no_error(
    R6.class(
      classname = "test_class",
      public    = list( funcA = function( xA ) return( T ) ),
      interface = interfaceA
    )
  )
  
  expect_no_error(
    R6.class(
      classname = "test_class",
      public    = list( funcA = function( xB ) return( T ) ),
      interface = interfaceB
    )
  )
  
  # ...but cannot create a class with interfaceA and interfaceB
  expect_error(
    R6.class(
      classname = "test_class",
      public    = list( funcA = function( xA ) return( T ) ),
      interface = list( interfaceA, interfaceB )
    )
  )
})

test_that( "Arguments of class methods can be defined in any order", {
  interfaceA <- R6.interface(
    interfacename = "test_interface",
    public        = list( funcA = function( xA, yA ) return( T ) )
  )
  
  # funcA arguments in the same order as the interface
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, yA ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  # funcA arguments in the opposite order to the interface
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( yA, xA ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  # funcA arguments in the same order as the interface + optional argument
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, yA, zA = 1 ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( xA, zA = 1, yA ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( zA = 1, xA, yA ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  # funcA arguments in the opposite order to the interface
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( yA, xA, zA = 1 ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( yA, zA = 1, xA ) return( T ) ),
      interfaces = interfaceA
    )
  )
  
  expect_no_error(
    R6.class(
      classname  = "test_class",
      public     = list( funcA = function( zA = 1, yA, xA ) return( T ) ),
      interfaces = interfaceA
    )
  )
})

######################

test_that( "Derived classes inherit and check interfaces from parent class", {
  interfaceA <- R6.interface( 
    interfacename = "interfaceA",
    public  = list( funcA = function( xA ) return( T ) )
  )
  
  classA <- R6.class(
    classname = "classA",
    public = list( funcA = function( xA ) return( T ) ),
    interfaces = list( interfaceA )
  )
  
  # Derived class correct implements interfaceA
  expect_no_error(
    R6.class(
      classname = "classB",
      inherit   = classA,
      public    = list( funcA = function( xA ) return( F ) )
    )
  )
  
  expect_false({
    classB <- R6.class(
      classname = "classB",
      inherit   = classA,
      public    = list( funcA = function( xA ) return( F ) )
    )
    classB$new()$funcA()
  })
  
  # Derived class inherits public method funcA from classA without redefining
  expect_no_error(
    R6.class(
      classname = "classB",
      inherit = classA,
      public  = list( funcB = function( x ) return( T ) )
    )
  )
  
  expect_true({
    classB <- R6.class(
      classname = "classB",
      inherit   = classA,
      public  = list( funcB = function( x ) return( T ) )
    )
    classB$new()$funcA()
  })
  
  # Derived class has incorrect signature for funcA
  expect_error(
    R6.class(
      classname = "classB",
      inherit = classA,
      public  = list( funcA = function( x ) return( T ) )
    )
  )
})
