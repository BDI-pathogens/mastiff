test_that( "distribution.abstract.class can not be created on its own", {
  expect_error( distribution.abstract.class$new() )
})

test_that( "A derived distribution returns errors on all interface functions assuming they are not redefined", {
  distribution.test.class <- R6.class(
    classname = "distribution.test.class",
    inherit = distribution.abstract.class,
    private = list(),
    public  = list(
      initialize = function(){ NULL }
    ),
    active  = list()
  )
  
  X <- distribution.test.class$new()
  expect_error( X$d() )
  expect_error( X$p() )
  expect_error( X$q() )
  expect_error( X$r() )
})
