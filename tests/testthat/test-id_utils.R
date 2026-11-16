test_that("small n works as expected", {
  expect_identical(get_n_character_ids(0), character(0))
  expect_identical(get_n_character_ids(1), "a")
  expect_identical(get_n_character_ids(5), letters[1:5])
  expect_identical(get_n_character_ids(26), letters[1:26])
  expect_identical(get_n_character_ids(28),
                   c(paste0("a", letters[1:26]),
                     paste0("b", letters[1:2])))
  expect_identical(get_n_character_ids(54),
                   c(paste0("a", letters[1:26]),
                     paste0("b", letters[1:26]),
                     paste0("c", letters[1:2])))
  expect_identical(
    get_n_character_ids(26^2 + 1),
    c(paste0("a", as.vector(t(outer(letters[1:26], letters[1:26],
                                    FUN = paste, sep = "")))),
      "baa"))

})

test_that("error messages are triggered", {
  expect_error(get_n_character_ids("foo"),
               regexp = "is\\.numeric\\(x\\) is not TRUE")
  expect_error(get_n_character_ids(1:2),
               regexp = "length\\(x\\) == 1 is not TRUE")
  expect_error(get_n_character_ids(-1),
               regexp = "x >= lower is not TRUE")
})
