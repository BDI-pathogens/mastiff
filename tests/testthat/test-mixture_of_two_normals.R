test_that("simulating with default params gives no error", {
  expect_no_error(simulate_mixture_of_two_normals())
})

test_that("simulating with bad args gives the expected error", {
  expect_error(simulate_mixture_of_two_normals(n = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(n = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(n = -1), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(mu_0 = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(mu_0 = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(mu_1 = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(mu_1 = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(mu_1 = -Inf), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_0 = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_0 = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_0 = -Inf), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_1 = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_1 = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_1 = -Inf), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(p = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(p = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(p = -1), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(p = 2), fixed = TRUE,
               regexp = "x <= upper is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_groups = "foo"), fixed = TRUE,
               regexp = "is.numeric(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_groups = NA_real_), fixed = TRUE,
               regexp = "!is.na(x) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(sd_groups = -Inf), fixed = TRUE,
               regexp = "x >= lower is not TRUE")
  expect_error(simulate_mixture_of_two_normals(groups = 1:2), fixed = TRUE,
               regexp = "is.character(groups) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(groups = c("A", "A")), fixed = TRUE,
               regexp = "!anyDuplicated(groups) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(group_frequencies = "foo"), fixed = TRUE,
               regexp = "is.numeric(group_frequencies) is not TRUE")
  expect_error(simulate_mixture_of_two_normals(groups = c("A", "B"),
                                               group_frequencies = 1),
               fixed = TRUE,
               regexp = "length(group_frequencies) == length(groups) is not TRUE")

  expect_error(simulate_mixture_of_two_normals(groups = c("A", "B"),
                                               group_frequencies = 1:2),
               fixed = TRUE,
               regexp = "sum(group_frequencies) == 1 is not TRUE")
  expect_error(simulate_mixture_of_two_normals(groups = c("A", "B"),
                                               group_frequencies = c(-1, 2)),
               fixed = TRUE,
               regexp = "all(group_frequencies >= 0) is not TRUE")
})

test_that("simulating 2 groups gives results with the right structure", {
  results <- simulate_mixture_of_two_normals(n = 100, groups = c("A", "B"), p = 0.5)
  expect_equal(names(results), c("params", "data"))
  expect_equal(names(results$data), c("group", "d", "y"))
  expect_equal(names(results$params), c("mu_0", "mu_1", "sd_0", "sd_1", "sd_groups", "p_for_A", "p_for_B"))
  expect_equal(length(unique(results$data$group)), 2) # prob this will fail by chance = 2^-99
})

test_that("simulating no groups gives results with the right structure", {
  # specify no groups with a length-zero character vector
  results <- simulate_mixture_of_two_normals(groups = character())
  expect_equal(names(results), c("params", "data"))
  expect_equal(names(results$data), c("d", "y"))
  expect_equal(names(results$params), c("mu_0", "mu_1", "sd_0", "sd_1", "p", "sd_groups"))
  expect_equal(length(unique(results$data$d)), 2) # prob this will fail by chance = 2^-99
  # specify no groups with a length-one character vector
  results <- simulate_mixture_of_two_normals(groups = "foo")
  expect_equal(names(results), c("params", "data"))
  expect_equal(names(results$data), c("d", "y"))
  expect_equal(names(results$params), c("mu_0", "mu_1", "sd_0", "sd_1", "p", "sd_groups"))
  expect_equal(length(unique(results$data$d)), 2) # prob this will fail by chance = 2^-99
})

test_that("simulating with p = 1 (or 0) results in all (or no) observations having d = 1", {
  results <- simulate_mixture_of_two_normals(groups = character(), p = 0)
  expect_true(all(results$data$d == FALSE))
  results <- simulate_mixture_of_two_normals(groups = character(), p = 1)
  expect_true(all(results$data$d == TRUE))
})

test_that("simulating a one-component mixture with no width gives a Dirac delta function", {
  mu_0 <- 5
  expect_true(all(simulate_mixture_of_two_normals(p = 0, sd_0 = 0, mu_0 = mu_0)$y == mu_0))
  mu_1 <- 10
  expect_true(all(simulate_mixture_of_two_normals(p = 1, sd_1 = 0, mu_1 = mu_1)$y == mu_1))
})
