test_that("GRIM works", {
  expect_false(GRIM_test(5.19, 28))
  expect_true(GRIM_test(5.18, 14, n_items = 2))
  expect_warning(closest <- GRIM_test(5.19, 28, return_values = TRUE))
  expect_equal(closest, 5.18)
})

set.seed(1234)
vec <- sample(c(1:6, 8:9), 25, replace = TRUE)
vec <- .adjust_mean(1000, vec, c(1, 7,7,7,7, 9), 3.5, 1, c(1:6, 8:9))

test_that("Adjusting mean works", {
  expect_equal(mean(vec), 2.88)
  expect_equal(sum(vec==7), 0)
})

test_that("Set differences found", {
  expect_equal(.get_diffs(c(1, 2, 3, 3), c(1, 2)), c(3, 3))
  expect_equal(.get_diffs(c(1, 2), c(1, 2, 7,7)), numeric())
})

restrictions <- list("2.33"=1, "4"=3, "4.67" = 0)
parameters <- set_parameters(3.40, 1.15, 15, 1, 5, m_prec = 2, n_items = 3,
                             restrictions_exact = restrictions,
                             restrictions_minimum = "range")

test_that("Samples are found", {
  expect_snapshot_value(find_possible_distributions(parameters, 5, seed = 5678,
                                                    return_tibble = FALSE), style = "serialize")
})
