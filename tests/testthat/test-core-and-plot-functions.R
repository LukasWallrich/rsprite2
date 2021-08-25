test_that("GRIM works", {
  expect_false(GRIM_test(5.19, 28))
  expect_true(GRIM_test(5.18, 14, n_items = 2))
  expect_warning(closest <- GRIM_test(5.19, 28, return_values = TRUE))
  expect_equal(closest, 5.18)
  expect_equal(GRIM_test(5.2, 20, return_values = TRUE), c(5.2, 5.25, 5.15))
})

set.seed(1234)
vec <- sample(c(1:6, 8:9), 25, replace = TRUE)
vec <- .adjust_mean(1000, vec, c(1, 7, 7, 7, 7, 9), 3.5, 1, c(1:6, 8:9))

test_that("Adjusting mean works", {
  expect_equal(mean(vec), 2.88)
  expect_equal(sum(vec == 7), 0)
})

restrictions <- list("2.33" = 1, "4" = 3, "4.67" = 0)
parameters <- set_parameters(3.40, 1.15, 15, 1, 5,
  m_prec = 2, n_items = 3,
  restrictions_exact = restrictions,
  restrictions_minimum = "range"
)

if (requireNamespace("tibble", quietly = TRUE)) {
  poss <- find_possible_distributions(parameters, 5, seed = 5678)

  test_that("Samples are found", {
    expect_class(poss, "sprite_distributions")
    expect_snapshot_value(poss, style = "serialize")
  })
}

### Test plot (continued here to avoid creating poss twice)

req_packages <- c("ggplot2", "tidyr", "tibble", "scales")

if (all(requireNamespace(req_packages, quietly = TRUE))) {
  p <- plot_distributions(poss, plot_type = "ecdf")

  test_that("ecdf plot is produced", {
    expect_class(p$layers[[1]]$stat, "StatEcdf")
    expect_identical(p$scales$scales[[1]]$limits, c(1, 5))
  })

    p <- plot_distributions(poss, plot_type = "density")

  test_that("density plot is produced", {
    expect_class(p$layers[[1]]$stat, "StatDensity")
    expect_identical(p$scales$scales[[1]]$limits, c(1, 5))
  })

  p <- plot_distributions(poss)

  test_that("histograms are produced", {
    expect_class(p$layers[[1]]$stat, "StatBin")
    expect_identical(p$scales$scales[[1]]$limits, c(1, 5))
    expect_identical(p$facet$params$nrow, 3)
  })
}
