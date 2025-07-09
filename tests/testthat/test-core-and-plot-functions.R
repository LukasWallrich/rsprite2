test_that("GRIM works as expected", {
  # Basic pass/fail
  expect_false(GRIM_test(5.19, 28))
  expect_true(GRIM_test(5.18, 14, n_items = 2))

  # Return closest value on failure
  expect_warning(closest <- GRIM_test(5.19, 28, return_values = TRUE))
  expect_equal(closest, 5.18)

  # Return two closest values on failure
  expect_warning(
    expect_equal(GRIM_test(5.83, 50, return_values = TRUE), c(5.82, 5.84))
  )

  # Return list on failure
  expect_equal(
    GRIM_test(5.83, 50, return_list = TRUE),
    list(passed = FALSE, values = c(5.82, 5.84))
  )

  # Return multiple values on success
  expect_equal(
    GRIM_test(5.2, 20, return_values = TRUE, quiet = TRUE),
    c(5.15, 5.2, 5.25)
  )

  # Granularity warning for large N
  expect_warning(
    GRIM_test(2.5, 200),
    "sample size.*too big"
  )
})

test_that("GRIMMER works as expected", {
  # Basic pass/fail
  expect_true(GRIMMER_test(5.21, 1.6, 28))
  expect_false(GRIMMER_test(3.44, 2.47, 18))

  # Fails if underlying GRIM test fails
  expect_warning(
    expect_false(GRIMMER_test(5.19, 1.5, n_obs = 28)),
    "GRIM test failed"
  )

  # Returns values on success in rare cases that there are more than 1

  expect_warning(expect_equal(
    round(GRIMMER_test(
    mean         = 3.33,   # reported to 2 d.p.
    sd           = 1.234,  # reported to 3 d.p.
    n_obs        = 250,    # > 10^m_prec so two integer sums are possible
    n_items      = 1,
    return_values = TRUE
  ), 4),
  c(1.2339, 1.2345)))

  # Returns empty vector on failure
  expect_length(
    GRIMMER_test(3.44, 2.47, 18, return_values = TRUE),
    0
  )

  # Returns list on success
  expect_equal(
    GRIMMER_test(5.21, 1.6, 28, return_list = TRUE),
    list(passed = TRUE, values = numeric(0))
  )
  # n_obs < 2 (SD is undefined)
  expect_warning(
    expect_false(GRIMMER_test(2, 1, n_obs = 1)),
    "single observation, SD is undefined"
  )
  expect_warning(expect_equal(
    GRIMMER_test(2, 1, n_obs = 1, return_list = TRUE),
    list(passed = FALSE, values = numeric(0))
  ))
})

test_that(".sd_limits calculates ranges correctly", {
  # Standard cases
  expect_equal(
    .sd_limits(n_obs = 5, mean = 4.2, min_val = 1, max_val = 7, sd_prec = 2),
    c(0.45, 3.03),
    tolerance = 1e-3
  )
  expect_equal(
    .sd_limits(n_obs = 5, mean = 4.2, min_val = 1, max_val = 7, sd_prec = 2, n_items = 2),
    c(0.27, 3.03),
    tolerance = 1e-3
  )

  # Edge cases
  expect_equal(.sd_limits(n_obs = 100, mean = 1, m_prec = 2, min_val = 1, max_val = 7), c(0, 0))
  expect_equal(.sd_limits(n_obs = 100, mean = 7, m_prec = 2, min_val = 1, max_val = 7), c(0, 0))
  expect_equal(.sd_limits(n_obs = 10, mean = 5, m_prec = 1, min_val = 5, max_val = 5), c(0, 0))

  # Fails if GRIM test fails
  expect_warning(
    result <- .sd_limits(n_obs = 28, mean = 5.19, min_val = 1, max_val = 7),
    "`GRIM_test` failed"
  )
  expect_true(all(is.na(result)))

  # quiet argument suppresses granularity warning
  expect_no_warning(
    .sd_limits(n_obs = 101, mean = 2.5, min_val = 1, max_val = 5, sd_prec = 1, quiet = TRUE)
  )
})

test_that("boundary_test works as expected", {
  # Plausible SD should pass
  expect_true(
    boundary_test(sd = 1.5, n_obs = 20, mean = 4, min_val = 1, max_val = 7)
  )

  # Impossible SD should fail
  expect_false(
    boundary_test(sd = 5, n_obs = 20, mean = 4, min_val = 1, max_val = 7)
  )

  # Fails if the mean is impossible (outside the scale)
  expect_warning(expect_false(
    boundary_test(sd = 1.5, n_obs = 20, mean = 8, min_val = 1, max_val = 7)
  ))

  # Fails if the mean fails GRIM test
  expect_warning(expect_false(
    boundary_test(sd = 1.5, n_obs = 28, mean = 5.19, min_val = 1, max_val = 7)
  ))

  # Confirms that granularity warning is suppressed during internal calls
  expect_no_warning(
    boundary_test(sd = 1, n_obs = 101, mean = 2.5, min_val = 1, max_val = 5, sd_prec = 1)
  )
})


set.seed(1234)
vec <- sample(c(1:6, 8:9), 25, replace = TRUE)
vec <- .adjust_mean(1000, vec, c(1, 7, 7, 7, 7, 9), 3.5, 1, c(1:6, 8:9))

test_that("Adjusting mean works", {
  expect_equal(mean(vec), 2.88)
  expect_equal(sum(vec == 7), 0)
})

test_that("Example set_parameters() run works", {
  expect_no_error(
    set_parameters(mean = 1.95, sd = 1.55, n_obs = 20,
                   min_val = 1, max_val = 5, n_items = 3,
                   restrictions_exact = list("3"=0, "3.67" = 2),
                   restrictions_minimum = list("1" = 1, "5" = 1))
  )
})

restrictions <- list("2.33" = 1, "4" = 3, "4.67" = 0)
parameters <- set_parameters(3.29, 1.29, 15, 1, 5,
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
