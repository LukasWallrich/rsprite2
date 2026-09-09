test_that("precision follows the reported statistic", {
  expect_equal(vapply(c(-.55, -3, 1e-5, 1e5, .1 + .2, 123456.789), .infer_prec, 0L),
               c(2, 0, 5, 0, 1, 3))
  expect_true(GRIM_test(-1.3, 3))
  expect_false(GRIM_test(1e-5, 10000, quiet = TRUE))
  expect_true(boundary_test(.71, 2, 1.5, 1, 2))
  expect_true(GRIMMER_test(1.5, .71, 2, min_val = 1, max_val = 2, quiet = TRUE))
  expect_true(boundary_test(3.1, 10, 4, 1, 7))
  expect_equal(GRIM_test(1.14, 8, return_values = TRUE, quiet = TRUE), 1.13)
  expect_equal(GRIM_test(1.07, 7, return_values = TRUE, quiet = TRUE), c(1, 1.14))
})

test_that("GRIMMER return modes agree including rounded zero and boundary failures", {
  cases <- list(
    list(mean = 1.33, sd = 0, n_obs = 3, n_items = 3, m_prec = 2, sd_prec = 2),
    list(mean = 1.1, sd = 0, n_obs = 10, m_prec = 1, sd_prec = 0),
    list(mean = 2, sd = 0, n_obs = 10),
    list(mean = 5.19, sd = 1.5, n_obs = 28),
    list(mean = 2, sd = 5, n_obs = 10, min_val = 1, max_val = 3),
    list(mean = 5.21, sd = 1.6, n_obs = 28),
    list(mean = 3.44, sd = 2.47, n_obs = 18),
    list(mean = 2, sd = 1, n_obs = 1)
  )
  for (args in cases) {
    args$quiet <- TRUE
    verdict <- do.call(GRIMMER_test, args)
    values <- do.call(GRIMMER_test, c(args, list(return_values = TRUE)))
    detailed <- do.call(GRIMMER_test, c(args, list(return_list = TRUE)))
    expect_type(verdict, "logical")
    expect_type(values, "double")
    expect_identical(detailed, list(passed = verdict, values = values))
    expect_identical(verdict, length(values) > 0)
  }
  expect_true(GRIMMER_test(1.33, 0, 3, n_items = 3, quiet = TRUE))
  expect_true(GRIMMER_test(1.1, 0, 10, sd_prec = 0, quiet = TRUE))
  expect_true(GRIMMER_test(1.83, .2, 9, n_items = 2, m_prec = 2, sd_prec = 1, quiet = TRUE))
})

test_that("statistical tests reject malformed inputs and preserve undefined range types", {
  for (bad in list(0, -1, NA_real_, Inf, 2.5)) {
    expect_error(GRIM_test(2, bad, quiet = TRUE))
    expect_error(GRIMMER_test(2, 1, 10, n_items = bad, quiet = TRUE))
  }
  for (bad in list(NA, c(TRUE, FALSE), 1)) {
    expect_error(GRIM_test(2, 10, return_values = bad))
    expect_error(GRIMMER_test(2, 1, 10, return_list = bad))
    expect_error(boundary_test(1, 10, 2, 1, 3, return_range = bad))
  }
  expect_error(GRIMMER_test(2, -1, 10))
  expect_error(GRIM_test(Inf, 10))
  expect_error(GRIM_test(1.23, 10, m_prec = 1))
  expect_error(boundary_test(.2, 20, 3, .5, 4.5))
  expect_error(boundary_test(1, 20, 3, 5, 1))
  expect_error(GRIMMER_test(2, 1, 10, min_val = 0))
  expect_identical(boundary_test(1.2, 20, 3.33, 1, 5, return_range = TRUE, quiet = TRUE),
                   c(NA_real_, NA_real_))
  expect_identical(boundary_test(1, 10, 8, 1, 5, return_range = TRUE, quiet = TRUE),
                   c(NA_real_, NA_real_))
  expect_no_warning(.sd_limits(28, 5.19, 1, 7, quiet = TRUE))
  expect_no_warning(.sd_limits(1, 2, 1, 3, quiet = TRUE))
  expect_error(GRIM_test(1e16, 10, quiet = TRUE), "reliable integer arithmetic")
  expect_true(GRIM_test(2, 1e9, m_prec = 0, quiet = TRUE))
  expect_error(GRIMMER_test(3.12, 1.53, 1e6, 2, 2, return_values = TRUE, quiet = TRUE),
               "More than one million candidates")
})

test_that("SD tests are invariant to large integer translations", {
  for (offset in c(-1e8, 0, 1e8)) {
    x <- c(1, 1, 2, 2) + offset
    expect_true(boundary_test(round(sd(x), 2), 4, mean(x), 1 + offset, 2 + offset,
                             m_prec = 1, sd_prec = 2))
    expect_true(GRIMMER_test(mean(x), round(sd(x), 2), 4,
                            m_prec = 1, sd_prec = 2, quiet = TRUE))
  }
})

test_that("translation preserves mean-rounding ties", {
  for (offset in c(-1e8, 0, 1e8)) for (m in seq(1.1, 1.9, .1)) {
    for (real_mean in c(m - .05, m + .05)) {
      total <- round(real_mean * 20)
      x <- c(rep(1, 40 - total), rep(2, total - 20))
      reported_sd <- round(sd(x), 2)
      expect_true(GRIMMER_test(offset + m, reported_sd, 20, m_prec = 1,
                              sd_prec = 2, quiet = TRUE))
      expect_true(boundary_test(reported_sd, 20, offset + m, offset + 1,
                               offset + 2, m_prec = 1, sd_prec = 2))
    }
  }
})

test_that("known samples pass GRIMMER and give exact SD bound extrema", {
  failures <- character()
  bound_failures <- character()
  checked <- 0L
  # Independently enumerate unordered samples using response frequencies.
  for (items in 1:3) for (n in 2:6) {
    lattice <- seq(0, 2, by = 1 / items)
    counts <- as.matrix(expand.grid(rep(list(0:n), length(lattice))))
    counts <- counts[rowSums(counts) == n, , drop = FALSE]
    samples <- lapply(seq_len(nrow(counts)), function(i) rep(lattice, counts[i, ]))
    means <- vapply(samples, mean, 0.0)
    sds <- vapply(samples, sd, 0.0)
    for (mp in 0:2) for (sp in 0:3) {
      cases <- unique(data.frame(m = round(means, mp), s = round(sds, sp)))
      for (i in seq_len(nrow(cases))) {
        checked <- checked + 1L
        if (!GRIMMER_test(cases$m[i], cases$s[i], n, mp, sp, items, quiet = TRUE)) {
          failures <- c(failures, paste(items, n, mp, sp, cases$m[i], cases$s[i]))
        }
      }
      for (m in unique(round(means, mp))) {
        compatible <- abs(means - m) <= .5 * 10^-mp + 1e-10
        expected <- c(round_down(min(sds[compatible]), sp), round_up(max(sds[compatible]), sp))
        actual <- .sd_limits(n, m, 0, 2, mp, sp, items, quiet = TRUE)
        if (!isTRUE(all.equal(actual, expected, tolerance = 1e-8))) {
          bound_failures <- c(bound_failures, paste(items, n, mp, sp, m))
        }
      }
    }
  }
  expect_equal(checked, 10489L)
  expect_equal(failures, character())
  expect_equal(bound_failures, character())
})
