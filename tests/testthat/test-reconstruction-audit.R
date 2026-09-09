expect_reconstruction <- function(result, parameters) {
  expect_identical(result$outcome, "success")
  expect_true(.valid_reconstruction(result$values, parameters))
  expect_true(.rounding_compatible(sd(result$values), parameters$sd, parameters$sd_prec))
  expect_identical(result$mean, mean(result$values))
  expect_identical(result$sd, sd(result$values))
}

test_that("restriction keys identify one lattice value and counts are validated", {
  p <- set_parameters(2, 1, 10, 1, 3, restrictions_exact = list(" 2.00 " = 0))
  expect_identical(p$fixed_values, 2)
  expect_reconstruction(find_possible_distribution(p, seed = 1), p)
  q <- set_parameters(2, 1, 10, 1, 3, restrictions_minimum = list(" 2.00 " = 2))
  expect_equal(q$fixed_responses, c(2, 2))
  expect_reconstruction(find_possible_distribution(q, seed = 1), q)
  fine <- set_parameters(1.5, .2, 10, 1, 2, n_items = 200,
                         restrictions_exact = list("1.01" = 1), dont_test = TRUE)
  expect_length(fine$fixed_values, 1)
  expect_equal(fine$fixed_values, 1.01)
  shorthand <- set_parameters(2, 1, 10, 1, 3, n_items = 3,
                              restrictions_exact = list("1.67" = 1))
  expect_equal(shorthand$fixed_values, 5/3)
  precise <- set_parameters(2, 1, 10, 1, 4, n_items = 3,
                            restrictions_exact = list("3.6667" = 1))
  expect_equal(precise$fixed_values, 11/3)
  three_places <- set_parameters(2, 1, 10, 1, 3, n_items = 3,
                                 restrictions_exact = list("1.667" = 1))
  expect_equal(three_places$fixed_values, 5/3)
  expect_error(set_parameters(1.5, .2, 10, 1, 2, n_items = 201,
                              restrictions_exact = list("1.01" = 1), dont_test = TRUE), "exactly one")
  for (bad in list(-1, 2.5, NA_real_, Inf, "two")) {
    expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list("1" = bad)))
    expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_minimum = list("1" = bad)))
  }
  expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list("1" = 11)), "exceed")
  expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list("2" = 0, "2.00" = 1)), "same value")
  expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list("2" = 0),
                              restrictions_minimum = list("2.0" = 1)), "same value")
  expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list("2.001" = 0)), "exactly one")
  expect_error(set_parameters(2, 1, 10, 1, 3, restrictions_exact = list(0)), "names")
})

test_that("singleton, fully fixed, and endpoint restrictions are safe", {
  for (seed in 1:5) {
    p <- set_parameters(3, 0, 10, 1, 3, restrictions_exact = list("1" = 0, "2" = 0))
    expect_reconstruction(find_possible_distribution(p, seed = seed), p)
    expect_equal(find_possible_distribution(p, seed = seed)$iterations, 0)
    p <- set_parameters(2, .7, 20, 1, 4, restrictions_exact = list("4" = 0))
    expect_reconstruction(find_possible_distribution(p, seed = seed), p)
    p <- set_parameters(3, .7, 20, 1, 4, restrictions_exact = list("1" = 0))
    expect_reconstruction(find_possible_distribution(p, seed = seed), p)
  }
  for (endpoint in c(1, 4)) {
    p <- set_parameters(if (endpoint == 1) 3 else 2, .8, 20, 1, 4,
                        m_prec = 1, sd_prec = 1,
                        restrictions_exact = stats::setNames(list(1), endpoint))
    expect_reconstruction(find_possible_distribution(p, seed = 1), p)
  }
  fixed <- set_parameters(2, 1, 3, 1, 3, restrictions_exact = list("1" = 1, "2" = 1, "3" = 1))
  expect_reconstruction(find_possible_distribution(fixed, seed = 1), fixed)
  expect_error(set_parameters(2, 1, 3, 1, 3, restrictions_exact = list("1" = 0, "2" = 0, "3" = 0)), "no allowed")
  impossible <- set_parameters(3, 1, 10, 1, 4, restrictions_exact = list("1" = 0, "2" = 0, "3" = 0))
  expect_error(find_possible_distribution(impossible, seed = 3), "initialize data")
  impossible <- set_parameters(4.5, 1, 10, 1, 5, restrictions_exact = list("1" = 8), dont_test = TRUE)
  expect_error(find_possible_distribution(impossible, seed = 1), "restrictions cannot be satisfied")
})

test_that("reconstruction uses inclusive rounding intervals", {
  for (sign in c(-1, 1)) {
    p <- set_parameters(sign * 1.3, .5, 4, if (sign == 1) 1 else -2,
                        if (sign == 1) 2 else -1, m_prec = 1, sd_prec = 1)
    expect_reconstruction(find_possible_distribution(p, seed = 1), p)
  }
  p <- set_parameters(1.72, 1.6, 40, -1, 4, n_items = 3, m_prec = 2, sd_prec = 2)
  res <- find_possible_distributions(p, 5, seed = 5)
  for (i in seq_len(nrow(res))) {
    expect_true(.rounding_compatible(mean(res$distribution[[i]]), p$mean, p$m_prec))
    expect_identical(res$mean[i], mean(res$distribution[[i]]))
    expect_identical(res$sd[i], sd(res$distribution[[i]]))
  }
  expect_equal(sort(.adjust_mean(1, c(1, 1), numeric(), 1.5, 1, c(1, 2))), c(1, 2))
})

test_that("the final adjustment is checked and failures retain their closest SD", {
  calls <- 0L
  local_mocked_bindings(
    rSprite.maxDeltaLoopsLower = 3L, rSprite.maxDeltaLoopsUpper = 3L,
    .adjust_mean = function(...) c(2, 2, 2),
    .shift_values = function(vec, ...) {
      calls <<- calls + 1L
      if (calls == 3L) c(1, 2, 3) else vec
    }
  )
  p <- set_parameters(2, 1, 3, 1, 3)
  result <- find_possible_distribution(p, seed = 1)
  expect_reconstruction(result, p)
  expect_identical(calls, 3L)
  expect_equal(result$iterations, 3)
})

test_that("failed metadata describes the best visited candidate", {
  local_mocked_bindings(
    rSprite.maxDeltaLoopsLower = 3L, rSprite.maxDeltaLoopsUpper = 3L,
    .adjust_mean = function(...) c(1, 2, 3),
    .shift_values = function(...) c(2, 2, 2)
  )
  p <- set_parameters(2, .8, 3, 1, 3, m_prec = 1, sd_prec = 1, dont_test = TRUE)
  result <- find_possible_distribution(p, seed = 1)
  expect_identical(result$outcome, "failure")
  expect_equal(result$values, c(1, 2, 3))
  expect_identical(result$sd, sd(result$values))
  expect_identical(result$mean, mean(result$values))
  expect_null(find_possible_distribution(p, values_only = TRUE, seed = 1))
})

test_that("an SD match alone cannot be labelled successful", {
  local_mocked_bindings(
    rSprite.maxDeltaLoopsLower = 0L, rSprite.maxDeltaLoopsUpper = 0L,
    .adjust_mean = function(...) c(1, 2, 3)
  )
  p <- set_parameters(2, 1, 3, 1, 3, restrictions_exact = list("2" = 0))
  expect_identical(find_possible_distribution(p, seed = 1)$outcome, "failure")
})

test_that("stopping counts repeated failures and failures after success", {
  calls <- 0L
  local_mocked_bindings(find_possible_distribution = function(...) {
    calls <<- calls + 1L
    list(outcome = if (calls == 1L) "success" else "failure",
         values = if (calls == 1L) c(1, 2, 3) else c(2, 2, 2), iterations = 0)
  })
  p <- set_parameters(2, 1, 3, 1, 3)
  expect_warning(suppressMessages(result <- find_possible_distributions(p, 5, return_failures = TRUE)), "last 10 attempts")
  expect_identical(calls, 11L)
  calls <- 0L
  expect_warning(suppressMessages(as_list <- find_possible_distributions(p, 5,
                           return_tibble = FALSE, return_failures = TRUE)), "last 10 attempts")
  expect_identical(as_list$distribution, result$distribution)
  expect_identical(as_list$outcome, result$outcome)
  expect_equal(nrow(result), 2)
  for (i in seq_len(nrow(result))) {
    expect_identical(result$sd[i], sd(result$distribution[[i]]))
    expect_identical(result$mean[i], mean(result$distribution[[i]]))
  }
})

test_that("new distributions reset consecutive duplicate counters", {
  calls <- 0L
  local_mocked_bindings(find_possible_distribution = function(...) {
    calls <<- calls + 1L
    list(outcome = "success", values = if (calls <= 100L) c(1, 2, 3) else c(2, 2, 2), iterations = 0)
  })
  p <- set_parameters(2, 1, 3, 1, 3)
  suppressMessages(find_possible_distributions(p, 20))
  expect_identical(calls, 202L)
})

test_that("the final success is included in the reported count", {
  local_mocked_bindings(rSprite.maxDupLoops = 1L, find_possible_distribution = function(...) {
    list(outcome = "success", values = c(1, 2, 3), iterations = 0)
  })
  expect_no_message(find_possible_distributions(set_parameters(2, 1, 3, 1, 3), 1))
})

test_that("explicit seeds preserve RNG state including missing state and errors", {
  p <- set_parameters(2, 1, 10, 1, 3)
  set.seed(27)
  prior <- .Random.seed
  a <- find_possible_distribution(p, seed = 1)
  expect_identical(.Random.seed, prior)
  expect_identical(find_possible_distribution(p, seed = 1), a)
  expect_identical(.Random.seed, prior)
  suppressMessages(find_possible_distributions(p, 1, seed = 1))
  expect_identical(.Random.seed, prior)
  impossible <- set_parameters(4.5, 1, 10, 1, 5, restrictions_exact = list("1" = 8), dont_test = TRUE)
  expect_error(find_possible_distribution(impossible, seed = 1))
  expect_identical(.Random.seed, prior)
  rm(".Random.seed", envir = .GlobalEnv)
  find_possible_distribution(p, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  assign(".Random.seed", prior, envir = .GlobalEnv)
})

test_that("parameter validation and zero requested distributions are explicit", {
  expect_no_warning(set_parameters(2.2, 1.3, 20, 1, 5))
  expect_error(set_parameters(8, 1, 20, 1, 7), "mean is outside")
  expect_error(set_parameters(2, -.1, 10, 1, 3, dont_test = TRUE))
  expect_error(set_parameters(2, 1, 1, 1, 3, dont_test = TRUE))
  expect_error(set_parameters(2, 1, 10, 1, 3, n_items = 0, dont_test = TRUE))
  expect_error(set_parameters(2, 1, 10, 1, 3, dont_test = NA))
  expect_no_error(set_parameters(5.19, 1, 28, 1, 7, dont_test = TRUE))
  expect_no_error(set_parameters(2, 100, 10, 1, 3, dont_test = TRUE))
  expect_error(set_parameters(2, 1, 10, 1, 3, m_prec = 309, dont_test = TRUE))
  expect_error(set_parameters(2.12, 1, 10, 1, 3, m_prec = 1, dont_test = TRUE))
  p <- set_parameters(2, 1, 10, 1, 3)
  expect_equal(nrow(find_possible_distributions(p, 0)), 0)
  expect_length(find_possible_distributions(p, 0, return_tibble = FALSE)$distribution, 0)
  expect_error(find_possible_distribution(p, values_only = NA))
  expect_error(find_possible_distributions(p, return_tibble = c(TRUE, FALSE)))
  expect_error(find_possible_distributions(p, return_failures = NA))
  expect_true(.equalish(1, 1.1, tol = .2))
  expect_false(.equalish(1, 1.1, tol = .01))
})

test_that("restricted shifts preserve lattice and mean in either direction", {
  for (items in c(1, 3)) for (lattice in list(c(1, 2, 4, 5), c(1, 2, 5, 6))) {
    allowed <- lattice / items
    vec <- rep(allowed, each = 5)
    fixed <- setdiff(seq_len(max(lattice)), lattice) / items
    target <- mean(vec)
    set.seed(413)
    changes_up <- changes_down <- 0L
    for (j in 1:300) {
      result <- .shift_values(vec, target, 1 / items, min(allowed), max(allowed),
                              5, 2, numeric(), allowed, fixed)
      expect_true(all(result %in% allowed))
      expect_equal(mean(result), target)
      changed <- which(result != vec)
      if (length(changed) > 2L) {
        if (sum(result < vec) > 1L) changes_up <- changes_up + 1L
        if (sum(result > vec) > 1L) changes_down <- changes_down + 1L
      }
    }
    expect_gt(changes_up, 0L)
    expect_gt(changes_down, 0L)
  }
})


test_that("restrictions retain exact numeric targets at large scale offsets", {
  values <- 1e6 + c(1, rep(4, 9)) / 7
  p <- set_parameters(round(mean(values), 2), round(sd(values), 2), 10, 1000000, 1000001,
                      n_items = 7, m_prec = 2, sd_prec = 2, dont_test = TRUE,
                      restrictions_minimum = stats::setNames(list(1), sprintf("%.17g", values[1])))
  expect_identical(p$restriction_values$minimum, values[1])
  expect_true(.valid_reconstruction(values, p))
  expect_reconstruction(find_possible_distribution(p, seed = 1), p)
})

test_that("the documented restricted multi-item example yields a valid reconstruction", {
  p <- set_parameters(1.95, 1.55, 20, 1, 5, n_items = 3,
                      restrictions_exact = list("3" = 0, "3.67" = 2),
                      restrictions_minimum = "range")
  expect_reconstruction(find_possible_distribution(p, seed = 1), p)
})
