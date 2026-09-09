plot_fixture <- function(values, n_items = NULL, min_val = 1, max_val = 5) {
  d <- tibble::tibble(id = seq_along(values), distribution = values)
  if (!is.null(n_items)) {
    class(d) <- c("sprite_distributions", class(d))
    attr(d, "parameters") <- list(n_items = n_items, min_val = min_val, max_val = max_val)
  }
  d
}

require_plot_packages <- function() {
  for (pkg in c("ggplot2", "tidyr", "tibble", "rlang", "scales")) {
    skip_if_not_installed(pkg)
  }
}

draw_distribution_plot <- function(p) {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off())
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

test_that("histogram bars preserve exact frequencies and complete endpoints", {
  require_plot_packages()
  cases <- list(
    plot_fixture(list(c(rep(1, 3), rep(2, 3), rep(3, 4))), 1, 1, 3),
    plot_fixture(list(c(1, 1, 2, 4, 5, 5)), 1),
    plot_fixture(list(c(1, 4/3, 5/3, 2, 2, 10/3, 4, 5)), 3),
    plot_fixture(list(rep(3, 10)), 1),
    plot_fixture(list(rep(3, 10))),
    plot_fixture(list(c(1, 2, 4), c(2, 4, 4, 5)), 1)
  )
  for (d in cases) {
    p <- plot_distributions(d, "histogram")
    built <- ggplot2::ggplot_build(p)
    bars <- built$data[[1]]
    expect_true(all(is.finite(bars$xmin)))
    expect_true(all(is.finite(bars$xmax)))
    expect_equal(sum(bars$count), sum(lengths(d$distribution)))
    for (i in seq_along(d$distribution)) {
      actual <- bars[as.integer(bars$PANEL) == i, ]
      responses <- sort(unique(d$distribution[[i]]))
      expect_equal(actual$x, responses)
      expect_equal(actual$count, vapply(responses, function(x) sum(d$distribution[[i]] == x), integer(1)))
    }
    if (inherits(d, "sprite_distributions")) {
      expect_equal(bars$xmax - bars$xmin,
                   rep(0.9 / attr(d, "parameters")$n_items, nrow(bars)))
    }
    expect_no_warning(draw_distribution_plot(p))
  }
})

test_that("empty and malformed plot inputs have deliberate diagnostics", {
  require_plot_packages()
  expect_error(plot_distributions(plot_fixture(list())), "No distributions to plot")
  for (x in list(numeric(), c(1, NA), c(1, Inf), "1", factor(1), matrix(1:4, 2), list(1))) {
    expect_error(plot_distributions(plot_fixture(list(x))), "nonempty, finite numeric vectors")
  }
  expect_error(plot_distributions(tibble::tibble(id = 1, distribution = 1)), "list-column")
  for (x in list(NA, 0, -1, Inf, 1.5, c(1, 2), "2")) {
    expect_error(plot_distributions(plot_fixture(list(1:3)), max_plots = x), "max_plots")
  }
  for (x in list(NA, 1, c(TRUE, FALSE), logical())) {
    expect_error(plot_distributions(plot_fixture(list(1:3)), show_ids = x), "show_ids")
    expect_error(plot_distributions(plot_fixture(list(1:3)), facets = x), "facets")
  }
  expect_error(plot_distributions(plot_fixture(list(1:3)), plot_type = c("ecdf", "density")), "plot_type")
})

test_that("constant ECDFs render and undefined densities are rejected", {
  require_plot_packages()
  d <- plot_fixture(list(rep(3, 10)))
  expect_no_warning(draw_distribution_plot(plot_distributions(d, "ecdf")))
  expect_error(plot_distributions(d, "density"), "at least two distinct")
  expect_error(plot_distributions(plot_fixture(list(3)), "density"), "at least two distinct")
  expect_no_warning(draw_distribution_plot(plot_distributions(plot_fixture(list(1:3)), "density")))
})

test_that("subsampling preserves the response lattice and frequency totals", {
  require_plot_packages()
  d <- plot_fixture(rep(list(c(1, 5/3, 5/3, 5)), 4), 3)
  expect_message(p <- plot_distributions(d, max_plots = 2), "randomly selected")
  bars <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(length(unique(bars$PANEL)), 2L)
  expect_equal(sum(bars$count), 8)
  expect_equal(bars$xmax - bars$xmin, rep(0.3, nrow(bars)))
  expect_no_warning(draw_distribution_plot(p))
})
