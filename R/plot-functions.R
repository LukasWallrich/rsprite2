



#' Plot distributions
#'
#' This plots distributions identified by \code{\link{find_possible_distributions}} using ggplot2.
#'
#' @param distributions Tibble with a column `distribution` and an identifier (`id`), typically as returned from \code{\link{find_possible_distributions}}
#' @param plot_type Plot multiple histograms, or overlapping density plots? "auto" is to plot histograms if up to 9 distributions are passed, or if there are fewer than 10 discrete values, and density plots otherwise
#' @param plot_type How many distributions should *at most* be plotted? If more are passed, this number is randomly selected.
#'
#' @return A ggplot2 object that can be styled with functions such as \code{\link[ggplot2]{labs}} or \code{\link[ggplot2]{theme_linedraw}}
#' @export

plot_distributions <- function(distributions, plot_type = c("auto", "histogram", "density"), max_plots = 100) {
  .check_req_packages(c("tidyr", "ggplot2"))

  assert_tibble(distributions)
  assert_subset(c("id", "distribution"), names(distributions))
  assert_choice(plot_type[1], c("auto", "histogram", "density"))
  plot_type <- plot_type[1]

  if (any(duplicated(distributions$id))) {
    warning("id column should not contain duplicates. Replaced by number instead.")
    distributions$id <- seq_along(distributions$id)
  }

  if (nrow(distributions) > max_plots) {
    message("Number of distributions passed exceeds max_plots parameter. ", max_plots, " will be randomly selected for plotting.")
    distributions <- distributions[sample(seq_along(distributions$id), max_plots), ]
  }

  n_distributions <- nrow(distributions)
  distributions <- tidyr::unnest_longer(distributions, distribution)
  unique_vals <- length(unique(distributions$distribution))

  if (plot_type == "auto") {
    plot_type <- ifelse(n_distributions > 9 & unique_vals > 9, "density", "histogram")
  }

  p <- ggplot2::ggplot(distributions, ggplot2::aes(x = distribution))

  if (plot_type == "histogram") {
    bins <- min(30, unique_vals)
    return(p + ggplot2::geom_histogram(bins = bins) +
      ggplot2::facet_wrap(ggplot2::vars(id), nrow = ceiling(sqrt(n_distributions))) +
      ggplot2::theme_light())
  }

  if (plot_type == "density") {
    return(p + ggplot2::geom_density(ggplot2::aes(color = factor(id)), alpha = 5 / (5 + log(n_distributions))))
  }
}
