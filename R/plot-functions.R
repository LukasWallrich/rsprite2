



#' Plot distributions
#'
#' This plots distributions identified by \code{\link{find_possible_distributions}} using ggplot2.
#' They can be shown as histograms or as \href{https://towardsdatascience.com/what-why-and-how-to-read-empirical-cdf-123e2b922480}{cumulative distributions (ECDF) plots}. The latter give
#' more information, yet not all audiences are familiar with them.
#'
#' @param distributions Tibble with a column `distribution` and an identifier (`id`), typically as returned from \code{\link{find_possible_distributions}}.
#' @param plot_type Plot multiple histograms, or overlapping cumulative distribution plots, or density plots? "auto" is to plot histograms if up to 9 distributions are passed, or if there are fewer than 10 discrete values, and empirical cumulative distribution plots otherwise
#' @param max_plots How many distributions should *at most* be plotted? If more are passed, this number is randomly selected.
#' @param show_ids Should ids of the distributions be shown with ecdf and density charts? Defaults to no, since the default ids are not meaningful.
#'
#' @return A ggplot2 object that can be styled with functions such as \code{\link[ggplot2]{labs}} or \code{\link[ggplot2]{theme_linedraw}}
#' @export

plot_distributions <- function(distributions, plot_type = c("auto", "histogram", "ecdf", "density"), max_plots = 100, show_ids = FALSE) {
  .check_req_packages(c("tidyr", "ggplot2"))

  assert_tibble(distributions)
  assert_subset(c("id", "distribution"), names(distributions))
  assert_choice(plot_type[1], c("auto", "histogram", "ecdf", "density"))
  plot_type <- plot_type[1]

  if (any(duplicated(distributions$id))) {
    warning("id column should not contain duplicates. Replaced by row number instead.")
    distributions$id <- seq_along(distributions$id)
  }

  if (nrow(distributions) > max_plots) {
    message("Number of distributions passed exceeds max_plots parameter. ", max_plots, " will be randomly selected for plotting.")
    distributions <- distributions[sample(seq_along(distributions$id), max_plots), ]
  }

  n_distributions <- nrow(distributions)
  distributions_long <- tidyr::unnest_longer(distributions, distribution)
  unique_vals <- length(unique(distributions_long$distribution))

  if("sprite_distributions" %in% class(distributions)) {
    params <- attr(distributions, "parameters")
    scale_min <- params$min_val
    scale_max <- params$max_val
  } else {
    scale_min <- min(distributions_long$distribution)
    scale_max <- max(distributions_long$distribution)
  }


  if (plot_type == "auto") {
    plot_type <- ifelse(n_distributions > 9 & unique_vals > 9, "ecdf", "histogram")
  }

  p <- ggplot2::ggplot(distributions_long, ggplot2::aes(x = distribution)) + ggplot2::theme_light() +
    ggplot2::scale_x_continuous(limits = c(scale_min, scale_max))


  if (plot_type == "histogram") {
    bins <- min(30, unique_vals)
    p <- p + ggplot2::geom_histogram(bins = bins) +
      ggplot2::facet_wrap(ggplot2::vars(id), nrow = ceiling(sqrt(n_distributions)))
  }

  if (plot_type == "density") {
    p <- p + ggplot2::geom_density(ggplot2::aes(color = factor(id)), alpha = 5 / (5 + log(n_distributions)), show.legend = show_ids) +
             ggplot2::labs(x = "Response", color = "id")
  }

  if (plot_type == "ecdf") {
    p <- p + ggplot2::stat_ecdf(ggplot2::aes(color = factor(id)), alpha = 5 / (5 + log(n_distributions)), show.legend = show_ids) +
             ggplot2::labs(x = "Response", color = "id", y = "Cumulative share") +
             ggplot2::scale_y_continuous(labels = scales::percent)
  }
  p
}

