#' Plot distributions
#'
#' This plots distributions identified by \code{\link{find_possible_distributions}} using ggplot2.
#' They can be shown as histograms or as \href{https://towardsdatascience.com/what-why-and-how-to-read-empirical-cdf-123e2b922480}{cumulative distributions (ECDF) plots}. The latter give
#' more information, yet not all audiences are familiar with them.
#'
#' @param distributions Nonempty tibble with a list-column `distribution` containing nonempty, finite numeric vectors, and an identifier (`id`), typically as returned from \code{\link{find_possible_distributions}}. Density plots require at least two distinct responses in each distribution.
#' @param plot_type Plot multiple histograms, or overlapping cumulative distribution plots, or density plots? "auto" is to plot histograms if up to 9 distributions are passed, or if there are fewer than 10 discrete values, and empirical cumulative distribution plots otherwise
#' @param max_plots Positive integer: how many distributions should *at most* be plotted? If more are passed, this number is randomly selected.
#' @param show_ids Should ids of the distributions be shown with ecdf and density charts? Defaults to no, since the default ids are not meaningful.
#' @param facets Should distributions be shown in one chart or in multiple small charts? Only considered for ecdf and density charts, histograms are always shown in facets
#'
#' @details Histograms show the exact frequency at each observed response. Bar widths
#' use the response spacing (`1 / n_items`) for SPRITE results. For ordinary
#' tibbles, widths use the smallest observed spacing, or 1 if all responses are
#' identical. Scale endpoints and complete bars are retained. Empty inputs
#' produce an informative error.
#'
#' @return A ggplot2 object that can be styled with functions such as \code{\link[ggplot2]{labs}} or \code{\link[ggplot2]{theme_linedraw}}

#' @examples
#' sprite_parameters <- set_parameters(mean = 2.2, sd = 1.3, n_obs = 20,
#'                                     min_val = 1, max_val = 5)
#'
#' poss <- find_possible_distributions(sprite_parameters, 5, seed = 1234)
#'
#' # All distributions in same plot
#' plot_distributions(poss, plot_type = "ecdf")
#'
#' # Separate plot for each distribution
#' plot_distributions(poss, plot_type = "ecdf", facets = TRUE)
#'
#' @export

plot_distributions <- function(distributions, plot_type = c("auto", "histogram", "ecdf", "density"),
                               max_plots = 100, show_ids = FALSE, facets = NULL) {
  .check_req_packages(c("tidyr", "ggplot2", "rlang", "scales"))

  # To avoid depending on rlang, this cannot be imported
  .data <- rlang::.data

  distribution <- id <- NULL #To avoid "no visible binding" CMD check note


  assert_tibble(distributions)
  assert_subset(c("id", "distribution"), names(distributions))
  if (missing(plot_type)) plot_type <- "auto"
  assert_choice(plot_type, c("auto", "histogram", "ecdf", "density"))
  assert_int(max_plots, lower = 1)
  assert_flag(show_ids)
  if (!is.null(facets)) assert_flag(facets)
  if (nrow(distributions) == 0L) {
    stop("No distributions to plot: provide at least one distribution.", call. = FALSE)
  }
  if (!is.list(distributions$distribution) ||
      !all(vapply(distributions$distribution, function(x) {
        is.numeric(x) && is.null(dim(x)) && length(x) > 0L && all(is.finite(x))
      }, logical(1)))) {
    stop("distribution must be a list-column of nonempty, finite numeric vectors.", call. = FALSE)
  }
  if (plot_type == "density" &&
      any(vapply(distributions$distribution, function(x) length(unique(x)) < 2L, logical(1)))) {
    stop("Density plots require at least two distinct responses in each distribution.", call. = FALSE)
  }

  if (any(duplicated(distributions$id))) {
    warning("id column should not contain duplicates. Replaced by row number instead.")
    distributions$id <- seq_along(distributions$id)
  }
  distributions$id <- factor(distributions$id)

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

  if (plot_type == "histogram") {
    if (!is.null(facets) && !facets) {
    warning("Histograms will always be shown in separate facets, facets argument ignored.")
    }
    facets <- TRUE
  } else if (is.null(facets)) {
    facets <- FALSE
  }

  p <- ggplot2::ggplot(distributions_long, ggplot2::aes(x = .data$distribution)) +
    ggplot2::theme_light()

  if (plot_type == "histogram") {
    if (inherits(distributions, "sprite_distributions")) {
      spacing <- 1 / params$n_items
    } else {
      gaps <- diff(sort(unique(distributions_long$distribution)))
      spacing <- if (length(gaps)) min(gaps) else 1
    }
    # Train on bar extents as well as the declared scale, so endpoint bars
    # remain complete and responses are never merged into arbitrary bins.
    p <- p + ggplot2::geom_bar(width = 0.9 * spacing) +
      ggplot2::scale_x_continuous(limits = function(x) range(x, scale_min, scale_max)) +
      ggplot2::labs(x = "Response", y = "Count")
  } else {
    p <- p + ggplot2::scale_x_continuous(limits = c(scale_min, scale_max))
  }

  if (plot_type == "density") {
    p <- p + ggplot2::geom_density(ggplot2::aes(color = .data$id), alpha = 5 / (5 + log(n_distributions)), show.legend = show_ids) +
             ggplot2::labs(x = "Response", color = "id")
  }

  if (plot_type == "ecdf") {
    p <- p + ggplot2::stat_ecdf(ggplot2::aes(color = .data$id), alpha = 5 / (5 + log(n_distributions)), show.legend = show_ids) +
             ggplot2::labs(x = "Response", color = "id", y = "Cumulative share") +
             ggplot2::scale_y_continuous(labels = scales::percent)
  }
  if (facets) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(id), nrow = ceiling(sqrt(n_distributions)))
  }
  p
}

