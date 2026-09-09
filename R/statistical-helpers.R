# Decimal precision of the displayed numeric value; trailing zeroes require an
# explicit precision because R numeric scalars do not retain them.
.infer_prec <- function(x) {
  assert_number(x, finite = TRUE)
  value <- format(abs(x), scientific = FALSE, digits = 15, trim = TRUE)
  if (!grepl(".", value, fixed = TRUE)) return(0L)
  nchar(sub("^[^.]*\\.", "", value))
}

.rounding_compatible <- function(actual, reported, precision, tol = rSprite.dust) {
  abs(actual - reported) <= 0.5 * 10^-precision +
    pmax(tol, 4 * .Machine$double.eps * pmax(abs(actual), abs(reported)))
}

.assert_reported <- function(x, precision, name) {
  assert_count(precision)
  if (precision > 308) {
    stop("`", name, "` precision must be at most 308 decimal places.", call. = FALSE)
  }
  if (abs(x - round(x, precision)) > max(rSprite.dust, abs(x) * .Machine$double.eps)) {
    stop("`", name, "` has more decimal places than its specified precision.", call. = FALSE)
  }
}

.assert_exact_integer <- function(x) {
  if (any(!is.finite(x) | abs(x) > 2^52)) {
    stop("The requested statistics exceed reliable integer arithmetic; reduce the scale or sample size.", call. = FALSE)
  }
}

.integer_sequence <- function(lower, upper, by = 1) {
  if (lower > upper) return(numeric(0))
  if ((upper - lower) / by > 1e6) {
    stop("More than one million candidates would need enumeration; increase the reported precision or request a logical GRIM result.", call. = FALSE)
  }
  seq(lower, upper, by = by)
}
