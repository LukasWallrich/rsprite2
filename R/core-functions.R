#' Define parameters for SPRITE algorithm
#'
#' The SPRITE algorithm aims to construct possible distributions that conform to
#' observed/reported parameters. This function performs some checks and returns a list of these
#' parameters that can then be passed to the functions that actually generate
#' the distributions (e.g. \code{\link{find_possible_distribution}})
#'
#' Restrictions can be used to define how often a specific value should appear in the sample.
#' They need to be passed as a list in the form `value = frequency`. Thus, to specify that
#' there should be no 3s and five 4s in the distribution, you would pass `restrictions = list(3 = 0, 4 = 5)`.
#'
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param n_obs The number of observations (sample size)
#' @param min_val The minimum value
#' @param max_val The maximum value
#' @param m_prec The precision of the mean, as number of digits after the decimal point.
#' If not provided, taken based on the significant digits of `mean` - so only needed if reported mean ends in 0
#' @param sd_prec The precision of the standard deviation, again only needed if
#' reported standard deviation ends in 0.
#' @param n_items Number of items in scale, if distribution represents scale averages.
#' Defaults to 1, which represents any single-item measure.
#' @param restrictions Restrictions on the frequency of specific responses, see Details)
#'
#' @return A named list of parameters.
#' @export

set_parameters <- function(mean, sd, n_obs, min_val, max_val,
                           m_prec = NULL, sd_prec = NULL,
                           n_items = 1, restrictions = NULL) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
  }

  assert_count(m_prec)
  assert_count(sd_prec)
  assert_count(n_obs)
  assert_count(n_items)
  assert_int(min_val)
  assert_int(max_val)
  assert_number(mean)
  assert_number(sd)

  if (min_val >= max_val) {
    stop("max_val needs to be larger than min_val")
  }

  if (!GRIM_test(mean, n_obs, m_prec, n_items)) {
    stop("The mean is not consistent with this number of observations (fails GRIM test).
         You can use GRIM_test() to identify the closest possible mean and try again.")
  }

  sd_limits <- .sd_limits(n_obs, mean, min_val, max_val, sd_prec, n_items = 1)

  if (!(sd >= sd_limits[1] & sd <= sd_limits[2])) {
    stop("The standard deviation is outside the possible range, given the other parameters.
         It should be between ", sd_limits[1], " and ", sd_limits[2], ".")
  }

    if (!(mean >= min_val & mean <= max_val)) {
    stop("The mean is outside the possible range, which is impossible - please check inputs.")
  }

  # TK: validate restrictions & find a way to enable restrictions for fractions (refactor poss values here!)

  out <- .named_list(mean, sd, n_obs, min_val, max_val, m_prec, sd_prec, n_items, restrictions)

  class(out) <- c("sprite_parameters", class(out))

  out
}

.named_list <- function(...) {
  out <- list(...)
  names(out) <- as.list(match.call())[-1]
  out
}

#' Find several possible distributions.
#'
#' This function aims to find several possible distribution that would give rise to
#' the observed sample parameters. For that, you need to pass a list of parameters,
#' created with \code{\link{set_parameters}}
#'
#' @param parameters List of parameters, see \code{\link{set_parameters}}
#' @param n_distributions The target number of distributions to return.
#' @param seed An integer to use as the seed for random number generation. Set this in scripts to ensure reproducibility.
#' @param return_tibble Should a tibble, rather than a list, be returned? Requires the `tibble`-package, ignored if that package is not available.
#' @param return_failures Should distributions that failed to produce the desired SD be returned? Defaults to false
#'
#' @return A tibble or list (depending on the `return_tibble` argument) with:
#' \item{outcome}{success or failure - character}
#' \item{distribution}{The distribution that was found (if success) / that had the closest variance (if failure) - numeric}
#' \item{sd}{The SD of the distribution that was found (success) / that came closest (failure) - numeric}
#' \item{iterations}{The number of iterations required to achieve the specified SD - numeric - the first time this distribution was found}
#'
#' @export
#'


find_possible_distributions <- function(parameters, n_distributions, seed = NULL, return_tibble = TRUE, return_failures = FALSE) {

  if (!is.null(seed)) {
    assert_int(seed)
    set.seed(seed)
  }

  assert_count(n_distributions)
  assert_logical(return_tibble)

  outcome <- character()
  distributions <- list()
  found_sd <- numeric()
  iterations <- numeric()



    duplications <- 0

    for (i in 1:(n_distributions * rSprite.maxDupLoops)) {

      n_found <- sum(outcome == "success")

      #This break should possibly be earlier?
      if(length(outcome) - max(c(which(outcome == "success"),0)) >= 10) {
        warning("No successful distribution found in last 10 attempts. Exiting.", if (n_found == 0) " There might not be any possible distribution, but you can try running the search again.")
        break
      }
      if (n_found >= n_distributions) break

      # Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
      # The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
      #  however, it's extremely likely that all possible solutions are not all equally likely to be found.
      # So we also set a floor of 100 attempts.
      max_duplications <- max(round(log(0.00001) / log(n_found / (n_found + 1))), 100)

      res <- find_possible_distribution(parameters, seed = NULL)

      res$values <- sort(res$values) # sorting lets us find duplicates more easily

      distributions <- c(list(res$values), distributions)
      if(head(duplicated(distributions, fromLast = TRUE), 1)) {
        distributions <- distributions[-1]
        duplications <- duplications + 1
        if (duplications > max_duplications) {
          break
        }
      } else {
        outcome <- c(res$outcome, outcome)
        found_sd <- c(res$sd, found_sd)
        iterations <- c(res$iterations, iterations)
      }

    }

    if (n_found < n_distributions) message("Only ", n_found, " matching distributions could be found. You can try again - given that SPRITE is based on random number generation, more distributions might be found then.")

    if (return_tibble & suppressWarnings(requireNamespace("tibble", quietly = TRUE))) {
      out <- tibble::tibble(id = seq_along(outcome), outcome = outcome, distribution = distributions, current_sd = found_sd, iterations = iterations)
      if(!return_failures) return(out[out$outcome == "success",])
      out
    } else {
      if(!return_failures) {
        successes <- outcome == "success"
      return(list(outcome = outcome[successes], distribution = distributions[successes], current_sd = found_sd[successes], iterations = iterations[successes]))
      }
      list(outcome = outcome, distribution = distributions, current_sd = found_sd, iterations = iterations)
    }

}

#' Find a possible distribution.
#'
#' This function aims to find a possible distribution that would give rise to
#' the observed sample parameters. For that, you need to pass a list of parameters,
#' best created with \code{\link{set_parameters}}
#'
#' @param parameters List of parameters, see \code{\link{set_parameters}}
#' @param seed An integer to use as the seed for random number generation. Set this in scripts to ensure reproducibility.
#' @param values_only Should only values or a more informative list be returned. See details.
#'
#' @return Unless `values_only = TRUE`, a list with:
#' \item{outcome}{success or failure - character}
#' \item{distribution}{The distribution that was found (if success) / that had the closest variance (if failure) - numeric}
#' \item{sd}{The SD of the distribution that was found (success) / that came closest (failure) - numeric}
#' \item{iterations}{The number of iterations required to achieve the specified SD - numeric}
#' If `values_only = TRUE`, then the distribution is returned if one was found, and NULL if it failed.
#'
#' @export
#'


find_possible_distribution <- function(parameters, seed = NULL, values_only = FALSE) {

  assert_class(parameters, "sprite_parameters")

  if (!is.null(seed)) {
    assert_int(seed)
    set.seed(seed)
  }

  if (is.null(parameters$restrictions)) {
    restricted <- NA
    fixed_vals <- numeric()
    fixed <- 0
  } else {
    restricted <- as.numeric(names(parameters$restrictions))
    fixed_vals <- rep(restricted, unlist(parameters$restrictions))
    fixed <- length(fixed_vals)
  }
  # Possible values - TK: check whether rSPRITE approach for n_items = 1 is much faster
  # TK: Also, should include something to reflect that some (e.g., consistent) responses are more likely?
  poss_values <- numeric()
  for (i in seq_len(parameters$n_items)) {
    poss_values <- c(poss_values, parameters$min_val:parameters$max_val + (1 / parameters$n_items) * (i - 1))
  }

  poss_values <- sort(poss_values[poss_values <= parameters$max_val])

  poss_non_restricted <- setdiff(poss_values, restricted)

  # Generate some random starting data.
  rN <- parameters$n_obs - fixed
  vec <- sample(poss_non_restricted, rN, replace = TRUE)

  # Adjust mean of starting data.
  max_loops <- parameters$n_obs * length(poss_values)
  vec <- .adjust_mean(max_loops, vec, fixed_vals, parameters$mean, parameters$m_prec, poss_values, restricted)


  # Find distribution that also matches SD
  maxLoops <- min(max(round(parameters$n_obs * (length(poss_non_restricted)^2)), rSprite.maxDeltaLoopsLower), rSprite.maxDeltaLoopsUpper)
  granule_sd <- ((0.1^parameters$sd_prec) / 2) + rSprite.dust # allow for rounding errors

  result <- NULL

  for (i in seq_len(maxLoops)) {

    #Should one break out of loop when vec no longer changes? Prob not worth all the comparisons?
    current_sd <- sd(c(vec, fixed_vals))
    if (abs(current_sd - parameters$sd) <= granule_sd) {
      result <- c(vec, fixed_vals)
      iter <- i
      break
    }
    vec <- .shift_values(vec, parameters$mean, parameters$sd, parameters$min_val, parameters$max_val, parameters$m_prec, parameters$sd_prec, fixed_vals, poss_values, restricted)
  }

  if (!is.null(result)) {
    if(values_only) return(result)
    return(list(outcome = "success", values = result, sd = current_sd, iterations = iter))
  } else {
    if(values_only) return(NULL)
    return(list(outcome = "failure", values = c(vec, fixed_vals), sd = current_sd, iterations = maxLoops))
  }
}

.adjust_mean <- function(max_iter, vec, fixed_vals, target_mean, m_prec, poss_values, restricted) {

  # Drop restricted values, then simply move up or down the remaining possibilities
  poss_non_restricted <- setdiff(poss_values, restricted)
  meanOK <- FALSE

  for (i in 1:max_iter) {
    fullVec <- c(vec, fixed_vals)
    current_mean <- mean(fullVec)
    if ((round(current_mean, m_prec) == target_mean)) {
      meanOK <- TRUE
      break
    }

    increaseMean <- (current_mean < target_mean)
    if (increaseMean) {
      filter <- (vec < (poss_non_restricted[length(poss_non_restricted)]))
    } else {
      filter <- (vec > (poss_non_restricted[1]))
    }

    possible_bump <- which(filter)
    bumpMean <- possible_bump[as.integer(runif(1) * length(possible_bump)) + 1] # select a  number
    vec[bumpMean] <- poss_non_restricted[which(poss_non_restricted == vec[bumpMean]) + ifelse(increaseMean, 1, -1)]
  }
  if (!meanOK) {
    if (!is.na(restricted)) {
      stop("Couldn't initialize data with correct mean. This *might* be because the restrictions cannot be satisfied.")
    } else {
      stop("Couldn't initialize data with correct mean") # this probably indicates a coding error, if the mean is in range
    }
  }
  return(vec)
}

.shift_values <- function(vec, target_mean, target_sd, min_val, max_val, m_prec = 2, sd_prec, fixed_vals, poss_values, restricted) {

  # Backup
  vec_original <- vec

  # Decide if we want to increment or decrement first.
  incFirst <- sample(c(TRUE, FALSE), 1)

  # Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed_vals)
  increaseSD <- (sd(fullVec) < target_sd)

  # Only deal with fixed values when need arises
  poss_non_restricted <- setdiff(poss_values, restricted)

  maxToInc <- poss_non_restricted[length(poss_non_restricted) - 1] # maximum value that we can increment
  minToDec <- poss_non_restricted[2] # minimum value that we can decrement

  # Select an element to increment or decrement.
  # For better performance, we select from unique elements only; this means that any number that appears in the vector is
  #  equally likely to be chosen regardless of how often it appears. I'm not sure if this is good or bad.
  # TK - check performance impact.
  uniqueCanBump1 <- !duplicated(vec)

  # The element that we change should be less than the maximum (increment) or greater than the minimum (decrement).
  notEdge1 <- if (incFirst) (vec <= maxToInc) else (vec >= minToDec)
  indexCanBump1 <- uniqueCanBump1 & notEdge1

  # If we can't find an element to change, just return the original vector and let our caller sort it out.
  if (sum(indexCanBump1) == 0) {
    return(vec_original)
  }

  # Unless we have no other choice:
  # - If we want to make the SD larger, there is no point in incrementing the smallest element, or decrementing the largest
  # - If we want to make the SD smaller, there is no point in decrementing the smallest element, or incrementing the largest
  if (increaseSD) {
    noPoint1 <- if (incFirst) (vec == min(vec)) else (vec == max(vec))
  } else {
    noPoint1 <- if (incFirst) (vec == maxToInc) else (vec == minToDec)
  }
  indexCanBump1Try <- indexCanBump1 & (!noPoint1)
  if (any(indexCanBump1Try)) {
    indexCanBump1 <- indexCanBump1Try
  }

  whichCanBump1 <- which(indexCanBump1)
  whichWillBump1 <- whichCanBump1[as.integer(runif(1) * length(whichCanBump1)) + 1]
  willBump1 <- vec[whichWillBump1]
  new1 <- poss_non_restricted[which(poss_non_restricted == willBump1) + ifelse(incFirst, 1, -1)]
  gap1 <- new1 - vec[whichWillBump1] # Note when restricted values have been skipped
  vec[whichWillBump1] <- new1

  # At this point we can decide to only change one of the elements (decrement one without incrementing another, or vice versa).
  # This enables us to explore different means that still round to the same target value.
  # So here we perform the first increment or decrement first, and see if the mean is still GRIM-consistent with the target mean.
  # If it is, then in a proportion of cases we don't adjust the other cell.
  newFullVec <- c(vec, fixed_vals)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, m_prec) != target_mean) # new mean is no longer GRIM-consistent

  if (meanChanged || (runif(1) < 0.4)) {
    vecBump2 <- vec # make a scratch copy of the input vector so we can change it
    vecBump2[whichWillBump1] <- NA # remove the element chosen in part 1...
    uniqueCanBump2 <- !duplicated(vecBump2) # ... but if there was more than one copy of that, it's still a candidate
    notEdge2 <- if (!incFirst) (vecBump2 <= maxToInc) else (vecBump2 >= minToDec)
    indexCanBump2 <- uniqueCanBump2 & notEdge2 & (!is.na(vecBump2))

    # If we can't find an element to change in the opposite direction to the first, then if the mean with the first change is still OK,
    #  we return either the vector with that change. Otherwise we return the original vector and let our caller sort it out.
    if (sum(indexCanBump2) == 0) {
      return(if (meanChanged) vec_original else vec)
    }

    # Unless we have no other choice:
    # - If we want to make the SD larger:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a larger one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a smaller one
    # - If we want to make the SD smaller:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a smaller one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a larger one
    # There is also no point in incrementing an element that is equal to the new value of the one that we have already chosen.
    noPoint2 <- ((if (increaseSD == incFirst) (vec > willBump1) else (vec < willBump1)) |
      (vec == (new1))
    )
    indexCanBump2Try <- indexCanBump2 & (!noPoint2)
    if (any(indexCanBump2Try)) {
      indexCanBump2 <- indexCanBump2Try
    }

    whichCanBump2 <- which(indexCanBump2)
    whichWillBump2 <- whichCanBump2[as.integer(runif(1) * length(whichCanBump2)) + 1]
    willBump2 <- vec[whichWillBump2]
    new2 <- poss_non_restricted[which(poss_non_restricted == willBump2) + ifelse(incFirst, -1, 1)]
    gap2 <- new2 - vec[whichWillBump2] # Note when restricted values have been skipped

    gap_resolved <- NA
    # Go into restricted values handling only when necessary - should be good for performance, but
    # leads to more complex backtracking here.
    if (!.equalish(abs(gap1), abs(gap2))) {
      gap_resolved <- FALSE
      poss <- which(.equalish(diff(poss_non_restricted), abs(gap1)))
      if (length(poss) > 1) {
        low <- poss_non_restricted[poss]
        up <- poss_non_restricted[poss + 1]
        if (incFirst) {
          # Should not now move down from target - otherwise we are simply reversing course
          target <- which(up == new1)
        } else {
          target <- which(low == new1)
        }
        up <- up[-target]
        low <- low[-target]

        if (incFirst) {
          from <- up[up %in% vec]
          to <- low[up %in% vec]
        } else {
          to <- up[low %in% vec]
          from <- low[low %in% vec]
        }
        if (length(from) > 0) {
          replace <- sample(1:length(from), 1)
          vec[cumsum(cumsum(vec == from[replace])) == 1] <- to[replace]
          gap_resolved <- TRUE
        }
      }

      if (!gap_resolved) {
        # Cannot do a single second replacement without undoing change so far
        restricted_runs <- rle(poss_values %in% poss_non_restricted)
        longest_run <- max(restricted_runs$lengths[restricted_runs$values == FALSE])
        vec_backup <- vec
        for (i in seq_len(longest_run)) {
          vec <- vec_backup
          replaced <- 0
          i <- i + 1 # Gap of 1 suggest 2 steps might be needed
          poss <- which(.equalish(diff(poss_non_restricted), gap1 / i))
          if (length(poss) > 0) {
            low <- poss_non_restricted[poss]
            up <- poss_non_restricted[poss + 1]

            for (j in seq_len(i)) {
              if (incFirst) {
                from <- up[up %in% vec]
                to <- low[up %in% vec]
              } else {
                to <- up[low %in% vec]
                from <- low[low %in% vec]
              }
              if (length(from) > 0) {
                replaced <- replaced + 1
                replace <- sample(1:length(from), 1)
                vec[cumsum(cumsum(vec == from[replace])) == 1] <- to[replace]
              } else {
                break
              }
            }
            if (replaced == i) {
              gap_resolved <- TRUE
              break
            }
          }
        }


        if (!gap_resolved) {
          # No way to get this done with multiple replacements - so, exit
          return(if (meanChanged) vec_original else vec)
        }
      }
    } else {
      vec[whichWillBump2] <- new2
    }
  }

  #added <- .get_diffs(vec, vec_original)
  #removed <- .get_diffs(vec_original, vec)

  newFullVec <- c(vec, fixed_vals)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, m_prec) != target_mean) # new mean is no longer GRIM-consistent

  # Floating point issues lead to mean drift with multi-item scales - curtail this straight-away
  if (meanChanged) return(vec_original)

  #if(!.equalish(sum(added), sum(removed)) & meanChanged) browser()

    #message(gap_resolved, "Mean diff detected when adding ", added, " while removing ", removed)

  return(vec)

}

.get_diffs <- function(x, y) {
  x_tbl <- table(x)
  y_tbl <- table(y)
  y_tbl_ord <- y_tbl[names(x_tbl)]
  y_tbl_ord[is.na(y_tbl_ord)] <- 0
  x_tbl <- x_tbl - y_tbl_ord
  as.numeric(rep(names(x_tbl), pmax(x_tbl,0)))
}

.equalish <- function(x, y, tol = rSprite.dust) {
  x <= (y + rSprite.dust) &
    x >= (y - rSprite.dust)
}

#' GRIM test for mean
#'
#' This function tests whether a given mean (with a specific precision) can
#' result from a sample of a given size based on integer responses to one or more
#' items. The test is based on Brown & Heathers (2017).
#' If `return_values = TRUE` and if there is more than one precise mean compatible
#' with the given parameters, all possible means are returned. In that case, if the
#' given mean is not consistent, the closest consistent mean is returned with a
#' warning.
#'
#' @param return_values Should all means consistent with the given parameters be returned?
#' @inheritParams set_parameters
#'
#' @return Either TRUE/FALSE, or all possible means (if test passes)/closest consistent mean (if test fails)
#'
#' @references
#' \insertRef{brown2017grim}{rsprite2}


GRIM_test <- function(mean, n_obs, m_prec = NULL, n_items = 1, return_values = FALSE) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  assert_count(m_prec)
  assert_count(n_obs)
  assert_count(n_items)
  assert_logical(return_values)
  assert_number(mean)

  int <- round(mean * n_obs * n_items) # nearest integer
  frac <- int / (n_obs * n_items) # mean resulting from nearest integer
  dif <- abs(mean - frac)
  granule <- ((0.1^m_prec) / 2) + rSprite.dust # allow for rounding errors
  if (dif > granule) {
    if (!return_values) {
      return(FALSE)
    }
    valid_mean <- round(frac, m_prec)
    prec_format <- paste("%.", m_prec, "f", sep = "")
    warning("Mean ", sprintf(prec_format, mean), " fails GRIM test - closest consistent value: ", sprintf(prec_format, valid_mean))
    return(valid_mean)
  } else {
    if (!return_values) {
      return(TRUE)
    }
    possible_means <- frac
    i <- 1
    original_int <- int
    while (TRUE) {
      int <- int + i
      frac <- int / (n_obs * n_items) # mean resulting from nearest integer
      dif <- abs(mean - frac)
      if (dif > granule) break()
      possible_means <- c(possible_means, frac)
      i <- i + 1
    }
    i <- 1
    int <- original_int
    while (TRUE) {
      int <- int - i
      frac <- int / (n_obs * n_items) # mean resulting from nearest integer
      dif <- abs(mean - frac)
      if (dif > granule) break()
      possible_means <- c(possible_means, frac)
      i <- i + 1
    }
    return(possible_means)
  }

  stop("Branching error - should not get here")
}

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
.sd_limits <- function(n_obs, mean, min_val, max_val, sd_prec, n_items = 1) {

  result <- c(-Inf, Inf)

  aMax <- min_val                                # "aMax" means "value of a to produce the max SD"
  aMin <- floor(mean*n_items)/n_items
  bMax <- max(max_val, min_val + 1, aMin + 1)   # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1/n_items
  total <- round(mean * n_obs * n_items)/n_items

  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]

    k <- round((total - (n_obs * b)) / (a - b))
    k <- min(max(k, 1), n_obs - 1)               # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, n_obs - k))
    diff <- sum(vec) - total
    if ((diff < 0) && (k > 1)) {
      vec <- c(rep(a, k - 1), abs(diff), rep(b, n_obs - k))
    }
    else if ((diff > 0) && ((n_obs - k) > 1)) {
      vec <- c(rep(a, k), diff, rep(b, n_obs - k - 1))
    }
    result[m] <- round(sd(vec), sd_prec)
  }

  return(result)
}


