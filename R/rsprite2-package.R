#' @import checkmate
#' @importFrom Rdpack reprompt
#' @importFrom stats runif sd
#' @importFrom utils head

## Global variables taken from rSPRITE
# Parameters that trade off speed versus completeness.
# maxDeltaLoopsLower controls how many times we tweak pairs of numbers before giving up hope of finding any solution;
#  it is the lower bound on a formula that includes the sample size and range.
# maxDeltaLoopsUpper is the absolute upper bound on that formula (a sanity check, in effect).
# maxDupLoops controls how many times we try to find another unique solution, when we know that at least one exists.
rSprite.maxDeltaLoopsLower <- 20000
rSprite.maxDeltaLoopsUpper <- 1000000
rSprite.maxDupLoops <- 20

rSprite.dust <- 1e-12 #To account for rounding errors
rSprite.huge <- 1e15 #Should this not be Inf?


#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
