
# rsprite2

<!-- badges: start -->
  [![R-CMD-check](https://github.com/LukasWallrich/rsprite2/workflows/R-CMD-check/badge.svg)](https://github.com/LukasWallrich/rsprite2/actions)
<!-- badges: end -->

This package creates possible distributions based on reported sample parameters, using the SPRITE algorithm. This can be used to check what the original sample might have looked like, and thus to understand the data generation process better. This can help with the identification of fabricated data or misreported parameters, but also with checking whether sample characteristics such as floor/ceiling effects might explain spurious findings.

For that, it implements the SPRITE algorithm proposed by [Heathers et al. (2018)](https://peerj.com/preprints/26968.pdf). Much of the code for is based on [Nick Brown's rSPRITE shiny app](https://github.com/sTeamTraen/rSPRITE), with some extensions. If you are just interested in interactive use, particularly to explore some data, that [Shiny online app](https://shiny.ieis.tue.nl/sprite/) might be better for you 

## Installation

You can install the present version of rsprite2 from GitHub with:

``` r
install.packages("remotes")
remotes::install_github('LukasWallrich/rsprite2')
```

## Example

...

``` r
library(rsprite2)
## basic example code
```

