# rsprite2 0.2.1.9002

* Changed implementation of `GRIM_test()` to more direct calculation rather than iterative check to increase transparency and efficiency. If requested to return the nearest possible means, the function now also correctly returns two values if there are two equidistant values. (For users wishing to use the original algorithm, it is presently retained as an *unexported* function that can be called with `rsprite2:::GRIM_test_old()`.)
* Changed implementation of `GRIMMER_test()` to account for the fact that multiple exact means may be compatible with the reported means, if the mean is reported to a lower precision than the standard deviation. The previous version missed some valid SDs in this rare case. Also optimised the implementation of the algorithm to catch special cases explicitly and run a lot faster.
* Added a `boundary_test()` function that tests whether a standard deviation is within the possible range. This tests was already part of GRIMMER, but is now exported and separately documented for transparency. The underlying function to calculate SD limits now also accounts for various rounding choices for values ending in .5, i.e. up, down or to even, to reduce the risk of false positives, and is vectorised to increase speed.

# rsprite2 0.2.1.9000

* Bug fixed in the boundary test, ensuring that SD limits are calculated correctly even when the mean is on (or very near) the scale limits.
* Removed potential issue with floating point imprecision in the `GRIMMER_test()` function, which could lead to false positives in some cases.


# rsprite2 0.2.1

* `GRIMMER_test()` now supports multi-item scales, as Aurélien Allard kindly extended his algorithm

# rsprite2 0.2.0

* Added GRIMMER test (`GRIMMER_test()`) to catch impossible standard deviations, using the [algorithm developed by Aurélien Allard](https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/)

# rsprite2 0.1.0

* Ported SPRITE algorithm from [rSPRITE](https://github.com/sTeamTraen/rSPRITE) 0.17 by Nick Brown
* Added support for multi-scale responses, inspired by [pySPRITE](https://github.com/QuentinAndre/pysprite)
* Added support for more complex restrictions, including minimum frequencies
* First submission to CRAN

# rsprite2 0.0.0 Known limitations / tasks for future development

* `set_parameters` does not consider the restrictions provided when running the GRIM test. Doing so would help to catch some impossible restrictions at this stage.
