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
