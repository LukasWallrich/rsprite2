# rsprite2 0.1.0

* Ported SPRITE algorithm from [rSPRITE](https://github.com/sTeamTraen/rSPRITE) 0.17 by Nick Brown
* Added support for multi-scale responses, inspired by [pySPRITE](https://github.com/QuentinAndre/pysprite)
* Added support for more complex restrictions, including minimum frequencies
* First submission to CRAN

# Known limitations / tasks for future development

* `set_parameters` does not consider the restrictions provided when running the GRIM test. Doing so would help to catch some impossible restrictions at this stage.
