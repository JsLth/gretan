# GRETA Analytics

<!-- badges: start -->

[![R-CMD-check](https://github.com/JsLth/greta-gis-tool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JsLth/greta-gis-tool/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

GRETA Analytics is the GIS tool of the GRETA project (GReen Energy Transition Actions). The tool serves to present and visualize the research output of the project in an interactive fashion. It is built using the R [Shiny](https://github.com/rstudio/shiny) framework and also runs Python code using [`reticulate`](https://rstudio.github.io/reticulate/).

## Getting started

### Installation

The app is deployed as an Electron app using [`electricShine`](https://github.com/chasemc/electricShine). Binaries are released in this repository and in the [OPCE database](https://zenodo.org/communities/greta/) of the GRETA project.

### Building from source

The app is built under the golem framework and installs like an R package:

```         
devtools::install_github("JsLth/greta-gis-tool")
```

## Contact

Jonas Lieth -- [jonas.lieth\@gesis.org](mailto:jonas.lieth@gesis.org){.email}

GRETA project -- <https://projectgreta.eu/>
