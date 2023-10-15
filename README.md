<!-- badges: start -->

[![R-CMD-check](https://github.com/JsLth/greta-gis-tool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JsLth/greta-gis-tool/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

<!-- badges: end -->



# GRETA Analytics <a href="https://zenodo.org/communities/greta/"><img align="right" height="150" src="man/figures/logo.png"></a>

GRETA Analytics (package name: `{gretan}`) is the GIS tool of the GRETA project (GReen Energy Transition Actions). The tool serves to present and visualize the research output of the project in an interactive fashion. It is specialized on spatial analysis and visualization, but hosts a variety of other information from the project. The package/app is built using the R [Shiny](https://github.com/rstudio/shiny) and [golemverse](https://golemverse.org/) frameworks and the UI framework [`{bs4Dash}`](https://rinterface.github.io/bs4Dash/)/[`AdminLTE3`](https://github.com/ColorlibHQ/AdminLTE).

The package is licensed under Apache 2.0.

## Getting started

### Deployment

The app is deployed on [shinyapps.io](shinyapps.io) and can be accessed using the following link: https://projectgreta.shinyapps.io/.

### Binaries

The app is deployed as an Electron app using [`{electricShine}`](https://github.com/chasemc/electricShine). Binaries are released in the [OPCE database](https://zenodo.org/communities/greta/) of the GRETA project.

### Running in R

The app is built under the `{golem}` framework and installs like an R package:

```r
devtools::install_github("JsLth/gretan")
library(gretan)
```

The Shiny app can then be run using the following command:

```r
run_greta()
```

## Acknowledgements

GRETA Analytics was mainly developed by [Jonas Lieth](mailto:jonas.lieth@gesis.org) from [GESIS - Leibniz Institute for the Social Sciences](https://www.gesis.org/en/). The package and Shiny app was made possible thanks to the valuable contributions from various institutions including:

- [Lappeenranta-Lahti University of Technology](https://www.lut.fi/en)
  - [Persona quiz based on energy citizen clusters](https://projectgreta.eu/wp-content/uploads/2023/09/GRETA_D2.4_Energy-citizen-empowerment-through-energy-data-interactions.pdf)
- [Dutch Organization for Applied Scientific Research (TNO)](https://www.tno.nl/en/)
  - [pLAtYpus model for stakeholder interaction modelling](https://github.com/TNO/pLAtYpus)
  - [Case study "Natural gas-free neighborhoods" wrap-up](https://projectgreta.eu/wp-content/uploads/2023/01/GRETA_D3_2_Case-study-2-report_v1_0.pdf)
- [Tecnalia Research & Innovation](https://www.tecnalia.com/en/home)
  - [Energy assessment using the Enerkad tool](https://www.enerkad.net/)
- [University of Bologna](https://www.unibo.it/)
  - [Case study "Pilastro-Roveri" wrap-up](https://projectgreta.eu/wp-content/uploads/2023/01/GRETA_D3_1_Case-study-1-report_v1_0.pdf)
  - Fragility index
- [Fraunhofer Institute for Systems and Innovation Research (ISI)](https://www.isi.fraunhofer.de/)
  - [Case study "Earnest app" wrap-up](https://projectgreta.eu/wp-content/uploads/2023/01/GRETA_D3_5_Case-study-5-report_v1_0.pdf)
- [Cleanwatts Digital](https://cleanwatts.energy/)
  - [Case study "Coopérnico" wrap-up](https://projectgreta.eu/wp-content/uploads/2023/01/GRETA_D3_3_Case-study-3-report_v1_0.pdf)
- [Kaskas Media](https://kaskas.fi/en/)
  - Minor editing and design template

<img width="100rem" align="left" src="inst/app/www/eu_flag.jpg">

GRETA Analystics was developed as part of the [GRETA project](https://projectgreta.eu/), which has received funding from the European Union's HORIZON 2020 Research and Innovation programme under grant agreement N°101022317.
