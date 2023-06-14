
<!-- README.md is generated from README.Rmd. -->

# eneRgyVD

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The aim of this `{shiny}` application (packaged under `{eneRgyVD}`) is
to provide communes and citizens of Canton de Vaud with energy
statistics, mostly at communal level, through an accessible and
documented plateform.

The app is built and maintained by the *data and indicators unit* at
[Direction de l’énergie du Canton de
Vaud](https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/diren-energie).

The data will be updated regularly when new data is available or when
the methodology has been improved.

New datasets will be updated when available.

Any broken or suboptimal feature can be directly reported at
<stat-energie@vd.ch>.

## Production version

The latest stable release of the application is deployed here (since
2023-06-19): <https://stat-energie-vd.ch/profil-energie/>

## Installation

You can install the development version of eneRgyVD like so:

``` r
devtools::install_github("mick-weber/eneRgyVD")
```

## Launching the app locally

After installing, you can call `library(eneRgyVD)` and then simply run
`run_app()` to launch the app on a local instance. Please note that for
performance-related reasons, the raw datafiles are not provided on
GitHub. However, all the `.rda` files are available in the `data/`
subfolder.
