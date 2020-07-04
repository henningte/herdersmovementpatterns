
<!-- README.md is generated from README.Rmd. Please edit that file -->

# herdersmovementpatterns

This repository contains the data and code for our paper:

> Henning Teickner, Christian Knoth, Thomas Bartoschek, Kati Kraehnert,
> Melinda Vigh, Myagmartseren Purevtseren, Munkhnaran Sugar, Edzer
> Pebesma, (2020). *Patterns in Mongolian Nomadic Household Movement
> Derived from GPS Trajectories*. Applied Geography.

### How to cite

Please cite this compendium as:

> Henning Teickner, Christian Knoth, Thomas Bartoschek, Kati Kraehnert,
> Melinda Vigh, Myagmartseren Purevtseren, Munkhnaran Sugar, Edzer
> Pebesma, (2020). *Compendium of R code and data for ‘Patterns in
> Mongolian Nomadic Household Movement Derived from GPS Trajectories’*.
> Accessed 04 Jul 2020. Online at
> <https://github.com/henningte/herdersmovementpatterns>.

### How to download or install

You can install this compendium as an R package,
herdersmovementpatterns, from GitHub with:

``` r
remotes::install_github("henningte\herdersmovementpatterns")
```

You will also have to install the package herdersTA:

``` r
remotes::install_github("henningte\herdersTA")
```

### How use

You can reproduce the analyses with:

``` r
rmarkdown::render("./analysis/paper/paper.Rmd")
```

Moreover, herdersmovementpatterns contains a set of functions to compute
movement summary indicators from GPS trajectories preprocessed with
herdersTA. See `?herdersmovementpatterns::mp_si` for a brief overview.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :**

1.  `herdersmovementpattern::d`:
    [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
    attribution requested in reuse.

2.  Shapefile for the Mongolian administrative districts (soums) (data
    in folder “analysis/data/derived\_data”): See
    <https://data.humdata.org/dataset/a9b0a8a6-cb14-448e-b35c-aa5eb51b0557>.
    (Freely available for non-commercial humanitarian use). Source:
    National Statistical Office of Mongolia.

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
