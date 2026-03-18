
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mastiff <img src="man/figures/mastiff_logo_2.svg" align="right" height="138" /></a>

<!-- badges: start -->

<!-- badges: end -->

mastiff is short for MAthematical and STatistical utIlities by the
Fraser group at oxFord. That’s [Christophe Fraser’s
group](https://www.bdi.ox.ac.uk/research/fraser-pathogen-dynamics-group)
in the [Pandemic Sciences Institute](https://www.psi.ox.ac.uk/),
University of Oxford. We work on infectious disease epidemiology. This
package will collect R and stan code we’ve written to implement
mathematical and statistical methods (and bits of code that facilitate
those) that has usefulness beyond infectious disease epidemiology.

## Installation

One or all of the following three methods should work.

1.  In R:

``` r
install.packages("remotes") # if not already installed
remotes::install_github("BDI-pathogens/mastiff", build_vignettes = TRUE)
```

2.  In a terminal / from the command line:

``` bash
git clone https://github.com/BDI-pathogens/mastiff.git
```

Then in R, change your directory to the repository you’ve just cloned,
and run

``` r
install.packages("devtools") # if not already installed
devtools::install(".", build_vignettes = TRUE)
```

3.  In R:

``` r
install.packages("pak") # if not already installed
pak::pak("BDI-pathogens/mastiff")
```

<br/><br/> mastiff logo by [Lucy Back](https://github.com/1ucyb).
