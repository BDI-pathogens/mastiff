# mastiff

mastiff is short for **ma**thematical and **st**atistical ut**i**lities
by the **F**raser group at Ox**f**ord. That’s [Christophe Fraser’s
group](https://www.bdi.ox.ac.uk/research/fraser-pathogen-dynamics-group)
in the [Pandemic Sciences Institute](https://www.psi.ox.ac.uk/),
University of Oxford. We work on infectious disease epidemiology. This
package will collect mathematical and statistical methods we write in R
(and stan) that have usefulness beyond infectious disease epidemiology.

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

  
  
mastiff logo by [Lucy Back](https://github.com/1ucyb).
