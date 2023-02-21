# REMIND-PyPSA model coupling scripts

R package **remindPypsa**, version **0.0.1.9010**

[![CRAN status](https://www.r-pkg.org/badges/version/remindPypsa)](https://cran.r-project.org/package=remindPypsa)  [![R build status](https://github.com/pik-piam/remindPypsa/workflows/check/badge.svg)](https://github.com/pik-piam/remindPypsa/actions) [![codecov](https://codecov.io/gh/pik-piam/remindPypsa/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/remindPypsa) 

## Purpose and Functionality

This package contains functions for the iterative bi-directional model coupling of the integrated assessment model REMIND and the spatio-temporally detailed power and energy system model family PyPSA.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remindPypsa")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Adrian Odenweller <adrian.odenweller@pik-potsdam.de>.

## Citation

To cite package **remindPypsa** in publications use:

Odenweller A (2023). _remindPypsa: REMIND-PyPSA model coupling scripts_. R package version 0.0.1.9010, <https://github.com/pik-piam/remindPypsa>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remindPypsa: REMIND-PyPSA model coupling scripts},
  author = {Adrian Odenweller},
  year = {2023},
  note = {R package version 0.0.1.9010},
  url = {https://github.com/pik-piam/remindPypsa},
}
```
