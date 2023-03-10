% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcLoad.R
\name{calcLoad}
\alias{calcLoad}
\title{Calculate multiple load time series for different years (REMIND to PyPSA)}
\usage{
calcLoad(rmFile, pyLoad, outDir, years)
}
\arguments{
\item{rmFile}{REMIND output gdx file}

\item{pyLoad}{PyPSA load.csv file}

\item{outDir}{Output folder for load_yXXXX.csv files}

\item{years}{Years coupled from REMIND to PyPSA}
}
\value{
Writes one csv file for each year into outDir.
}
\description{
Calculate multiple load time series for different years (REMIND to PyPSA)
}
