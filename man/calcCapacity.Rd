% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcCapacity.R
\name{calcCapacity}
\alias{calcCapacity}
\title{Calculate pre-investment capacities (REMIND to PyPSA)
This function calculates capacities after
This function does two things:
1. Scaling of the PyPSA power plant database for conventional generators and
output as powerplants_y***.csv files
2. Output of}
\usage{
calcCapacity(rmFile, pyPowerplants, rm2pyTech, outDir, years)
}
\arguments{
\item{rmFile}{REMIND output gdx file}

\item{pyPowerplants}{PyPSA powerplants.csv file}

\item{rm2pyTech}{REMIND to aggregated PyPSA technology mapping}

\item{outDir}{Output folder for csv files}

\item{years}{Years coupled from REMIND to PyPSA}
}
\value{
Writes one csv file for each year into outDir.
}
\description{
(i) scales the PyPSA power plant database for conventional
generators and exports this as a csv file and (ii) exports another csv file
for the pre-investment capacities of renewable generators.
}
