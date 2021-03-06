% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{mapCsvRegMark}
\alias{mapCsvRegMark}
\title{mapCsvRegMark}
\usage{
mapCsvRegMark(num, col, we, long, lat, pop)
}
\arguments{
\item{num}{valeur de la region a afficher}

\item{col}{valeur de la couleur des frontieres des regions}

\item{we}{valeur de l'paisseur des frontieres des regions}

\item{long}{valeur d'un vecteur de longitudes}

\item{lat}{valeur d'un vecteur de latitudes}

\item{pop}{valeur de popup}
}
\description{
Cette fonction permet de tracer une region choisis du Maroc et d'ajouter des Markers selon le choix d'utilisateur
}
\examples{
etab <- read.csv("data/population_Fes_Meknes/etab_meknes.csv", stringsAsFactors = FALSE)
mapCsvRegMark(3,"green",1.25,etab$long,etab$lat,etab$nbr_eleve)
}
