% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{mapShpRegs}
\alias{mapShpRegs}
\title{mapShpRegs}
\usage{
mapShpRegs(couleur)
}
\arguments{
\item{couleur}{valeur de couleur degradee selon la densite de la region
Toutes ces palettes peuvent être utilisées conjointement avec colorRamp () et colorRampPalette ().
Voici un affichage de toutes les palettes de couleurs disponibles dans le package RColorBrewer.
> library(RColorBrewer)
> display.brewer.all()
Permet d'afficher une liste de combinaisons de couleurs.}
}
\description{
Creation d'une carte interactive avec LEAFLET
}
\examples{
mapShpRegs("YlOrRd")
}
