% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{mapShpRegs0}
\alias{mapShpRegs0}
\title{mapShpRegs0}
\usage{
mapShpRegs0(shp, density, lab)
}
\arguments{
\item{shp}{valeur de data du fichier shapefile}

\item{density}{valeur de density (nombre d'ahibitants par km)}

\item{lab}{valeur de label}
}
\description{
Creation d'une carte interactive avec LEAFLET
}
\examples{
shp <- dataShpReg("data/shp_nouveau_decoupage_territorial_2015")
reg <- read.csv("data/regions.csv", stringsAsFactors = FALSE)
lab<- sprintf(
"<strong>\%s</strong><br>\%s Hab/Km2",
reg$name,reg$density
) \%>\% lapply(htmltools::HTML)
mapShpRegs0(shp,density,lab)
}
