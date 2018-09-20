#' uploadSHP
#'
#' Cette fonction permet de charger les fichiers  shapefile, ou « fichier de formes » est un format de fichier pour les systèmes d'informations géographiques (SIG) pour les régions du Maroc
#' @param sans parametres
#' @examples
#' uploadSHP()
#' @export

uploadSHP <- function() {
  print("Chargement des fichiers shp - regions du Maroc....")
  dir.create("data")
  download.file("https://raw.githubusercontent.com/timmimohamed/carteMaroc/master/regions/regions.shp", destfile =paste0(getwd(),"/data/","regions.shp"),method="wininet")
  download.file("https://raw.githubusercontent.com/timmimohamed/carteMaroc/master/regions/regions.prj", destfile =paste0(getwd(),"/data/","regions.prj"),method="wininet")
  download.file("https://raw.githubusercontent.com/timmimohamed/carteMaroc/master/regions/regions.qpj", destfile =paste0(getwd(),"/data/","regions.qpj"),method="wininet")
  download.file("https://raw.githubusercontent.com/timmimohamed/carteMaroc/master/regions/regions.dbf", destfile =paste0(getwd(),"/data/","regions.dbf"),method="wininet")
  download.file("https://raw.githubusercontent.com/timmimohamed/carteMaroc/master/regions/regions.shx", destfile =paste0(getwd(),"/data/","regions.shx"),method="wininet")

}


#' plotSHP
#'
#' Cette fonction permet de ploter la carte des regions du Maroc
#' @param chemin valeur de chemin : l'eplacement ou ce trouve le fichier shapefile
#' @examples
#' plotSHP("data/shp_nouveau_decoupage_territorial_2015")
#' @export

plotSHP <- function(chemin) {
  print("ploting - regions du Maroc....")
  shp <- readOGR(dsn=chemin,layer = 'region')
  plot(shp)
}

#' dataShpReg
#'
#' Cette fonction permet de récupérer le contenu (data) du fichier shapefile de regions du Maroc
#' @param chemin valeur de chemin : l'eplacement ou ce trouve le fichier shapefile
#' @return data shapefile
#' @examples
#' dataShpReg("data/shp_nouveau_decoupage_territorial_2015")
#' @export

dataShpReg <- function(chemin) {
  shp <-readOGR(dsn=chemin,layer = 'region')
  return(shp)
}
dataShpBas <- function(chemin) {
  shp <-readOGR(dsn=chemin,layer = 'bassins')
  return(shp)
}

#' mapShpRegs0
#'
#' Creation d'une carte interactive avec LEAFLET
#' @param shp valeur de data du fichier shapefile
#' @param density valeur de density (nombre d'ahibitants par km)
#' @param lab valeur de label
#' @examples
#' shp <- dataShpReg("data/shp_nouveau_decoupage_territorial_2015")
#' reg <- read.csv("data/regions.csv", stringsAsFactors = FALSE)
#' lab<- sprintf(
#' "<strong>%s</strong><br>%s Hab/Km2",
#' reg$name,reg$density
#' ) %>% lapply(htmltools::HTML)
#' mapShpRegs0(shp,density,lab)
#' @export

mapShpRegs0<- function(shp,density,lab) {

  bins <- c(0, 5, 10, 20, 40, 100, 200, 250, 300, Inf)
  pall <- colorBin("BuGn", domain = density, bins = bins)

  leaflet() %>%addTiles() %>%
    setView(lng = -7.092620000000011, lat=31.791702,zoom=6)   %>%
    addPolygons(
      data= spTransform(shp, CRS("+proj=longlat +ellps=GRS80")),
      weight = 2,
      opacity = 1,
      fillColor = ~pall(density),
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = lab,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    )%>%
    addLegend(pal = pall, values = density, opacity = 0.7, title = NULL,
              position = "bottomright")

}

#' mapShpReg
#'
#' Cette fonction permet de selectionner la region dont son nom passe au parametre
#' @param shp valeur de data du fichier shapefile
#' @param reg valeur de la region a afficher
#' @param coul valeur de la couleur de la region choisis
#' @examples
#' shp <- dataShpReg("data/shp_nouveau_decoupage_territorial_2015")
#' reg = "Fes-Meknes"
#' mapShpReg(shp,reg,"green")
#' @export

mapShpReg<- function(shp,reg,coul) {
  shp <- subset(shp, shp$name %in% c(reg))
  leaflet() %>%addTiles() %>%
    setView(lng = -7.092620000000011, lat=31.791702,zoom=6)   %>%
    addPolygons(
      data= spTransform(shp, CRS("+proj=longlat +ellps=GRS80")),
      weight = 2,
      opacity = 1,
      color = coul,
      dashArray = "3",
      fillOpacity = 0.7,
      label = reg
    )

}

#' mapShpDensPop
#'
#' Cette fonction permet de selectionner la region dont son nom passe au parametre
#' @param shp valeur de data du fichier shapefile
#' @param reg valeur de la region a afficher
#' @param coul valeur de la couleur de la region choisis
#' @examples
#' shp <- dataShpReg("data/shp_nouveau_decoupage_territorial_2015")
#' reg = "Fes-Meknes"
#' mapShpDensPop(shp,reg,"green")
#' @export

mapShpDensPop<- function(shp,reg,coul,dens) {
  shp <- subset(shp, shp$name %in% c(reg))
  leaflet() %>%addTiles() %>%
    setView(lng = -7.092620000000011, lat=31.791702,zoom=6)   %>%
    addPolygons(
      data= spTransform(shp, CRS("+proj=longlat +ellps=GRS80")),
      weight = 2,
      opacity = 1,
      color = coul,
      dashArray = "3",
      fillOpacity = 0.7,
      label = reg
    )%>%
    addCircles(long,etab$lat, weight = 1,radius = sqrt(etab$nbr_eleve) * 50,
               popup =  lab, fillOpacity = 0.5)

}


#' mapShpRegs
#'
#' Creation d'une carte interactive avec LEAFLET
#' @param couleur valeur de couleur degradee selon la densite de la region
#' Toutes ces palettes peuvent être utilisées conjointement avec colorRamp () et colorRampPalette ().
#' Voici un affichage de toutes les palettes de couleurs disponibles dans le package RColorBrewer.
#' > library(RColorBrewer)
#' > display.brewer.all()
#' Permet d'afficher une liste de combinaisons de couleurs.
#' @examples
#' mapShpRegs("YlOrRd")
#' @export

mapShpRegs<- function(couleur) {

  bins <- c(0, 5, 10, 20, 40, 100, 200, 250, 300, Inf)
  pall <- colorBin(couleur, domain = shpMaroc@data$density, bins = bins)
  lab<- sprintf(
    "<strong>%s</strong><br>%s Hab/Km2",
    shpMaroc@data$name,shpMaroc@data$density
  ) %>% lapply(htmltools::HTML)

  leaflet() %>%addTiles() %>%
    setView(lng = -7.092620000000011, lat=31.791702,zoom=6)   %>%
    addPolygons(
      data= shpMaroc,
      weight = 2,
      opacity = 1,
      fillColor = ~pall(shpMaroc@data$density),
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = lab,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    )%>%
    addLegend(pal = pall, values = shpMaroc@data$density, opacity = 0.7, title = NULL,
              position = "bottomright")

}


#' mapCsvRegs
#'
#' Cette fonction permet de tracer la carte du Maroc - regions
#' @param col valeur de la couleur des frontieres des regions
#' @param we valeur de l'paisseur es frontieres des regions
#' @examples
#' output$regions <- renderLeaflet({
#' mapCsvRegs("red",1.25)
#' })
#' @export

mapCsvRegs<- function(col,we) {

# turn into SpatialLines
split_data = lapply(unique(gpsMaroc$group), function(x) {
  df = as.matrix(gpsMaroc[gpsMaroc$group == x, c("Longitude", "Latitude")])
  lns = Lines(Line(df), ID = x)
  return(lns)
})

data_lines = SpatialLines(split_data)

leaflet(data_lines) %>%
  addTiles() %>%
  addPolylines(color = col, weight=we)
}


#' mapCsvReg
#'
#' Cette fonction permet de tracer une region choisis du Maroc
#' @param num de la region a afficher
#' @param col valeur de la couleur des frontieres des regions
#' @param we valeur de l'paisseur es frontieres des regions
#' @examples
#' output$regions <- renderLeaflet({
#' mapCsvReg(6,"red",1.25)
#' })
#' @export

mapCsvReg<- function(num,col,we) {
  gpsMaroc <- subset(gpsMaroc, gpsMaroc$group %in% c(num))

  # turn into SpatialLines
  split_data = lapply(unique(gpsMaroc$group), function(x) {
    df = as.matrix(gpsMaroc[gpsMaroc$group == x, c("Longitude", "Latitude")])
    lns = Lines(Line(df), ID = x)
    return(lns)
  })

  data_lines = SpatialLines(split_data)

  leaflet(data_lines) %>%
    addTiles() %>%
    addPolylines(color = col, weight=we) %>%
    addMarkers(lng = -7.092620000000011, lat=31.791702)
}


#' mapCsvRegMark
#'
#' Cette fonction permet de tracer une region choisis du Maroc et d'ajouter des Markers selon le choix d'utilisateur
#' @param num valeur de la region a afficher
#' @param col valeur de la couleur des frontieres des regions
#' @param we valeur de l'paisseur des frontieres des regions
#' @param long valeur d'un vecteur de longitudes
#' @param lat valeur d'un vecteur de latitudes
#' @param pop valeur de popup
#' @examples
#' etab <- read.csv("data/population_Fes_Meknes/etab_meknes.csv", stringsAsFactors = FALSE)
#' mapCsvRegMark(3,"green",1.25,etab$long,etab$lat,etab$nbr_eleve)
#' @export

mapCsvRegMark<- function(num,col,we,long,lat,pop) {
  gpsMaroc <- subset(gpsMaroc, gpsMaroc$group %in% c(num))

  # turn into SpatialLines
  split_data = lapply(unique(gpsMaroc$group), function(x) {
    df = as.matrix(gpsMaroc[gpsMaroc$group == x, c("Longitude", "Latitude")])
    lns = Lines(Line(df), ID = x)
    return(lns)
  })

  data_lines = SpatialLines(split_data)

  leaflet(data_lines) %>%
    addTiles() %>%
    addPolylines(color = col, weight=we) %>%
    addMarkers(lng = long, lat=lat,popup = pop)
}
