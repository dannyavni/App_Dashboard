library(leaflet)
library(geojsonio)
library(tidyverse)

make_map <- function() {
  states <- geojsonio::geojson_read("geojson.js", what = "sp")
  states[48, "density"] = 20000
  
  m <- leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addTiles()
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
  
  
  m <- addPolygons(
    m,
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)

  return(m)
}
