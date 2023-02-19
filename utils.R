# --- Shiny utils ---
basemap <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  # pal_fact <- colorFactor(
  #   c("darkorange", "lightgreen"),
  #   # topo.colors(5),
  #   domain = shp$BASIN
  #         )
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    # leaflet::addPolygons(
    #   data = shp,
    #   fillColor = 'white',
    #   # fillColor = 'grey',
    #   # fillColor = ~pal_fact(BASIN),
    #   fillOpacity = 0.7,
    #   col = "black",
    #   opacity = 1,
    #   weight = 2.5,
    #   label = ~paste0("District  ", DISTRICT),
    #   layerId = ~DISTRICT,
    #   labelOptions = labelOptions(
    #     noHide = F,
    #     # direction = 'center',
    #     # textOnly = F)
    #     style = list(
    #       "color" = "black",
    #       "font-weight" = "1000")
    #   )
    # ) %>%
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7)
  
}
