# Shiny & Flexdashboard packages
library(shiny)
library(flexdashboard)

# Data manipulation
# library(tidyr)
# library(dplyr)
library(magrittr)
library(ggplot2)

# Mapping packages
library(leaflet)
library(leafem)
# library(sf)

# water data
# library(nhdplusTools)
# library(cdssr)

# Load function data_utils.R file
source('utils.R')

# water districts
dist <- sf::read_sf("data/water_districts_simple.geojson")

# water rights net amounts data
wr_net <- readRDS("data/water_right_netamounts.rds")

# water rights net amounts spatial data
wr_pts <- readRDS("data/water_right_netamounts_pts.rds")

# colorado state geometry
co <- AOI::aoi_get(state = "CO")

# HUC8s
hucs <- nhdplusTools::get_huc8(co) 

# convert HUC8s to HUC4
huc4s <-
  hucs %>% 
  dplyr::mutate(
    huc4 = substr(huc8, 1, 4)
  ) %>% 
  dplyr::group_by(huc4) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  sf::st_crop(co)

# loop over each huc4 and get mainstem of the river
main_stems <- lapply(1:nrow(huc4s), function(i) {
  message(paste0(i, "/", nrow(huc4s)))

  huc_net <- nhdplusTools::get_nhdplus(
    AOI = huc4s[i, ],
    realization = "outlet"
    )
  
  starts  <-
    huc_net %>% 
    dplyr::filter(startflag == 1) %>% 
    dplyr::filter(hydroseq == min(hydroseq))

  dm_net <- nhdplusTools::navigate_network(
                          start       = starts,
                          mode        = "DM",
                          distance_km = 250
                        ) %>% 
    dplyr::mutate(huc4 = huc4s$huc4[i]) %>% 
    dplyr::mutate(dplyr::across(c(-geometry), as.character))
  
  dm_net

}) %>% 
  dplyr::bind_rows()
mapview::mapview(hucs) +co + huc4s
# 39.7792 -105.7214
# 40.69548 -105.2631
pt <- sf::st_as_sf(
  data.frame(
    lat = "40.69548",
    lng =  "-105.2631"
  ),
  coords = c("lng", "lat"),
  crs = 4326
  )

fline <- get_us_net(
  pt       = pt, 
  distance = 250
)

pt_buff <- 
  pt %>% 
  sf::st_transform(5070) %>% 
  sf::st_buffer(10000)
hucs <- nhdplusTools::get_huc8(pt_buff)

pt_area <- nhdplusTools::get_nhdarea(AOI = pt_buff)
pt_net <- nhdplusTools::get_nhdplus(AOI = hucs[2,])
mapview::mapview(fline) + pt + pt_buff + pt_net + hucs
# clicked water district
click_dist <- sf::st_filter(dist, pt)

mapview::mapview(click_dist) + pt + dist

click_dist$DISTRICT
system.time(
wr_net <- cdssr::get_water_rights_netamount(
  aoi    = pt[1, ],
  radius = 15
)
)

system.time(
  wr_net2 <- cdssr::get_water_rights_netamount(
    # aoi    = pt[1, ],
    # radius = 15
    water_district = click_dist$DISTRICT
  )
)
# buffer to search area for NHD features
# buff <- sf::st_buffer(pt, 1500)
buff <- sf::st_transform(
  sf::st_buffer(
    sf::st_transform(pt, 5070),
    1609.3*7
  ),
  4326
)
# sf::st_transform(pt, 5070)
# # buffer bounds
bounds <-
  # buff %>%
  buff %>%
  # sf::st_buffer(5250) %>%
  sf::st_bbox() %>%
  as.vector()
# nhdplusTools::
comid_pt <- dataRetrieval::findNLDI(location = pt)

um_net <- nhdplusTools::navigate_network(
  start       = comid_pt,
  mode        = "UM",
  distance_km = 20
)
# Get flowlines and catchment geometries
area_nhd <- nhdplusTools::get_nhdplus(
  AOI         = buff,
  realization = c("flowline", "catchment"),
  streamorder = 3
)

# Get flowlines and catchment geometries
pt_nhd <- nhdplusTools::get_nhdplus(
  AOI         = pt,
  realization = "all"
  # streamorder = 3
)


# area_nhd$flowline %>% 
#   dplyr::mutate(
#     streamorde = as.character(streamorde),
#     streamleve = as.character(streamleve)
#                 ) %>% 
#   sf::st_buffer(1609.3) %>% 
# ggplot2::ggplot() +
#   ggplot2::geom_sf(ggplot2::aes(color = streamleve))
# NHD flowlines
out   <- pt_nhd$outlet

# NHD flowlines
fline <- pt_nhd$flowline

# NHD Catchment
catch <- pt_nhd$catchment

mapview::mapview(out) + fline + catch

um_net <- nhdplusTools::navigate_network(
  start       = out,
  mode        = "UM",
  distance_km = 20
  )

ut_net <- nhdplusTools::navigate_network(
  start       = out,
  mode        = "UT",
  distance_km = 20
)
# mapview::mapview(out) + fline + catch
  mapview::mapview(ut_net, color = "blue") +
  mapview::mapview(um_net, color = "red") +
  mapview::mapview(fline, color = "green") +
  mapview::mapview(catch, col.regions = "yellow")  +
  mapview::mapview(out, color = "black") +
  pt
# # NHD flowlines
# outlet <- area_nhd$flowline
# 
# # NHD flowlines
# fline <- area_nhd$flowline
# 
# # NHD Catchment
# catch <- area_nhd$catchment

min_fline <- sf::st_as_sf(
  sf::st_cast(
    sf::st_union(
      sf::st_buffer(
        sf::st_transform(
          fline[fline$streamleve == min(fline$streamleve), ],
          5070
        ),
        1609.3
      )
    ),
    "POLYGON"
  )
)

# get Water right points
wr_pts <- get_wr_pts(x = min_fline, buffer = 10)

# # Get flowlines and catchment geometries
# area_nhd <- nhdplusTools::get_nhdplus(
#   AOI         = buff,
#   realization = c("flowline", "catchment"),
#   streamorder = 3
#   )
# 
# # NHD flowlines
# fline <- area_nhd$flowline
# 
# # NHD Catchment
# catch <- area_nhd$catchment

# mapview::mapview(buff) + pt + wr_pts + catch + fline

# # get points nearest to the furthest upstream and downstream main stem river segments
# end_pts <- get_nearest_pts(
#               flowlines = fline,
#               pts       = wr_pts
#               )

# get points nearest to the furthest upstream and downstream main stem river segments
main_stem <- get_main_stem(
  flowlines = fline
)

# get points nearest to the furthest upstream and downstream main stem river segments
end_pts <- get_nearest_pts(
  flowlines = main_stem,
  pts       = wr_pts
)

# get water right calls timeseries
call_df <- get_calls(
  df         = end_pts,
  start_date = Sys.Date() - 365,
  end_date   = Sys.Date()
)


call_ts %>% 
  dplyr::mutate(
    day   = lubridate::yday(datetime),
    year  = as.character(year),
    month = as.character(month)
  ) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = day, y = priority_date, color = year), size = 1) +
  gghighlight::gghighlight(year %in% c("2002"))
