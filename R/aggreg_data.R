# Angus Watters
# Collect data from CDSS to use in Shiny App

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)

# ***********************
# ---- Paths to data ----
# ***********************

# Paths
wr_net_path    <- "data/water_right_netamounts.rds"
wr_pts_path    <- "data/water_right_netamounts_pts.rds"
districts_path <- "data/water_districts_tbl.rds"

# ***********************************
# ---- get water districts table ----
# ***********************************

if(file.exists(districts_path)) {
  
  message(paste0("Reading data from:\n---> ", districts_path))
  
  water_dists <- readRDS(districts_path)
  
} else {
  
  message(paste0("Data not found at path:\n---> ", districts_path))
  
  # water districts reference table
  water_dists <- cdssr::get_reference_tbl("waterdistricts")
  
  message(paste0("Saving data to path:\n---> ", districts_path))
  
  # save water districts reference table data
  saveRDS(water_dists, districts_path)
  
}

# ***************************************************
# ---- Water rights netamounts by water district ----
# ***************************************************

if(file.exists(wr_net_path)) {
  
  message(paste0("Reading data from:\n---> ", wr_net_path))
  
  wr_net <- readRDS(wr_net_path)
  
} else {
  
  message(paste0("Data not found at path:\n---> ", wr_net_path))
  
  wr_net <- lapply(1:nrow(water_dists), function(i) {
    
    message(paste0("District: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")"))
    
    # GET request to CDSS API
    tryCatch({
      
      wr_net <- cdssr::get_water_rights_netamount(
        water_district    = water_dists$water_district[i]
      )
      wr_net
    },
    error = function(e) {
      
      NULL
      
    })
    
  }) %>%
    dplyr::bind_rows()
  
  message(paste0("Saving data to path:\n---> ", wr_net_path))
  
  # save water rights netamount data
  saveRDS(wr_net, wr_net_path)
  
  if(!file.exists(wr_pts_path)) {
    
    wr_pts <- 
      wr_net %>% 
      dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>% 
      dplyr::filter(!is.na(stream_mile)) %>% 
      sf::st_as_sf(
        coords = c("longitude", "latitude"), 
        crs    = 4326
      )  
  
  message(paste0("Saving spatial data to path:\n---> ", wr_pts_path))
    
  # save water rights netamount spatial data
  saveRDS(wr_pts, wr_pts_path)
  
  }

}

# ***********************
# ---- get mainstems ----
# ***********************

# path to mainstem data
ms_path <- "data/nhd_mainstems.rds"

if(file.exists(ms_path)) {
  
  message(paste0("Reading data from:\n---> ", ms_path))
  
  main_stems <- readRDS(ms_path)
  
} else {
  
  message(paste0("Data not found at path:\n---> ", ms_path))
  
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
      realization = "flowline"
    )

    tryCatch({
      
      # minimum stream level of mainstem
      min_lvl <- 
        huc_net %>%
        # huc_outs %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::group_by(terminalpa) %>%
        dplyr::filter(streamleve == min(streamleve)) %>% 
        dplyr::filter(streamorde >= 3) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(huc4 = huc4s$huc4[i]) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character))
      
      # longest levelpath will be the main flowline/mainstem
      main_lvlpath <- 
        min_lvl %>% 
        dplyr::group_by(levelpathi) %>% 
        dplyr::summarise() %>% 
        dplyr::mutate(
          lengths = sf::st_length(geometry)
        ) %>% 
        dplyr::slice(which.max(lengths)) %>% 
        .$levelpathi
      
      # filter to the main levelpathi
      min_lvl <- 
        min_lvl %>% 
        dplyr::filter(levelpathi == main_lvlpath)
    
    min_lvl
    
    }, error = function(e) {
      
      message(paste0("Skipping iteration: ", i, "Error:\n", e))
      
      NULL
    
      })
    
    # plot(huc_net$geometry)
    # plot(min_lvl$geometry, lwd = 2, color = "red", add = T)
    # plot(um_net$geometry, color = "red", add = T)
    
    # min_lvl %>% dplyr::mutate(streamleve = as.character(streamleve),
    #            streamorde = as.character(streamorde)) %>% 
    #   ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(color = hydroseq))
    
    # mapview::mapview(ends, color = "green") +
    #   mapview::mapview(um_net, color = "blue") +
    #   mapview::mapview(min_lvl, color = "red")
    
  }) %>% 
    dplyr::bind_rows()
  
  # save rds
  saveRDS(main_stems, ms_path)
  
}

# ***************************
# ---- get call_analysis ----
# ***************************

ms_path <- "data/nhd_mainstems.rds"

if(file.exists(ms_path)) {
  
  message(paste0("Reading data from:\n---> ", ms_path))
  
  main_stems <- readRDS(ms_path)
  
} else {
  
  message(paste0("Data not found at path:\n---> ", ms_path))
  
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
  
  # i <- 9
  # rm(i, min_lvl, huc_net, huc_net2, huc_net3, main_lvlpath, main_stems)
  
  # loop over each huc4 and get mainstem of the river
  main_stems <- lapply(1:nrow(huc4s), function(i) {
    
    message(paste0(i, "/", nrow(huc4s)))
    
    huc_net <- nhdplusTools::get_nhdplus(
      AOI = huc4s[i, ],
      realization = "flowline"
    )
    
    tryCatch({
      
      # minimum stream level of mainstem
      min_lvl <- 
        huc_net %>%
        # huc_outs %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::group_by(terminalpa) %>%
        dplyr::filter(streamleve == min(streamleve)) %>% 
        dplyr::filter(streamorde >= 3) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(huc4 = huc4s$huc4[i]) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character))
      
      # longest levelpath will be the main flowline/mainstem
      main_lvlpath <- 
        min_lvl %>% 
        dplyr::group_by(levelpathi) %>% 
        dplyr::summarise() %>% 
        dplyr::mutate(
          lengths = sf::st_length(geometry)
        ) %>% 
        dplyr::slice(which.max(lengths)) %>% 
        .$levelpathi
      
      # filter to the main levelpathi
      min_lvl <- 
        min_lvl %>% 
        dplyr::filter(levelpathi == main_lvlpath)
      
      min_lvl
      
    }, error = function(e) {
      
      message(paste0("Skipping iteration: ", i, "Error:\n", e))
      
      NULL
      
    })
    
    # plot(huc_net$geometry)
    # plot(min_lvl$geometry, lwd = 2, color = "red", add = T)
    # plot(um_net$geometry, color = "red", add = T)
    
    # min_lvl %>% dplyr::mutate(streamleve = as.character(streamleve),
    #            streamorde = as.character(streamorde)) %>% 
    #   ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(color = hydroseq))
    
    # mapview::mapview(ends, color = "green") +
    #   mapview::mapview(um_net, color = "blue") +
    #   mapview::mapview(min_lvl, color = "red")
    
    # # minimum hydroseq
    # min_hydro <- 
    #   min_lvl %>% 
    #   dplyr::filter(hydroseq == min(hydroseq))
    # um_net <- lapply(1:nrow(min_hydro), function(x) {
    #   fl <- nhdplusTools::navigate_network(
    #     start       = min_hydro[x, ],
    #     mode        = "UM",
    #     distance_km = 550
    #   ) %>%
    #     dplyr::mutate(huc4 = huc4s$huc4[i]) %>%
    #     dplyr::mutate(dplyr::across(c(-geometry), as.character))
    #   fl
    # }) %>% 
    #   dplyr::bind_rows()
    
    # strm_lvl <- 
    #   huc_net %>%
    #   dplyr::filter(streamcalc != 0) %>%
    #   dplyr::group_by(terminalpa) %>%
    #   dplyr::filter(streamleve == min(streamleve)) %>% 
    #   dplyr::ungroup() %>% 
    #   dplyr::mutate(streamleve = as.character(streamleve), streamorde = as.character(streamorde)) %>% 
    #   dplyr::group_by(streamleve) %>%
    #   dplyr::summarise() %>% 
    #   dplyr::mutate(lengths = sf::st_length(geometry)) %>% 
    #   dplyr::slice(which.max(lengths)) %>% 
    #   .$streamleve 
    # min_lvl <-
    #   huc_net %>%
    #   dplyr::filter(streamcalc != 0) %>%
    #   dplyr::filter(streamleve %in% c(strm_lvl), streamorde >= 3)
  }) %>% 
    dplyr::bind_rows()
  
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = levelpathi))
  # mapview::mapview(main_stems) + huc4s
  
  # save rds
  saveRDS(main_stems, ms_path)
  
}
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

# save rds
saveRDS(main_stems. ms_path)

