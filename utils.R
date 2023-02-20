# *************************
# ---- leaflet basemap ----
# *************************
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
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7)
  
  
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
  
}

# ********************************************
# ---- Get Water right Net Amounts points ----
# ********************************************

get_wr_pts <- function(
    x, 
    buffer,
    as_sf = TRUE
    ) {
  
  # loop through rows of x and call cdssr::get_water_rights_netamount()
  wrs <- lapply(1:nrow(x), function(i) {
    
    tryCatch({
      
      # message(paste0(i, "/", nrow(x)))
      
      wr_net <- cdssr::get_water_rights_netamount(
        aoi    = x[i, ],
        radius = buffer
      ) 
      wr_net
      
    }, error = function(e)
      
      NULL
    
    )
    
  })
  
  # bind rows together
  wrs <- do.call(rbind, wrs)
  
  # remove water rights that have NA values for stream_mile,
  # call analysis functions only work on WDIDs located on streams
  wrs <- wrs[!is.na(wrs$stream_mile), ]
  
  # if SF object should be returned
  if(as_sf == TRUE) {
    
    # get precise lat/long values
    wrs$longitude <- sprintf("%.8f", wrs$longitude)
    wrs$latitude  <- sprintf("%.8f", wrs$latitude)
    
    # convert to SF points
    wrs <- sf::st_as_sf(
      wrs, 
      coords = c("longitude", "latitude"), 
      crs    = 4326
    )
    
  }
  
  return(wrs)
}


get_nearest_pts <- function(
    flowlines = NULL, 
    pts       = NULL
) {
  # flowlines = fline
  # pts       = wr_pts

  # check if no arguments are provided
  if(any(is.null(flowlines), is.null(pts))) {
    
    stop("Invalid or missing 'flowlines' or 'pts' arguments")
    
  }
  
  # reproject flowlines and pts to projected CRS (5070)
  flowlines <- sf::st_transform(flowlines, 5070)
  pts       <- sf::st_transform(pts, 5070)
  
  # unique terminal paths
  uterm_paths <- unique(flowlines$terminalpa)
  
  # get sum total of each set of flowlines for each terimal path
  path_lengths <- lapply(1:length(uterm_paths), function(i) {
    
    fl <- flowlines[flowlines$terminalpa == uterm_paths[i], ]
  
    data.frame(
      terminalpa = as.character(uterm_paths[i]),
      length     = sum(fl$lengthkm, na.rm = T)
    )
    
    })
  
  # bind rows
  path_lengths <- do.call(rbind, path_lengths)
  
  # get terminal path ID of longest segments of rivers
  term_pa <- path_lengths[path_lengths$length == max(path_lengths$length), ]$terminalpa

  # flow lines associated with terminal paths of more represented stream flowlines
  fls <- flowlines[flowlines$terminalpa == term_pa, ]

  # lowest levelpath stream (mainstem)
  main_stem <- fls[fls$streamleve == min(fls$streamleve), ]
  
  # plot(flowlines$geometry)
  # plot(main_stem$geometry, col = "red", add = T)

  # min hydrosequence (downstream) flow line on lowest levelpath stream (mainstem)
  min_fline <- main_stem[main_stem$hydroseq == min(main_stem$hydroseq), ]
  
  # max hydrosequence (upstream) flow line on lowest levelpath stream (mainstem)
  max_fline <- main_stem[main_stem$hydroseq == max(main_stem$hydroseq), ]
  
  # index of points nearest to min and max hydroseq of mainnstem
  near_min <- sf::st_nearest_feature(min_fline, pts)
  near_max <- sf::st_nearest_feature(max_fline, pts)
  
  # nearest WDIDs to most downstream points of river segments
  min_pt  <- sf::st_transform(pts[near_min, ], 4326)
  max_pt  <- sf::st_transform(pts[near_max, ], 4326)
  
  # add upstream/downstream columns
  min_pt$position <- "downstream"
  max_pt$position <- "upstream"
  
  # add comid columns
  min_pt$comid <- min_fline$comid
  max_pt$comid <- max_fline$comid
  
  # add hydroseq columns
  min_pt$hydroseq <- min_fline$hydroseq
  max_pt$hydroseq <- max_fline$hydroseq
  
  # add stream level columns
  min_pt$stream_lvl <- min_fline$streamleve
  max_pt$stream_lvl <- max_fline$streamleve
  
  # bind the max and min points to single sf object
  end_pts <- rbind(min_pt, max_pt)
  
  return(end_pts)
  
}

get_calls <- function(
    df         = NULL, 
    start_date = NULL,
    end_date   = NULL
    ) {
  
  # check if df is null 
  if(is.null(df)) {
    
    stop("Invalid or missing 'df' argument")
    
  }

  # drop geometry if needed
  df <- sf::st_drop_geometry(df)
  
  # if no start_date given, default to today - 1 year
  if(is.null(start_date)) {
    
    start_date <- Sys.Date() - 365*1
    
  }
  
  # if no end_date given, default to today
  if(is.null(end_date)) {
    
    end_date <- Sys.Date()
    
  }

  call_df <- lapply(1:nrow(df), function(i) {
    
    # message(
    #   paste0("WDID: ", df$wdid[i], " - ",
    #          i, "/", nrow(df))
    # )
    
    tryCatch({
      
      calls <- cdssr::get_call_analysis_wdid(
        wdid       = df$wdid[i],
        admin_no   = "99999.00000",
        start_date = start_date,
        end_date   = end_date
      ) 
      
      # add info columns
      calls$structure_name <- df$structure_name[i]
      calls$structure_type <- df$structure_type[i]
      calls$water_source   <- df$water_source[i]
      calls$position       <- df$position[i]
      calls$comid          <- df$comid[i]
      calls$hydroseq       <- df$hydroseq[i]
      calls$stream_lvl     <- df$stream_lvl[i]
      
      calls
      
    }, error = function(e)
      
      NULL
    
    )
    
  }) 
  
  call_df <- do.call(rbind, call_df)
  
  return(call_df)
  
}

make_calls_plot <- function(df) {
  
  # replace NA dates with the max priority date on record
  df$priority_date <- ifelse(is.na(df$priority_date), max(df$priority_date, na.rm = T), df$priority_date)
  
  # convert dates to date type
  df$priority_date <- as.Date(df$priority_date)
  
  
  admin_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = datetime, y = priority_date, color = analysis_wdid)) +
    ggplot2::labs(
      x     = "Date",
      y     = "Priority Date",
      color = "WDID"
      ) +
    ggplot2::facet_wrap(~position, nrow = 2)+ 
    ggplot2::theme_bw()
  
  return(admin_plot)
  
  
}

















