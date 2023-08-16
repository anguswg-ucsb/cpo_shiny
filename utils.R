
# leaflet basemap 
basemap <- function() {

  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7)
  
}

# leaflet basemap w/ HUC4 shape
huc_basemap <- function(shp) {
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7) %>% 
    leaflet::addPolygons(
      data = shp,
      # group = "base_hucs",
      fillColor = 'white',
      # fillColor = 'grey',
      # fillColor = ~pal_fact(BASIN),
      fillOpacity = 0.7,
      col = "black",
      opacity = 1,
      weight = 2.5,
      label = ~paste0("HUC4: ", huc4),
      layerId = ~huc4,
      labelOptions = labelOptions(
        noHide = F,
        # direction = 'center',
        # textOnly = F)
        style = list(
          "color" = "black",
          "font-weight" = "1000")
      )
  )
  
}

# leaflet basemap w/ HUC4 shape
model_basemap <- function(shp) {
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7) %>% 
    leaflet::addPolygons(
      data = shp,
      # group = "base_hucs",
      fillColor = 'white',
      # fillColor = 'grey',
      # fillColor = ~pal_fact(BASIN),
      fillOpacity = 0.7,
      col = "black",
      opacity = 1,
      weight = 2.5,
      label = ~paste0("District: ", DISTRICT),
      layerId = ~DISTRICT,
      labelOptions = labelOptions(
        noHide = F,
        # direction = 'center',
        # textOnly = F)
        style = list(
          "color" = "black",
          "font-weight" = "1000")
      )
    )
  
}

# leaflet basemap w/ HUC4 shape
dist_basemap <- function(shp) {
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    leaflet::addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7) %>% 
    leaflet::addPolygons(
      data = shp,
      # group = "base_hucs",
      fillColor = 'white',
      # fillColor = 'grey',
      # fillColor = ~pal_fact(BASIN),
      fillOpacity = 0.7,
      col = "black",
      opacity = 1,
      weight = 2.5,
      label = ~paste0("District: ", DISTRICT),
      layerId = ~DISTRICT,
      labelOptions = labelOptions(
        noHide = F,
        # direction = 'center',
        # textOnly = F)
        style = list(
          "color" = "black",
          "font-weight" = "1000")
      )
    )
  
  # pal <- leaflet::colorFactor(
  #   palette = "RdYlBu",
  #   domain = sub_flines$gnis_name)
  # # dynamically alter leaflet map
  # leaflet::leaflet() %>%
  #   leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
  #   leaflet::addScaleBar("bottomleft") %>%
  #   leafem::addMouseCoordinates() %>%
  #   leaflet::setView(lng = -105.6, lat = 39.7, zoom = 7) %>% 
  #   # leaflet::addPolygons(
  #   #   data = dists,
  #   #   # group = "base_hucs",
  #   #   fillColor = 'white',
  #   #   # fillColor = 'grey',
  #   #   # fillColor = ~pal_fact(BASIN),
  #   #   fillOpacity = 0.7,
  #   #   col = "black",
  #   #   opacity = 1,
  #   #   weight = 2.5,
  #   #   label = ~paste0("District: ", DISTRICT),
  #   #   layerId = ~DISTRICT,
  #   #   labelOptions = labelOptions(
  #   #     noHide = F,
  #   #     # direction = 'center',
  #   #     # textOnly = F)
  #   #     style = list(
  #   #       "color" = "black",
  #   #       "font-weight" = "1000")
  #   #   )
  #   # )
  #   # leaflet::removeShape(layerId = levelpathi) %>%
  #   leaflet::addPolygons(
  #     data         = dists,
  #     fillColor    = 'white',
  #     # fillColor = 'grey',
  #     # fillColor = ~pal_fact(BASIN),
  #     fillOpacity  = 0.7,
  #     col          = "black",
  #     opacity      = 1,
  #     weight       = 2.5,
  #     label        = ~paste0("District: ", DISTRICT),
  #     layerId      = ~DISTRICT,
  #     labelOptions = leaflet::labelOptions(
  #       noHide = F,
  #       style  = list(
  #         "color" = "black",
  #         "font-weight" = "1000")
  #     )
  #   ) %>%
  #   leaflet::addPolygons(
  #     data         = sub_dist,
  #     fillColor    = '#3EB489',
  #     # fillColor = 'grey',
  #     # fillColor = ~pal_fact(BASIN),
  #     fillOpacity  = 0.5,
  #     col          = "black",
  #     opacity      = 1,
  #     weight       = 3,
  #     group        = "subdist",
  #     label        = ~paste0("District: ", DISTRICT),
  #     layerId      = ~DISTRICT,
  #     labelOptions = leaflet::labelOptions(
  #       noHide = F,
  #       style  = list(
  #         "color" = "black",
  #         "font-weight" = "1000")
  #     )
  #   ) %>%
  #   leaflet::addCircleMarkers(
  #     # data        = sub_pts(),
  #     data        = sub_pts,
  #     radius      = 6,
  #     stroke      = FALSE,
  #     color       = "black",
  #     opacity     = 0.6,
  #     fillColor   = "black",
  #     fillOpacity = 0.6,
  #     label       = ~paste0("WDID: ", wdid, " - ( Approp date: ", appropriation_date, " )"),
  #     labelOptions = leaflet::labelOptions(
  #       noHide = F,
  #       style  = list(
  #         "color" = "black",
  #         "font-weight" = "1000")
  #     )
  #   ) %>% 
  #   leaflet::addPolylines(
  #     # data = sub_flines(),
  #     data = sub_flines,
  #     color = ~pal(gnis_name),
  #     label       = ~paste0("GNIS name: ", gnis_name),
  #     labelOptions = leaflet::labelOptions(
  #       noHide = F,
  #       style  = list(
  #         "color" = "black",
  #         "font-weight" = "1000")
  #     ),
  #     opacity = 0.9,
  #     stroke = TRUE
  #   )
    # leaflet::addPolygons(
    #   data        = shp,
    #   fillColor = 'red',
    #   fillOpacity = 0.7,
    #   col = "red",
    #   opacity = 1,
    #   # weight = 2.5,
    #   weight      = 1,
    #   layerId     = ~map_id,
    #   group       = ~new_id,
    #   label = ~paste0("District: ", new_id),
    #   labelOptions = labelOptions(
    #     style     = list("font-weight" = "normal", padding = "3px 8px"),
    #     textsize  = "15px",
    #     direction = "auto"
    #   )
    #   # label       = ~map_id
    # ) %>%
    # leaflet::addPolygons(
    #   data = shp,
    #   # group = "base_hucs",
    #   fillColor = 'white',
    #   # fillColor = 'grey',
    #   # fillColor = ~pal_fact(BASIN),
    #   fillOpacity = 0.7,
    #   col = "black",
    #   opacity = 1,
    #   weight = 2.5,
    #   label = ~paste0("District: ", new_id),
    #   layerId      = ~new_id,
    #   group        = "regions",
    #   labelOptions = labelOptions(
    #     noHide = F,
    #     # direction = 'center',
    #     # textOnly = F)
    #     style = list(
    #       "color" = "black",
    #       "font-weight" = "1000")
    #   )
    # ) %>% 
    # leaflet::hideGroup(group = shp$new_id)
  
}

# Create a linear regression model from a filtered down dataset provided as a dataframe which is then worked into a linear regression 
# 'model_data' is a dataframe with a 'resp_var', 'resp_val', and 'predictor' column
make_lm <- function(model_data) {
  
  # model_data <- dplyr::filter(df, district == ud)
  
  # create linear regression model
  lm_model <- lm(
    resp_val ~ predictor_val, 
    data = model_data
  )
  
  # create a string of the LM equation
  lm_equation = paste0(model_data$resp_var[1], " ~ ", paste0(unique(model_data$predictor), collapse = "+"))
  
  # summarize model
  lm_summary = summary(lm_model)
  
  # extract coefficients
  lm_coeffs <- janitor::clean_names(lm_summary$coefficients)
  # janitor::clean_names(lm_summary$coefficients[1, ])
  # unname(unlist(lm_summary$coefficients[1, ]))
  # model_data %>% na.omit()
  # ADD fitted data as column to orginal dataframe
  model_data <-
    model_data %>%
    dplyr::mutate(fitted = lm_model$fitted.values)
  
  # return list with fitted model and data
  res <- list(
    model      = lm_model,
    model_data = model_data,
    equation   = lm_equation,
    r2         = round(lm_summary$r.squared, 3),
    coeffs     = lm_coeffs
  )
  
  return(res)
}

# make a list of linear regression models from a dataframe of districts with these columns: 
#   "district", "resp_var", "resp_val", "predictor", and "predictor_val" columns
make_lm_list <- function(df) {
  
  udistricts <- unique(df$district)
  # df <- lm_data
  # df
  lm_list <- lapply(1:length(udistricts), function(i) {
    # i = 1
    # unique district
    ud <- udistricts[i]
    
    # message(i, "/", length(udistricts))
    # message("District: ", ud)
    
    make_lm(dplyr::filter(df, district == ud) )
    
    
  }) %>% 
    stats::setNames(paste0("district_", udistricts))
  
  return(lm_list)
}

# Make a scatter plot of observed vs predicted values from an Linear regression model 
# df is a dataframe with containing a "resp_val" column for the observed data, 
# and a "fitted" column for the fitted values from a linear regression model
make_fitted_plot <- function(df) {
  
  fitted_plot <- 
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = resp_val, y = fitted),
      size = 2
      ) +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = fitted), 
                         method = "lm",
                         se = FALSE
    ) + 
    ggplot2::ylim(c(1800, 2030)) +
    ggplot2::xlim(c(1800, 2030)) +
    # ggplot2::scale_x_continuous(
    #   breaks = seq(1800, 2030, by = 25)
    #   ) +
    # ggplot2::scale_y_continuous(
    #   breaks = seq(1800, 2030, by = 100)
    #   ) +
    ggplot2::labs(x = "Observed", y = "Predicted") + 
    ggplot2::theme_bw() 
  
  return(fitted_plot)
  
}

# Make a scatter plot of PRedictor data vs predicted values from an Linear regression model 
# 'df' is a dataframe with containing columns:
#  "resp_val" column for the observed data, 
#  "fitted" column for the fitted values from a linear regression model,
#  "predictor_val" column for the value of the predictor data, 
#  "predictor" column for the name of the predictor
make_predictor_fit_plot <- function(df) {
  
  # df    = lm_out$model_data
  # df$predictor_val
  # df$fitted
  
  # round(max(df$predictor_val ) + 200)
  pred_fit_plot <- 
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = predictor_val, y = fitted),
      size = 2
      ) +
    # ggplot2::geom_smooth(ggplot2::aes(x = predictor_val, y = fitted), 
    #                      method = "lm",
    #                      se = FALSE
    # ) + 
    ggplot2::ylim(c(1800, 2030)) +
    # ggplot2::xlim(c(1800, 2030))
    # ggplot2::xlim(c(0,  
    #                 round(max(df$predictor_val ) + 200))
    #               ) +
    ggplot2::xlim(c(0, 1000)) +
    ggplot2::labs(x = df$predictor[1], y = "Predicted") + 
    ggplot2::theme_bw() 
  
  return(pred_fit_plot)
  
}
# fitted_plot <- make_fitted_plot(
#   df = lm_out$model_data,
#   new_prediction = pred$fitted[1]  # Pass the new prediction value
# )

# Make a scatter plot of observed vs predicted values from an Linear regression model 
# df is a dataframe with containing a "resp_val" column for the observed data, 
# and a "fitted" column for the fitted values from a linear regression model
# make_new_predfit_plot <- function(df, new_prediction, new_input) {
make_new_predfit_plot <- function(df, pred_df) {
  # df = lm_out$model_data
  # pred_df = pred
  # new_prediction = pred$fitted[1]
  # new_input = pred$predictor_val[1]
  # df
  # pred
  
  # df <- 
  #   df %>% 
  #   dplyr::select(predictor, predictor_val, fitted)
  
  updated_pred_fit <- 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df, 
      ggplot2::aes(x = predictor_val, y = fitted),
      size = 2
      ) +
    ggplot2::geom_point(
      data = pred_df,
      ggplot2::aes(x = predictor_val, y = fitted), 
      color = "red", 
      size = 5
      ) +
    ggplot2::ylim(c(1800, 2030)) +
    ggplot2::xlim(c(0, 1000)) +
    ggplot2::labs(x = df$predictor[1], y = "Predicted") + 
    ggplot2::theme_bw() 
  
  # ggplot2::ggplot(data = df, ggplot2::aes(x = predictor_val, y = fitted)) +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_point(data = pred_df, color = "red", size = 3) +
  #   ggplot2::ylim(c(1800, 2030)) +
  #   ggplot2::xlim(c(0, 1000)) +
  #   ggplot2::labs(x = df$predictor[1], y = "Predicted") + 
  #   ggplot2::theme_bw() 
  
  return(updated_pred_fit)
  
}

# Make a scatter plot of observed vs predicted values from an Linear regression model 
# df is a dataframe with containing a "resp_val" column for the observed data, 
# and a "fitted" column for the fitted values from a linear regression model
make_updated_fitted_plot <- function(df) {
  
  fitted_plot <- 
    df %>% 
    ggplot2::ggplot() +
    # ggplot2::geom_point(ggplot2::aes(x = resp_val, y = fitted, color = pt_type)) +
    ggplot2::geom_point(ggplot2::aes(x = resp_val, y = fitted)) +
    gghighlight::gghighlight(pt_type == "added") +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = fitted), 
                         method = "lm",
                         se = FALSE
    ) + 
    ggplot2::ylim(c(1800, 2030)) +
    ggplot2::xlim(c(1800, 2030)) +
    # ggplot2::scale_x_continuous(
    #   breaks = seq(1800, 2030, by = 25)
    #   ) +
    # ggplot2::scale_y_continuous(
    #   breaks = seq(1800, 2030, by = 100)
    #   ) +
    ggplot2::labs(x = "Observed", y = "Predicted") + 
    ggplot2::theme_bw() 
  
  return(fitted_plot)
  
}

make_prediction <- function(model, predictor, val) {
  
  # put value into a dataframe as 'predictor_val'
  out <- data.frame(
          predictor     = predictor, 
          predictor_val = val
          )
  
  # add prediction as 'fitted' column in out 
  out$fitted <- unname(
    predict.lm(model, newdata = out)
  )
  
  # # add prediction as 'fitted' column in out 
  # out$fitted <- unname(
  #   predict.lm(model, newdata = out)
  # )
  
  return(out)
}

get_us_net <- function(pt, distance) {
  
  # comid_pt <- dataRetrieval::findNLDI(location = pt)
  
  um_net <- nhdplusTools::navigate_network(
    start       = dataRetrieval::findNLDI(location = pt),
    mode        = "UM",
    distance_km = distance
  )
  
  return(um_net)
  
}

# Get Water right Net Amounts points 
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

  # check if no arguments are provided
  if(any(is.null(flowlines), is.null(pts))) {
    
    stop("Invalid or missing 'flowlines' or 'pts' arguments")
    
  }
  
  # reproject flowlines and pts to projected CRS (5070)
  flowlines <- sf::st_transform(flowlines, 5070)
  pts       <- sf::st_transform(pts, 5070)
  
  # min hydrosequence (downstream) flow line on lowest levelpath stream (mainstem)
  min_fline <- flowlines[flowlines$hydroseq == min(flowlines$hydroseq), ]
  
  # max hydrosequence (upstream) flow line on lowest levelpath stream (mainstem)
  max_fline <- flowlines[flowlines$hydroseq == max(flowlines$hydroseq), ]
  
  end_pt   <- nhdplusTools::get_node(min_fline, position = "end")
  start_pt <- nhdplusTools::get_node(max_fline, position = "start")
  
  # mapview::mapview(end_pt) + start_pt + max_fline + min_fline + flowlines + pts
  
  # index of points nearest to min and max hydroseq of mainnstem
  near_min <- sf::st_nearest_feature(end_pt, pts)
  near_max <- sf::st_nearest_feature(start_pt, pts)
  
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
  
  # add upstream/downstream columns
  min_pt$plot_str <- paste0("WDID: ", min_pt$wdid, " - (downstream)")
  max_pt$plot_str <- paste0("WDID: ", max_pt$wdid, " - (upstream)")
  
  # bind the max and min points to single sf object
  end_pts <- rbind(min_pt, max_pt)
  
  # mapview::mapview(end_pt) +  mapview::mapview(min_pt, color = "red") +  mapview::mapview(max_pt, color = "green") + start_pt + max_fline + min_fline + flowlines

  return(end_pts)
  
}

# get_nearest_pts <- function(
#     flowlines = NULL, 
#     pts       = NULL
# ) {
#   # flowlines = fline
#   # pts       = wr_pts
# 
#   # check if no arguments are provided
#   if(any(is.null(flowlines), is.null(pts))) {
#     
#     stop("Invalid or missing 'flowlines' or 'pts' arguments")
#     
#   }
#   
#   # reproject flowlines and pts to projected CRS (5070)
#   flowlines <- sf::st_transform(flowlines, 5070)
#   pts       <- sf::st_transform(pts, 5070)
#   
#   # unique terminal paths
#   uterm_paths <- unique(flowlines$terminalpa)
#   
#   # get sum total of each set of flowlines for each terimal path
#   path_lengths <- lapply(1:length(uterm_paths), function(i) {
#     
#     fl <- flowlines[flowlines$terminalpa == uterm_paths[i], ]
#   
#     data.frame(
#       terminalpa = as.character(uterm_paths[i]),
#       length     = sum(fl$lengthkm, na.rm = T)
#     )
#     
#     })
#   
#   # bind rows
#   path_lengths <- do.call(rbind, path_lengths)
#   
#   # get terminal path ID of longest segments of rivers
#   term_pa <- path_lengths[path_lengths$length == max(path_lengths$length), ]$terminalpa
# 
#   # flow lines associated with terminal paths of more represented stream flowlines
#   fls <- flowlines[flowlines$terminalpa == term_pa, ]
# 
#   # lowest levelpath stream (mainstem)
#   main_stem <- fls[fls$streamleve == min(fls$streamleve), ]
#   
#   # plot(flowlines$geometry)
#   # plot(main_stem$geometry, col = "red", add = T)
# 
#   # min hydrosequence (downstream) flow line on lowest levelpath stream (mainstem)
#   min_fline <- main_stem[main_stem$hydroseq == min(main_stem$hydroseq), ]
#   
#   # max hydrosequence (upstream) flow line on lowest levelpath stream (mainstem)
#   max_fline <- main_stem[main_stem$hydroseq == max(main_stem$hydroseq), ]
#   
#   # index of points nearest to min and max hydroseq of mainnstem
#   near_min <- sf::st_nearest_feature(min_fline, pts)
#   near_max <- sf::st_nearest_feature(max_fline, pts)
#   
#   # nearest WDIDs to most downstream points of river segments
#   min_pt  <- sf::st_transform(pts[near_min, ], 4326)
#   max_pt  <- sf::st_transform(pts[near_max, ], 4326)
#   
#   # add upstream/downstream columns
#   min_pt$position <- "downstream"
#   max_pt$position <- "upstream"
#   
#   # add comid columns
#   min_pt$comid <- min_fline$comid
#   max_pt$comid <- max_fline$comid
#   
#   # add hydroseq columns
#   min_pt$hydroseq <- min_fline$hydroseq
#   max_pt$hydroseq <- max_fline$hydroseq
#   
#   # add stream level columns
#   min_pt$stream_lvl <- min_fline$streamleve
#   max_pt$stream_lvl <- max_fline$streamleve
#   
#   # add upstream/downstream columns
#   min_pt$plot_str <- paste0("WDID: ", min_pt$wdid, " - (downstream)")
#   max_pt$plot_str <- paste0("WDID: ", max_pt$wdid, " - (upstream)")
#   
#   # bind the max and min points to single sf object
#   end_pts <- rbind(min_pt, max_pt)
#   
#   return(end_pts)
#   
# }

get_main_stem <- function(
    flowlines = NULL
) {
  # flowlines = fline
  # pts       = wr_pts
  
  # check if no arguments are provided
  if(any(is.null(flowlines))) {
    
    stop("Invalid or missing 'flowlines' arguments")
    
  }
  
  # reproject flowlines and pts to projected CRS (5070)
  flowlines <- sf::st_transform(flowlines, 5070)
  
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
  # min_fline <- main_stem[main_stem$hydroseq == min(main_stem$hydroseq), ]
  
  return(main_stem)
  
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
      calls$plot_str       <- df$plot_str[i]
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
  
  # df <- 
  #   df %>% 
  #   dplyr::mutate(
  #     priority_date = dplyr::case_when(
  #       is.na(priority_date) ~ max(priority_date),
  #       TRUE                 ~ priority_date
  #     )
  #   )
  # 
  df <- 
    df %>% 
    dplyr::mutate(
      priority_date = dplyr::case_when(
        is.na(priority_date) ~ max(priority_date),
        TRUE                 ~ priority_date
      )
    )
  admin_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(
      # ggplot2::aes(x = datetime, y = priority_date, color = analysis_wdid),
      ggplot2::aes(x = datetime, y = priority_date),
      color = "red"
      ) +
    ggplot2::labs(
      x     = "Date",
      y     = "Priority Date"
      # color = "WDID"
      ) +
    ggplot2::facet_wrap(~plot_str, nrow = 2)+ 
    ggplot2::theme_bw()
  
  return(admin_plot)
  
  
}

make_calls_plot_year <- function(df) {
  
  # # replace NA dates with the max priority date on record
  # df$priority_date <- ifelse(is.na(df$priority_date), max(df$priority_date, na.rm = T), df$priority_date)
  # 
  # # convert dates to date type
  # df$priority_date <- as.Date(df$priority_date)
  # df <- 
  #   df %>% 
  #   dplyr::mutate(
  #     priority_date = dplyr::case_when(
  #       is.na(priority_date) ~ max(priority_date),
  #       TRUE                 ~ priority_date
  #     )
  #   )
  # 
  # df <- 
  #   df %>% 
  #   dplyr::mutate(
  #     priority_date = dplyr::case_when(
  #       is.na(priority_date) ~ max(priority_date),
  #       TRUE                 ~ priority_date
  #     )
  #   )
  admin_plot <-
    df %>% 
    dplyr::mutate(
        day   = lubridate::yday(datetime),
        year  = as.character(year),
        month = as.character(month)
        ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(
      # ggplot2::aes(x = datetime, y = priority_date, color = analysis_wdid),
      ggplot2::aes(x = day, y = priority_date, color = year)
      # color = "red"
    ) +
    ggplot2::labs(
      title = "Priority date vs. time",
      subtitle = "Call analysis done uses the most upstream WDID on each HUC4's main river segment\nCall analysis uses the most junior right to show the downstream priority date that called out all rights junior to it",
      x     = "Date",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::facet_wrap(~year)+
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title  = ggplot2::element_text(size = 14),
      legend.text   = ggplot2::element_text(size = 14),
      axis.title     = ggplot2::element_text(size = 16, face = "bold"),
      axis.text     = ggplot2::element_text(size = 14)
    )
  
  return(admin_plot)
  
  
}

make_highlight_calls_plot <- function(df, years) {
  
  # df <- call_df[call_df$wdid == end_pts[end_pts$huc4 == "1401",]$wdid, ]
  # df = calls
  # add day of year number and year columns
  df <- 
    df %>% 
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      # year  = as.character(lubridate::year(datetime))
      year  = as.character(year)
    ) 
  
  # add month label name per day number
  df$month <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::days(df$day - 1), label = TRUE)
  
  # df <- 
  #   df %>% 
  #   dplyr::mutate(
  #     priority_date = dplyr::case_when(
  #       is.na(priority_date) ~ Sys.Date(),
  #       TRUE                 ~ as.Date(priority_date)
  #     )
  #   )
  
  admin_highlight_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = day, y = priority_date, color = factor(year)),
                       alpha = 0.9, size = 2.5) +
      ggplot2::scale_x_continuous(limits = c(1, 365)) +
      ggplot2::scale_x_continuous(
        breaks = c(1, 59, 120, 181, 242, 303, 365),
        labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")) +
    gghighlight::gghighlight(year %in% c(years), 
                             unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = "Priority date vs. time",
      # subtitle = "Colorado River Basin",
      x     = "Month",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  admin_highlight_plot
  
  # ggplot2::ggsave(
  #   filename = "img/rightograph_colorado.png",
  #   plot = admin_highlight_plot,
  #   height = 10,
  #   width = 12,
  #   dpi = 300,
  #   scale = 1
  # )
  return(admin_highlight_plot)
  
  
}

make_single_call_plot <- function(df,min_line, years) {
  # df    = calls
  # min_line = min_date
  # years = 2020
  # 
  
  df <- 
    df %>% 
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      year  = as.character(lubridate::year(datetime))
      # week  = lubridate::week(datetime)
    ) 
  
  
  df$hline <- min_line

  # add month label name per day number
  df$month <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::days(df$day - 1), label = TRUE)
  
  admin_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = day, y = priority_date, color = factor(year)),
                       alpha = 0.9, size = 2.5) +
    ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(limits = c(1, 365)) +
    ggplot2::scale_x_continuous(
      breaks = c(1, 59, 120, 181, 242, 303, 365),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")) +
    gghighlight::gghighlight(year %in% c(years), 
                             unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = "Priority date vs. time",
      # subtitle = "Colorado River Basin",
      x     = "Month",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 

  return(admin_plot)
}
# stream_mile = "162.81"
# gnis_id <- "201759"
# wdid = "0100501"
# admin_number <- "20226.00000"
# 
# gnis_calls <- cdssr::get_call_analysis_gnisid(
#   gnis_id = gnis_id,
#   # wdid = wdid,
#   admin_no= admin_number,
#   stream_mile = stream_mile,
#   start_date = Sys.Date() - 365*years,
#   end_date   = Sys.Date()
# )
# calls <- cdssr::get_call_analysis_wdid(
#   wdid       = input$selectWDID,
#   # wdid = "0200509",
#   admin_no   = "99999.00000",
#   # start_date = "2018-01-01",
#   # end_date   = "2019-12-31"
#   start_date = Sys.Date() - 365*years,
#   end_date   = Sys.Date()
# ) %>%
#   dplyr::mutate(
#     priority_date = dplyr::case_when(
#       is.na(priority_date) ~ Sys.Date(),
#       TRUE                 ~ as.Date(priority_date)
#       # is.na(priority_date) ~ as.Date("2019-12-31"),
#       # TRUE                 ~ as.Date(priority_date)
#     )
#   )

aggreg_by_year_type <- function(df) {
  
  df <-
    df %>% 
    dplyr::group_by(district, year_type, week) %>% 
    dplyr::summarise(priority_date = mean(priority_date, na.rm = T)) %>% 
    dplyr::ungroup()
  
  return(df)
  
}
make_yeartype_rightograph_plot <- function(df, district, yeartype) {
  # yeartype = "average"
  # dist = 1
  
  df <- 
    weekly_calls %>% 
    dplyr::filter(
      district == dist,
      year_type == yeartype
      )
  
  
  district_lab = unique(df$district)
  
  # rightograph <- 
  df %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, 
                                    y = priority_date,
                                    color = factor(year)),
                       alpha = 0.7,
                       size = 2.5
    ) 
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    gghighlight::gghighlight(year %in% c(years),
                             unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = paste0("Right-o-graph (WDID: ", wdid_lab, ")"),
      subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
  
}

make_avg_yeartype_rightograph_plot <- function(df, type) {
  
  # df <- weekly_calls
  # yeartype = "average"
  # dist = 1
  # avg_yeartype
  # df <- 
  #   # weekly_calls %>% 
  #   avg_yeartype %>% 
  #   dplyr::filter(
  #     district == dist
  #   )
  # type = "dry"
  district_lab = unique(df$district)
  
  rightograph <-
    df %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, 
                                    y = priority_date,
                                    color = factor(year_type)),
                       alpha = 0.7,
                       size = 2.5
                       ) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    gghighlight::gghighlight(
      year_type %in% c(type),
      unhighlighted_params = list(size = 1)
      ) +
    ggplot2::scale_color_manual(values = c("Wet" = "dodgerblue", "Average" = "black", "Dry" = "darkred"), 
                                guide = ggplot2::guide_legend(
                                direction = "horizontal",
                                title.position = "top"
    )) +
    ggplot2::labs(
      title = paste0("Right-o-graph (District: ", district_lab, ")"),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      legend.position   = "bottom",
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
  
}
# 
# make_avg_yeartype_rightograph_all_districts_plot <- function(df, theme_obj = NULL) {
#   # df <- avg_yeartype
#   
#   # df <- weekly_calls
#   # yeartype = "average"
#   # dist = 1
#   # avg_yeartype
#   # df <- 
#   #   # weekly_calls %>% 
#   #   avg_yeartype %>% 
#   #   dplyr::filter(
#   #     district == dist
#   #   )
#   # type = "dry"
#   # district_lab = unique(df$district)
#   
#   rightograph <-
#     df %>% 
#     # dplyr::filter(district %in% c("2","1")) %>%
#       dplyr::mutate(
#         district = paste0("District: ", district)
#       ) %>% 
#     ggplot2::ggplot() +
#     ggplot2::geom_line(ggplot2::aes(x = week, 
#                                     y = priority_date,
#                                     color = factor(year_type)),
#                        alpha = 0.7,
#                        size = 2.5
#     ) +
#     # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
#     ggplot2::scale_x_continuous(
#       limits = c(1, 52), 
#       breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
#       labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
#     ) +
#     ggplot2::facet_wrap(~district, nrow = 4) +
#     # gghighlight::gghighlight(
#     #   year_type %in% c(type),
#     #   unhighlighted_params = list(size = 1)
#     # ) +
#     ggplot2::scale_color_manual(values = c("Wet" = "dodgerblue", "Average" = "black", "Dry" = "darkred") 
#                                 # guide = ggplot2::guide_legend(
#                                 #   direction = "horizontal",
#                                 #   title.position = "top"
#                                 # )
#                                 ) +
#     ggplot2::labs(
#       # title = paste0("Average call date by year type"),
#       title = "",
#       # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
#       # caption = "Black horizontal line represents average % out of priority over the period of record",
#       x     = "",
#       y     = "Priority Date",
#       color = "Year type"
#     ) 
#   
#   if(is.null(theme_obj)) {
#     rightograph <- 
#       rightograph +
#       ggplot2::theme_bw() +
#       ggplot2::theme(
#         plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
#         plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
#         legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
#         legend.text       = ggplot2::element_text(size = 16),
#         # legend.position   = "bottom",
#         axis.title        = ggplot2::element_text(size = 16, face = "bold"),
#         axis.text         = ggplot2::element_text(size = 16),
#         legend.key.width  = unit(1.5, "cm"),
#         legend.text.align = 0,
#         legend.key.height = unit(1, "cm")
#       ) 
#     
#   } else {
#     
#     rightograph <- rightograph + theme_obj
#     
#   }
# 
#   return(rightograph)
# 
# }

make_weekly_rightograph_plot <- function(df, min_line, years) {
  
  wdid_lab = unique(df$wdid)
  
  rightograph <- 
    df %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, y = priority_date, color = factor(year)),
                       alpha = 0.7, size = 2.5) +
    ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
      ) +
    gghighlight::gghighlight(year %in% c(years),
                                 unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = paste0("Right-o-graph (WDID: ", wdid_lab, ")"),
      subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
 
}

make_wdid_year_rightograph_plot <- function(df, wdid, highlight_year) {
  
  # wdid_lab = unique(df$wdid)
  
  rightograph <- 
    df %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, 
                                    y = priority_date, color = factor(year)),
                       alpha = 0.7, size = 2.5) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    gghighlight::gghighlight(year %in% c(highlight_year),
                             unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = paste0("Right-o-graph (WDID: ", wdid, ")"),
      subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
  
}


make_wdid_call_year_grid_plot <- function(df, wdid_id, highlight_year) {
  # df <- wdid_calls 
  # 
  # wdid_calls$analysis_wdid %>% unique()
  # wdid_id <- "0801020"
  # highlight_year = c(1974, 1990, 2002, 2019)
  
  highlight_year <- sort(highlight_year)
  
  df_sub <- 
    df %>% 
    # dplyr::filter(analysis_wdid == wdid_id)
    dplyr::filter(analysis_wdid == wdid_id, year %in% highlight_year)

  df_sub %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, 
                                    y = priority_date, color = factor(year)),
                       alpha = 0.7, size = 2.5) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    ggplot2::facet_wrap(~year, nrow = 1)
    # gghighlight::gghighlight(year %in% c(highlight_year[i]),
    #                          unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      # title = paste0("District: ", df$district[1]),
      # subtitle = paste0("WDID: ", wdid),
      # title = paste0("Right-o-graph (WDID: ", wdid, ")"),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  for (i in 1:length(highlight_year)) {
    # i = 1
    # rightograph <- 
    # df %>% 
      df_sub %>% 
      # dplyr::filter(year == 2022) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = week, 
                                      y = priority_date, color = factor(year)),
                         alpha = 0.7, size = 2.5) +
      # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
      ggplot2::scale_x_continuous(
        limits = c(1, 52), 
        breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
        labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
      ) +
      gghighlight::gghighlight(year %in% c(highlight_year[i]),
                               unhighlighted_params = list(size = 1)) +
      ggplot2::labs(
        # title = paste0("District: ", df$district[1]),
        # subtitle = paste0("WDID: ", wdid),
        # title = paste0("Right-o-graph (WDID: ", wdid, ")"),
        # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
        # caption = "Black horizontal line represents average % out of priority over the period of record",
        x     = "",
        y     = "Priority Date",
        color = "Year"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
        legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
        legend.text       = ggplot2::element_text(size = 16),
        axis.title        = ggplot2::element_text(size = 16, face = "bold"),
        axis.text         = ggplot2::element_text(size = 16),
        legend.key.width  = unit(1.5, "cm"),
        legend.text.align = 0,
        legend.key.height = unit(1, "cm")
      ) 
    
  }
  # # rightograph <- 
  #   df %>% 
  #   # dplyr::filter(year == 2022) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(ggplot2::aes(x = week, 
  #                                   y = priority_date, color = factor(year)),
  #                      alpha = 0.7, size = 2.5) +
  #   # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
  #   ggplot2::scale_x_continuous(
  #     limits = c(1, 52), 
  #     breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
  #     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
  #   ) +
  #   gghighlight::gghighlight(year %in% c(highlight_year),
  #                            unhighlighted_params = list(size = 1)) +
  #   ggplot2::labs(
  #     title = paste0("District: ", df$district[1]),
  #     subtitle = paste0("WDID: ", wdid),
  #     # title = paste0("Right-o-graph (WDID: ", wdid, ")"),
  #     # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
  #     # caption = "Black horizontal line represents average % out of priority over the period of record",
  #     x     = "",
  #     y     = "Priority Date",
  #     color = "Year"
  #   ) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(
  #     plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
  #     plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
  #     legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
  #     legend.text       = ggplot2::element_text(size = 16),
  #     axis.title        = ggplot2::element_text(size = 16, face = "bold"),
  #     axis.text         = ggplot2::element_text(size = 16),
  #     legend.key.width  = unit(1.5, "cm"),
  #     legend.text.align = 0,
  #     legend.key.height = unit(1, "cm")
  #   ) 
  
  return(rightograph)
  
  
}
make_wdid_rightograph_per_year_plot <- function(df, wdid, highlight_year) {
  
  # # # wdid_lab = unique(df$wdid)
  # df <- call_subset
  #   # call_subset %>% 
  #   # dplyr::filter(year == 2002)
  # highlight_year = as.character(2005)
  # 
  # wdid = wdid_id
  
  # df %>% 
  #   dplyr::mutate(
  #     lubridate::wee
  #   )
  # df %>% 
  #    dplyr::group_by(week) %>% 
  #   dplyr::summarise(
  #     priority_date = mean(priority_date)
  #   ) %>% 
  # rightograph <- 
  # df %>% 
  # dplyr::filter(year == 2022) %>%

  df %>% 
    dplyr::filter(year == highlight_year) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = week,
      # x = datetime,
      y = priority_date),
      alpha = 0.7, size = 2.5) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    # gghighlight::gghighlight(year %in% c(highlight_year),
    #                          unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      # title = paste0("Right-o-graph (WDID: ", wdid, ")"),
      title = paste0("District: ", df$district[1]),
      subtitle = paste0("WDID: ", wdid),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      # x     = "Week of the year",
      x = "",
      y     = "Priority Date",
      # color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18,
                                                face = "bold",
                                                # hjust = 0.5
      ),
      plot.subtitle     = ggplot2::element_text(size = 14
                                                # hjust = 0.5
      ),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
  
}

make_wdid_rightograph_week_avg_plot <- function(df, wdid) {
  
  # # wdid_lab = unique(df$wdid)
  # df <- call_subset
  # wdid = wdid_id
  
  # df %>% 
  #   dplyr::mutate(
  #     lubridate::wee
  #   )
  # df %>% 
  #    dplyr::group_by(week) %>% 
  #   dplyr::summarise(
  #     priority_date = mean(priority_date)
  #   ) %>% 
  # rightograph <- 
    # df %>% 
    # dplyr::filter(year == 2022) %>%
  
    df %>% 
      dplyr::group_by(week) %>%
      dplyr::summarise(
        priority_date = mean(priority_date)
      ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = week,
      # x = datetime,
                                    y = priority_date),
                       alpha = 0.7, size = 2.5) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    # gghighlight::gghighlight(year %in% c(highlight_year),
    #                          unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      # title = paste0("Right-o-graph (WDID: ", wdid, ")"),
      title = paste0("District: ", df$district[1]),
      subtitle = paste0("WDID: ", wdid),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      # x     = "Week of the year",
      x = "", 
      y     = "Priority Date",
      # color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18,
                                                face = "bold",
                                                # hjust = 0.5
                                                ),
      plot.subtitle     = ggplot2::element_text(size = 14
                                                # hjust = 0.5
                                                ),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(rightograph)
  
  
}

aggreg_weekly_wdid <- function(df, omit_na = TRUE) {
  # df <- raw
  # df %>% 
  #   dplyr::filter(analysis_wdid == wdid_id) %>% 
  #   dplyr::mutate(
  #     day   = lubridate::yday(datetime),
  #     year  = as.character(lubridate::year(datetime)), 
  #     week  = lubridate::week(datetime),
  #     week_date = lubridate::weeks(datetime )
  #   ) %>% 
  #   dplyr::relocate(week)
  
  df <-
    df %>%
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      year  = as.character(lubridate::year(datetime)), 
      week  = lubridate::week(datetime),
      week_date = lubridate::weeks(datetime )
    )
  
  week_starts <- 
    df %>%
    dplyr::group_by(year, week, analysis_wdid) %>% 
    # dplyr::select(datetime, year, week, wdid = analysis_wdid) %>% 
    dplyr::select(datetime, year, week, analysis_wdid) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # calls$analysis_wdid
  df <-
    df %>%
    # dplyr::mutate(
    #   day   = lubridate::yday(datetime),
    #   year  = as.character(lubridate::year(datetime)), 
    #   week  = lubridate::week(datetime),
    #   week_date = lubridate::weeks(datetime )
    # ) %>% 
    dplyr::group_by(year, week, analysis_wdid) %>% 
    dplyr::summarise(
      
      out_pct       = mean(analysis_out_of_priority_percent_of_day, na.rm = T)/100,
      priority_date = mean(priority_date, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      district = substr(analysis_wdid, 0, 2)
    ) 
    # dplyr::rename(wdid = analysis_wdid)
  
  df <- 
    df %>% 
    dplyr::left_join(
      week_starts,
      by = c("year", "week", "analysis_wdid")
    ) %>% 
    dplyr::relocate(district, year, week, datetime)
  
  # dplyr::mutate(
  #   year_type = dplyr::case_when(
  #     year >= 1970 & year < 1985 ~ "dry",
  #     year >= 1985 & year < 2000 ~ "average",
  #     year >= 2000 ~ "wet"
  #   )
  # )
  # %>% 
  #   dplyr::rename(wdid = analysis_wdid)
  
  df$week_lab <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::weeks(df$week - 1), label = TRUE)
  
  
  if(omit_na) {
    df <- na.omit(df)
  }
  
  return(df)
}

# aggreg_weekly_wdid <- function(df, omit_na = TRUE) {
#   
#   # calls$analysis_wdid
#   df <-
#     df %>%
#     dplyr::mutate(
#       day   = lubridate::yday(datetime),
#       year  = as.character(lubridate::year(datetime)), 
#       week  = lubridate::week(datetime),
#       week_date = lubridate::weeks(datetime )
#     ) %>% 
#     dplyr::group_by(year, week, analysis_wdid) %>% 
#     dplyr::summarise(
#       out_pct       = mean(analysis_out_of_priority_percent_of_day, na.rm = T)/100,
#       priority_date = mean(priority_date, na.rm = T)
#     ) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::mutate(
#       district = substr(analysis_wdid, 0, 2)
#     ) %>% 
#     dplyr::rename(wdid = analysis_wdid)
#     # dplyr::mutate(
#     #   year_type = dplyr::case_when(
#     #     year >= 1970 & year < 1985 ~ "dry",
#     #     year >= 1985 & year < 2000 ~ "average",
#     #     year >= 2000 ~ "wet"
#     #   )
#     # )
#   # %>% 
#   #   dplyr::rename(wdid = analysis_wdid)
#   
#   df$week_lab <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::weeks(df$week - 1), label = TRUE)
#   
#   
#   if(omit_na) {
#     df <- na.omit(df)
#   }
#   
#   return(df)
# }

aggreg_weekly_district <- function(df, omit_na = TRUE) {
  
  # calls$analysis_wdid
  df <-
    df %>%
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      year  = as.character(lubridate::year(datetime)), 
      week  = lubridate::week(datetime),
      week_date = lubridate::weeks(datetime )
    ) %>% 
    dplyr::group_by(year, week, district) %>% 
    dplyr::summarise(
      out_pct       = mean(analysis_out_of_priority_percent_of_day, na.rm = T)/100,
      priority_date = mean(priority_date, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      year_type = dplyr::case_when(
        year >= 1970 & year < 1985 ~ "dry",
        year >= 1985 & year < 2000 ~ "average",
        year >= 2000 ~ "wet"
      )
    )
  # %>% 
  #   dplyr::rename(wdid = analysis_wdid)
  
  df$week_lab <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::weeks(df$week - 1), label = TRUE)
  
  
  if(omit_na) {
    df <- na.omit(df)
  }
  
  return(df)
}
aggreg_weekly2 <- function(df) {
  
  # calls$analysis_wdid
  df <- 
    df %>% 
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      year  = as.character(lubridate::year(datetime)), 
      week  = lubridate::week(datetime),
      week_date = lubridate::weeks(datetime )
    ) %>% 
    dplyr::group_by(year, week, analysis_wdid) %>% 
    dplyr::summarise(
      out_pct       = mean(out_pct, na.rm = T)/100,
      priority_date = mean(priority_date, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(wdid = analysis_wdid)
  
  df$week_lab <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::weeks(df$week - 1), label = TRUE)
  
  return(df)
}
aggreg_weekly <- function(df) {

  # calls$analysis_wdid
  df <- 
    df %>% 
    dplyr::mutate(
      day   = lubridate::yday(datetime),
      year  = as.character(lubridate::year(datetime)), 
      week  = lubridate::week(datetime),
      week_date = lubridate::weeks(datetime )
    ) %>% 
    dplyr::group_by(year, week, analysis_wdid) %>% 
    dplyr::summarise(
      out_pct       = mean(analysis_out_of_priority_percent_of_day, na.rm = T)/100,
      priority_date = mean(priority_date, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(wdid = analysis_wdid)
  
  df$week_lab <- lubridate::month(lubridate::ymd(paste0(df$year, "-01-01")) + lubridate::weeks(df$week - 1), label = TRUE)
  
  return(df)
}
# library(terra)
# tt <-   raster::raster(terra::rast("C:/Users/angus/Downloads/2000_01.tif"))
# plot(tt)
# aggreg_weekly(df = calls)

make_weekly_out_pct_plot <- function(df, years) {
  
  # df    = aggreg_weekly(df = calls)
  # min_line = min_date
  # years = 2022
  
  
  # rightograph <- 
  out_pct_plot <-
    df %>% 
    # dplyr::filter(year == 2022) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, y = out_pct, color = factor(year)),
                       alpha = 0.7, size = 2.5) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = out_pct), yintercept = mean(df$out_pct), size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    gghighlight::gghighlight(year %in% c(years),
                             unhighlighted_params = list(size = 1)) +
    ggplot2::labs(
      title = "Average time spent out of priority",
      subtitle = "Weekly average",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "% out of priority",
      color = "Year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      legend.text       = ggplot2::element_text(size = 16),
      axis.title        = ggplot2::element_text(size = 16, face = "bold"),
      axis.text         = ggplot2::element_text(size = 16),
      legend.key.width  = unit(1.5, "cm"),
      legend.text.align = 0,
      legend.key.height = unit(1, "cm")
    ) 
  
  return(out_pct_plot)
  
}

# tmp_huc = "1019"
# # select mainstem of HUC4 area
# fline <-
#   main_stem %>%
#   dplyr::filter(huc4 == tmp_huc)
# 
# # buffer bounds of mainstem for map fly to zoom
# bounds <-
#   # buff %>%
#   fline %>%
#   # sf::st_buffer(5250) %>%
#   sf::st_bbox() %>%
#   as.vector()
# 
# print("Buffering mainstem...")
# 
# # buffer around mainstem to get points within
# fline_buff <- sf::st_as_sf(
#   sf::st_cast(
#     sf::st_union(
#       sf::st_buffer(
#         sf::st_transform(
#           fline,
#           5070
#         ),
#         1609.3
#       )
#     ),
#     "POLYGON"
#   )
# )
# 
# print("Subsetting WDIDs to mainstem buffer...")
# 
# # Water rights points around main flowline
# fline_pts <- sf::st_transform(
#   sf::st_filter(
#     sf::st_transform(
#       dplyr::filter(wr_pts, huc4 == tmp_huc),
#       5070
#     ) ,
#     fline_buff
#   ),
#   4326
# )
# 
# # subset to HUC4 of interest
# sub_huc <- hucs[hucs$huc4 == tmp_huc, ]

make_date_map <- function(lines, pts) {
  # lines <- main_stem
  # pts <- wr_pts
  # lines = fline
  # pts   = fline_pts
  
  bin_wr <- 
    pts %>%
    dplyr::group_by(wdid) %>%
    dplyr::mutate(
      appropriation_date = as.Date(appropriation_date)
    ) %>% 
    dplyr::slice(which.min(appropriation_date)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      bin_date = dplyr::case_when(
        appropriation_date <= "1850-01-01"                                      ~ "< 1850",
        appropriation_date >  "1850-01-01" & appropriation_date <= "1900-01-01" ~ "1850 - 1900",
        appropriation_date >  "1900-01-01" & appropriation_date <= "1950-01-01" ~ "1900- 1950",
        appropriation_date >  "1950-01-01" & appropriation_date <= "2000-01-01" ~ "1950 - 2000",
        appropriation_date >  "2000-01-01"                                      ~ "2000 - present"
      )
    )  
  
    date_plot <- 
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = lines) +
      ggplot2::geom_sf(
        data  = bin_wr,
        ggplot2::aes(color = bin_date), 
        alpha = 0.7,
        size  = 3
        ) +
      ggplot2::labs(
        title = "Binned Appropriation Dates",
        color = "Appropriation dates"
        ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 16,face = "bold", hjust = 0.5),
        legend.title = ggplot2::element_text(size = 14),
        legend.text  = ggplot2::element_text(size = 14),
        axis.title   = ggplot2::element_text(size = 16, face = "bold"),
        axis.text    = ggplot2::element_text(size = 14)
        )
    
    # date_plot <- 
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(data = dplyr::filter(hucs, huc4 == tmp_huc)) +
    #   ggplot2::geom_sf(data = lines) +
    #   ggplot2::geom_sf(
    #     data  = bin_wr,
    #     # ggplot2::aes(color = bin_date), 
    #     alpha = 0.5,
    #     size  = 2.5
    #   ) +
    #   # ggplot2::geom_sf(data = dplyr::filter(hucs, huc4 == tmp_huc)) +
    #   ggplot2::labs(
    #     # title = "Binned Appropriation Dates",
    #     title = "South Platte River Basin WDIDs",
    #     color = "Appropriation dates"
    #   ) +
    #   ggplot2::theme_bw() +
    #   ggplot2::theme(
    #     plot.title   = ggplot2::element_text(size = 16,face = "bold", hjust = 0.5),
    #     legend.title = ggplot2::element_text(size = 14),
    #     legend.text  = ggplot2::element_text(size = 14),
    #     axis.title   = ggplot2::element_text(size = 16, face = "bold"),
    #     axis.text    = ggplot2::element_text(size = 14)
    #   )
    # 
    # date_plot
    # 
    # ggplot2::ggsave(
    #   filename = "img/southplatte_wdid_map.png",
    #   plot = date_plot,
    #   height = 10,
    #   width = 12,
    #   dpi = 300,
    #   scale = 1
    # )
    
    return(date_plot)
}

# aq <- readRDS("C:/Users/angus/Downloads/epa_aqi_all.RDS")
# 
# tmp <- 
#   aq %>% 
#   tidyr::pivot_longer(cols = c(-year, -state, -county)) %>% 
#   tidyr::separate(name, into = c("month", "aqi")) %>% 
#   dplyr::mutate(
#     month     = toupper(month),
#     month_num = match(month, toupper(month.abb)),
#     month_num = ifelse(month_num > 9, month_num, paste0("0", month_num)),
#     date      = as.Date(paste0(year, "-", month_num, "-01"))
#   ) %>% 
#   dplyr::filter(state == "colorado")

# aqi_plot <- 
#   tmp %>% 
#   dplyr::group_by(date) %>% 
#   dplyr::summarise(aq_value = mean(value, na.rm = T)) %>% 
#   dplyr::ungroup() %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = date, y = aq_value), size = 1) + 
#   ggplot2::labs(
#     title = "Colorado EPA Air Quality Index",
#     subtitle = "Statewide mean AQI",
#     x     = "",
#     y     = "AQI",
#     color = "County"
#   ) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
#     plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
#     legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
#     legend.text       = ggplot2::element_text(size = 16),
#     axis.title        = ggplot2::element_text(size = 16, face = "bold"),
#     axis.text         = ggplot2::element_text(size = 16),
#     # legend.key.width  = unit(1.5, "cm"),
#     # legend.text.align = 0,
#     # legend.key.height = unit(1, "cm")
#   ) 
# 
# ggplot2::ggsave(
#   filename = "img/aqi_plot.png",
#   plot = aqi_plot,
#   height = 10,
#   width = 12,
#   dpi = 300,
#   scale = 1
# )
# 
# county_aqi <- 
#   tmp %>% 
#   dplyr::filter(county %in% c("boulder", "denver", "larimer", "adams")) %>% 
#   dplyr::mutate(county = tools::toTitleCase(county)) %>% 
#   # dplyr::group_by(date) %>% 
#   # dplyr::summarise(aq_value = mean(value, na.rm = T)) %>% 
#   # dplyr::ungroup() %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = date, y = value, color = county), 
#                      size = 1) + 
#   ggplot2::facet_wrap(~county) + 
#   ggplot2::labs(
#     title = "Colorado EPA Air Quality Index",
#     # subtitle = "Colorado River Basin",
#     x     = "",
#     y     = "AQI",
#     color = "County"
#   ) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
#     plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
#     legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
#     legend.text       = ggplot2::element_text(size = 16),
#     axis.title        = ggplot2::element_text(size = 16, face = "bold"),
#     axis.text         = ggplot2::element_text(size = 16),
#     strip.text = ggplot2::element_text(size=16)
#     # legend.key.width  = unit(1.5, "cm"),
#     # legend.text.align = 0,
#     # legend.key.height = unit(1, "cm")
#   ) 
# 
# ggplot2::ggsave(
#   filename = "img/county_aqi_plot.png",
#   plot = county_aqi,
#   height = 10,
#   width = 12,
#   dpi = 300,
#   scale = 1
# )
# ***********************************
# ---- convert admin no to dates ----
# ***********************************

admins_to_date <- function(
    admin_no
) {
  
  # admin_no <- dplyr::enquo(admin_no)
  # tmp <- unname(sapply(admin_no, admins_to_date)
  # unname(tmp)
  # length(admin_no)
  # admin_no <- "26302.20226"
  
  
  # if NA is given return NA
  if(is.na(admin_no) | admin_no == "NA") {
    return(NA)
  }
  
  # if NULL is given, return NULL
  if(is.null(admin_no) | admin_no == "NULL") {
    return(NULL)
  }
  
  # if date is lowest possible admin number
  if(admin_no == "0.00000") {
    
    admin_date <- "0001-01-01"
    
    return(admin_date)
    
  }
  
  # most senior water right date
  most_senior_date <- as.Date("1849-12-31")
  
  # if admin number has "00000" digits after period
  if(unlist(strsplit(admin_no, "[.]"))[2] == "00000") {
    
    # admin values left of period
    aleft  <- as.numeric(unlist(strsplit(admin_no, "[.]"))[1])
    
    # admin date == appropriation date
    admin_date <- as.character(most_senior_date + aleft)
    
    # if left side of admin number is NOT "00000"
  } else {
    
    # split admin number to the left and right of period
    aleft  <- as.numeric(unlist(strsplit(admin_no, "[.]"))[1])
    aright <- as.numeric(unlist(strsplit(admin_no, "[.]"))[2])
    
    # prior adjudication date
    prior_adjx  <- as.character(most_senior_date + aleft)
    
    # appropriation date
    appropx <- as.character(most_senior_date + aright)
    
    # admin_date <- prior_adjx
    
    # if prior adjudication date is AFTER appropriation date, than admin date is appropriation date
    if(appropx > prior_adjx) {
      
      admin_date <- appropx
      
      # if prior adjudication date is BEFORE appropriation date, than admin date is prior adjudication date
    } else {
      
      admin_date <- prior_adjx
      
    }
    # # if prior adjudication date is AFTER appropriation date, than admin date is appropriation date
    # if(prior_adjx > appropx) {
    #
    #   admin_date <- appropx
    #
    # # if prior adjudication date is BEFORE appropriation date, than admin date is prior adjudication date
    # } else {
    #
    #   admin_date <- prior_adjx
    #
    # }
    
  }
  
  return(admin_date)
  
}











