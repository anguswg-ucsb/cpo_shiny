
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

make_observed_plot <- function(df,
                               yaxis = NULL
                               ) {
  # tmp <- 
  #   mod_df %>% 
  #   dplyr::filter(district == "01")
  # df <-  tmp
  
  # yaxis = NULL
  # if(is.null(yaxis)) {
  #   yaxis <- df$predictor_long_name[1]
  # }
  yaxis <- df$predictor_long_name[1]
  
  
  observed_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = resp_val, y = predictor_val),
      size = 2
    ) +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = predictor_val), 
                         method = "lm",
                         se = FALSE
    ) + 
    ggplot2::ylim(c(0, 1000)) +
    ggplot2::xlim(c(1800, 2030)) +
    # ggplot2::scale_x_continuous(
    #   breaks = seq(1800, 2030, by = 25)
    #   ) +
    # ggplot2::scale_y_continuous(
    #   breaks = seq(1800, 2030, by = 100)
    #   ) +
    ggplot2::labs(
      x = "Observed average call year",
      y = paste0("Observed ", yaxis)
    ) + 
    ggplot2::theme_bw() 
  
  return(observed_plot)
  
  
}

make_new_observed_plot <- function(
                              df,
                              pred_df,
                              yaxis = NULL
                              ) {
  # tmp <-
  #   mod_df %>%
  #   dplyr::filter(district == "01")
  # df <-  tmp
  
  # yaxis = NULL
  # if(is.null(yaxis)) {
  #   yaxis <- df$predictor_long_name[1]
  # }
  
  # mod_obj <- lm_list[["district_03"]]$model
  yaxis <- df$predictor_long_name[1]
  # p_str <-  df$predictor[1]
  # 
  # pred_df <- make_prediction(
  #   model     = mod_obj,
  #   predictor = p_str,
  #   val       = as.numeric(300)
  # )
  
  new_observed_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = resp_val, y = predictor_val),
      size = 2
    ) +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = predictor_val),
                           method = "lm",
                           se = FALSE
    ) +
    ggplot2::geom_point(
      data = pred_df,
      ggplot2::aes(x = fitted, y = predictor_val),
      color = "red",
      size = 5
    ) +
    ggplot2::ylim(c(0, 1000)) +
    ggplot2::xlim(c(1800, 2030)) +
    # ggplot2::scale_x_continuous(
    #   breaks = seq(1800, 2030, by = 25)
    #   ) +
    # ggplot2::scale_y_continuous(
    #   breaks = seq(1800, 2030, by = 100)
    #   ) +
    ggplot2::labs(
      x = "Observed average call year",
      y = paste0("Observed ", yaxis)
    ) + 
    ggplot2::theme_bw() 
  
  return(new_observed_plot)
  
  
}

