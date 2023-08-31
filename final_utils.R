# leaflet basemap w/ HUC4 shape
model_basemap <- function(shp) {
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
    leaflet::addScaleBar("bottomleft") %>%
    # leafem::addMouseCoordinates() %>%
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
      label = ~paste0("District: ", district),
      layerId = ~district,
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
    # leafem::addMouseCoordinates() %>%
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
      label = ~paste0("District: ", district),
      layerId = ~district,
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

make_yeartype_rightograph_plot <- function(df) {
  
  # df <-
  #   avg_yeartype %>%
  #   dplyr::filter(district == "06")
  
  # df <- weekly_calls
  # yeartype = "average"
  # dist = 1
  
  # df <-
  #   avg_yeartype %>%
  #   dplyr::filter(
  #     district == "64"
  #   )
  # df$label <- NA
  # df <-
  #   df %>%
  #   dplyr::group_by(year_type) %>%
  #   dplyr::mutate(
  #     label = dplyr::case_when(
  #       week == max(week) ~ year_type,
  #       TRUE              ~ NA
  #     )
  #   )
  # yeartype_min = 1840
  # yeartype_max = 2000
  
  # district_lab = unique(df$district)

  # df %>%
  #   dplyr::mutate(
  #     year_type = factor(year_type, levels = c("Dry", "Average", "Wet"))
  #   ) %>%
  
  rightograph <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = week, 
                                    y = priority_date,
                                    color = year_type,
                                    alpha = year_type,
                                    size = year_type
                                    # color = factor(year_type),
                                    # alpha = factor(year_type)
                                    ),
                       # alpha = 0.7,
                       size = 3
    ) +
    # ggplot2::geom_hline(ggplot2::aes(x = day, y = priority_date), yintercept = min_line, size = 2.5, color = "black") +
    ggplot2::scale_x_continuous(
      limits = c(1, 52), 
      breaks =   seq(1, 52, length.out = length(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"))),
      labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec")
    ) +
    ggplot2::scale_y_datetime(
      limits = c(as.POSIXct("1840-01-01 00:00:00"),
                 as.POSIXct("2000-01-01 12:00:00")),
      breaks = seq.POSIXt(
        as.POSIXct("1840-01-01"),
        as.POSIXct("2000-01-01"),
        "20 years"
        ),
      date_labels = "%Y"
      ) +
    # ggplot2::ylim(c(as.POSIXct("1840-01-01 00:00:00"), 
    #                 as.POSIXct("2000-01-01 12:00:00"))) +
    ggplot2::scale_alpha_manual(
      values = c("Average" = 1, "Wet" = 0.5, "Dry" = 0.5),
      guide = FALSE
      ) +
    # ggplot2::scale_size_manual(
    #     values = c("Average" = 3, "Wet" = 2, "Dry" = 2),
    #     guide = FALSE
    #   ) +
    ggplot2::scale_color_manual(
      values = c("Wet" = "dodgerblue", "Average" = "black", "Dry" = "darkred"),
      guide = ggplot2::guide_legend(
        direction = "vertical",
        title.position = "top"
      )
    ) +
    ggplot2::labs(
      title = paste0("Right-o-graph"),
      # title = paste0("Right-o-graph (District: ", district_lab, ")"),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year type"
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
      legend.key.height = unit(1, "cm"),
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0),
      legend.box.background = ggplot2::element_rect(colour = "black", size = 2)
    ) 
  
  return(rightograph)
  
  
}
make_avg_yeartype_rightograph_plot <- function(df, type) {
  
  # df <- weekly_calls
  # yeartype = "average"
  # dist = 1
  
  # df <-
  #   avg_yeartype %>%
  #   dplyr::filter(
  #     district == "01"
  #   )
  # df$label <- NA
  # df <-
  #   df %>%
  #   dplyr::group_by(year_type) %>%
  #   dplyr::mutate(
  #     label = dplyr::case_when(
  #       week == max(week) ~ year_type,
  #       TRUE              ~ NA
  #     )
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
    # ggrepel::geom_label_repel(
    #   ggplot2::aes(x = week, 
    #                y = priority_date,
    #                label = label),
    #                  nudge_x = 0,
    #                  na.rm = TRUE) +
    # ggplot2::geom_text(
    #   ggplot2::aes(x = week, y = priority_date, label = label),
    #   nudge_x = 0.5, nudge_y = 0.5, # Adjust these nudges for better label placement
    #   size = 4,
    #   color = "black",
    #   vjust = 1.5,
    #   show.legend = FALSE
    # ) +
    ggplot2::scale_color_manual(values = c("Wet" = "dodgerblue", "Average" = "black", "Dry" = "darkred"), 
                                guide = ggplot2::guide_legend(
                                  direction = "horizontal",
                                  title.position = "top"
                                )) +
    ggplot2::labs(
      title = paste0("Right-o-graph"),
      # title = paste0("Right-o-graph (District: ", district_lab, ")"),
      # subtitle = "Water rights above priority date lines are called out by more senior rights at or below the priority date lines",
      # caption = "Black horizontal line represents average % out of priority over the period of record",
      x     = "",
      y     = "Priority Date",
      color = "Year Type"
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
make_mlr <- function(model_data) {
  
  # model_data <- dplyr::filter(df, district == ud) 
  
  # model_data <- dplyr::filter(df, district == ud)
  
  # create linear regression model
  mlr_model <- lm(
    resp_val ~ predictor_val1 + predictor_val2, 
    data = model_data
  )
  # summary(mlr_model)
  # create a string of the LM equation
  mlr_equation = paste0(model_data$resp_var[1], " ~ ", paste0(c(unique(model_data$predictor1), unique(model_data$predictor2)), collapse = " + "))
  
  # summarize model
  mlr_summary = summary(mlr_model)
  
  # extract coefficients
  mlr_coeffs <- janitor::clean_names(mlr_summary$coefficients)
  
  # janitor::clean_names(lm_summary$coefficients[1, ])
  # unname(unlist(lm_summary$coefficients[1, ]))
  # model_data %>% na.omit()
  
  # ADD fitted data as column to orginal dataframe
  model_data <-
    model_data %>%
    dplyr::mutate(fitted = mlr_model$fitted.values)
  
  # return list with fitted model and data
  res <- list(
    model      = mlr_model,
    model_data = model_data,
    equation   = mlr_equation,
    r2         = round(mlr_summary$r.squared, 3),
    coeffs     = mlr_coeffs
  )
  
  return(res)
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

# make a list of linear regression models from a dataframe of districts with these columns: 
#   "district", "resp_var", "resp_val", "predictor", and "predictor_val" columns
make_mlr_list <- function(df) {
  
  # df <- lm_data
  
  udistricts <- unique(df$district)
  # df <- lm_data
  # df
  # i = 1
  mlr_list <- lapply(1:length(udistricts), function(i) {
    # i = 1
    # unique district
    ud <- udistricts[i]
    # ud
    # message(i, "/", length(udistricts))
    # message("District: ", ud)
    
    make_mlr(dplyr::filter(df, district == ud) )
    
    
  }) %>% 
    stats::setNames(paste0("district_", udistricts))
  
  return(mlr_list)
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

make_mlr_prediction <- function(model, pred1, pred2, val1, val2) {
  # make a prediction with new data from user input
  # pred <- make_prediction(
  #   model     = model_obj()
  #   predictor = predictor_str()
  #   val       = as.numeric(input$newDataInput)
  # # )
  #   
  #   model = mlr_list[["district_01"]]$model
  #   pred1  = mlr_list[["district_01"]]$model_data$predictor1[1]
  #   pred2 = mlr_list[["district_01"]]$model_data$predictor2[1]
  # val1 = 445
  # val2 = -1
  #   
  # put value into a dataframe as 'predictor_val'
  out <- data.frame(
    predictor1 = pred1,
    predictor2 = pred2,
    predictor_val1 = val1,
    predictor_val2 = val2
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
      ggplot2::aes(x = resp_val, y = predictor_val1),
      size = 2
    ) +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = predictor_val1), 
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
  # pred_df <- make_prediction(
  #   model     = lm_list[["district_01"]]$model,
  #   predictor = lm_list[["district_01"]]$model_data$predictor_long_name[1],
  #   val       = as.numeric(200))
  # 
  # df = lm_list[["district_01"]]$model_data
  # yaxis = NULL
  
  #           
  # make_new_observed_plot(
  #   df      = model_data(),
  #   pred_df = pred,
  #   yaxis   = NULL
  # )
  # tmp <-
  #   mod_df %>%
  #   dplyr::filter(district == "01")
  # df <-  tmp
  
  # yaxis = NULL
  # if(is.null(yaxis)) {
  #   yaxis <- df$predictor_long_name[1]
  # }
  # yaxis
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
      ggplot2::aes(x = resp_val, y = predictor_val1),
      size = 2
    ) +
    ggplot2::geom_smooth(ggplot2::aes(x = resp_val, y = predictor_val1),
                           method = "lm",
                           se = FALSE
    ) +
    ggplot2::geom_point(
      data = pred_df,
      ggplot2::aes(x = fitted, y = predictor_val1),
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

# df <- model_data %>% 
#   dplyr::filter(district == "01")

# make_observed_plot2(
#   df = df, 
#   pred_col = "predictor_val1",
#   yaxis = df$predictor_long_name[1],
#   ylim = c(0, 1000)
# )
# 
# make_observed_plot2(
#   df = df, 
#   pred_col = "predictor_val2",
#   yaxis = df$predictor_long_name2[1],
#   ylim = c(-4, 4)
# )

# df$predictor_val1 
make_observed_plot2 <- function(df,
                                pred_col = NULL,
                                # long_name_col = NULL,
                               yaxis = NULL,
                               ylim = c(0, 1000)
) {
  # tmp <- 
  #   mod_df %>% 
  #   dplyr::filter(district == "01")
  # df <-  tmp
  
  # yaxis = NULL
  # if(is.null(yaxis)) {
  #   yaxis <- df$predictor_long_name[1]
  # }
  
  # yaxis <- df$predictor_long_name[1]
  
  
  observed_plot <-
    df %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes_string(x = "resp_val", y = pred_col),
      size = 2
    ) +
    ggplot2::geom_smooth(ggplot2::aes_string(x = "resp_val", y = pred_col), 
                         method = "lm",
                         se = FALSE
    ) + 
    ggplot2::ylim(ylim) +
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
# mmake_new_observed_plot2(
#     df,
#     pred_col = NULL,
#     pred_df,
#     yaxis = predictor_long_name(),
#     ylim = c(0, 1000)
# )
    
make_new_observed_plot2 <- function(
    df,
    pred_col = NULL,
    pred_df,
    yaxis = NULL,
    ylim = c(0, 1000)
) {
  # pred_df <- make_prediction(
  #   model     = lm_list[["district_01"]]$model,
  #   predictor = lm_list[["district_01"]]$model_data$predictor_long_name[1],
  #   val       = as.numeric(200))
  # 
  # df = lm_list[["district_01"]]$model_data
  # yaxis = NULL
  
  #           
  # make_new_observed_plot(
  #   df      = model_data(),
  #   pred_df = pred,
  #   yaxis   = NULL
  # )
  # tmp <-
  #   mod_df %>%
  #   dplyr::filter(district == "01")
  # df <-  tmp
  
  # yaxis = NULL
  # if(is.null(yaxis)) {
  #   yaxis <- df$predictor_long_name[1]
  # }
  # yaxis
  # mod_obj <- lm_list[["district_03"]]$model
  # yaxis <- df$predictor_long_name[1]
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
      ggplot2::aes_string(x = "resp_val", y = pred_col), 
      size = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes_string(x = "resp_val", y = pred_col), 
                         method = "lm",
                         se = FALSE
    ) +
    ggplot2::geom_point(
      data = pred_df,
      ggplot2::aes_string(x = "fitted", y = pred_col), 
      color = "red",
      size = 5
    ) +
    ggplot2::ylim(ylim) +
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

#' Get a single EDDI data value for each year on April 1 and May 1
#'
#' @param aoi SF
#' @param varname Charcter vector, climateR::params for a list of gridMET climate parameters to choose from
#' @param start_date starting date string (YYYY-MM-DD ). Defaults to "1980-01-01"
#' @param end_date date string (YYYY-MM-DD ). Defaults to yesterday.
#' @param name_col character, name of column in SF object that that uniquely identifies each polygon. Default is "district".
#' @param wide logical, whether data should be return wide (column for each climate variable) or long (a column naming the variable and a column represnting the value of the variable). Default is TRUE, returns a wide dataframe
#' @param verbose logical, should messages print or not. Default is FALSE, no messages print
#' @return dataframe with weekly average climate variable values for each polygon in the provided aoi SF object
#' @export
eddi_from_catalog2 <- function(
    catalog    = NULL,
    aoi        = NULL,
    varname    = NULL,
    start_date = NULL,
    end_date   = NULL,
    name_col   = "district",
    wide       = TRUE,
    verbose    = FALSE
) {
  
  # catalog    = cat
  # aoi        = dplyr::mutate(dists, 
  #                            district = as.integer(district))
  # # varname    = "eddi30d"
  # varname    = c("eddi30d", "eddi1y")
  # start_date = Sys.Date() - 365
  # # start_date = Sys.Date() - (365*5)
  # end_date   = Sys.Date()
  # name_col   = "district"
  # wide       = TRUE
  # verbose    = TRUE
  
  # rm(shp, aoi, varname, start_date, end_date, wide, verbose)
  # aoi = dists2
  # varname = "eddi30d"
  # start_date = start
  # end_date = end
  # name_col = "district"
  # wide       = TRUE
  # verbose = TRUE
  
  # if no varname, use catalog variable name
  if(is.null(varname)) {
    varname <- unique(catalog$variable)
  }
  
  # if start date is NULL
  if(is.null(start_date)) {
    
    start_date = "1980-01-01"
    
  }
  
  # if end date is NULL
  if(is.null(end_date)) {
    
    end_date = Sys.Date() - 1
    
  }
  
  # make name column name lowercase
  name_col <- tolower(name_col)
  
  # make sure AOI is in correct CRS and is a MULTIPOLYGON
  # # aoi <-
  # shp <-
  #   aoi %>%
  #   sf::st_transform(4326) %>%
  #   sf::st_cast("MULTIPOLYGON")
  
  # make lower case column names
  names(aoi) <- tolower(names(aoi))
  
  message(paste0(
    "Getting gridMET data...\n",
    "-------------------------",
    "\nStart date: ", start_date,
    "\nEnd date: ", end_date
  ))
  
  # get daily gridMET data
  gridmet <- climateR::dap(
    catalog   = catalog,
    AOI       = aoi,
    startDate = start_date,
    endDate   = end_date,
    verbose   = verbose
  )
  
  # district names
  district_names <- paste0(aoi[[name_col]])
  
  # remove gridMET "category" data that is accidently returned
  gridmet <- gridmet[names(gridmet) != "category"]
  
  message(paste0("Getting April 1/May 1  EDDI values..."))
  
  # mask and crop variables for each polygon in 'aoi'
  eddi_rasters <- lapply(1:length(gridmet), function(i) {
    # i = 1
    # loop over all polygons and crop/mask SpatRasters
    lapply(seq_len(nrow(aoi)), function(x) {
      # x = 1
      raster_date_snapshots(
        raster   = gridmet[[i]],
        polygon  = aoi[x, ]
      )
    }
    ) %>%
      stats::setNames(district_names)
    
  }) %>%
    stats::setNames(varname)
  
  # eddi_rasters %>% names()
  # eddi_rasters$eddi30d %>% names()
  # eddi_rasters$eddi1y %>% names()
  
  message(paste0("Calculating means..."))
  
  tidy_gridmet <- lapply(seq_along(eddi_rasters), function(i) {
    
    # lapply counter message
    if(verbose) {
      message(paste0(i, "/", length(eddi_rasters)))
    }
    
    lapply(seq_along(eddi_rasters[[i]]), function(x) {
      
      eddi_rasters[[i]][[x]] %>%
        as.data.frame(xy = F) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), mean)) %>%
        tidyr::pivot_longer(cols = dplyr::everything()) %>%
        tidyr::separate(name, c("variable", "date"), sep = "_", extra = "merge") %>%
        dplyr::mutate(
          date      = as.Date(gsub("_", "-", date)),
          district  = names(eddi_rasters[[i]])[x]
          # units     = unique(terra::units(eddi_rasters[[i]][[x]]))
        ) %>%
        dplyr::relocate(district, date, variable, value)
      
    }) %>%
      dplyr::bind_rows()
    
  }) %>%
    dplyr::bind_rows()
  
  # plot to check outputs
  # tidy_gridmet %>%
  #   # dplyr::filter(district %in% c(1)) %>%
  #   dplyr::filter(district %in% c(1, 2, 3, 4, 5, 6)) %>%
  #   dplyr::mutate(
  #     year  = lubridate::year(date),
  #     month = lubridate::month(date, label = T)
  #   ) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(ggplot2::aes(x = year, y = value, color = variable)) +
  #   ggplot2::facet_grid(month~district)
  
  # if wide is TRUE, then pivot the table wider and return that
  if(wide) {
    
    tidy_gridmet <-
      tidy_gridmet %>%
      dplyr::mutate(
        year     = format(as.Date(date), "%Y"),
        new_cols = paste0(
          ifelse(format(as.Date(date), "%m") == "04", "apr", "may"),
          "_",
          variable
        )
      ) %>%
      dplyr::select(-variable, -date) %>%
      tidyr::pivot_wider(
        # id_cols     = c(year, district),
        names_from  = "new_cols",
        values_from = "value"
      ) %>%
      dplyr::mutate(
        district = dplyr::case_when(
          as.numeric(district) < 10 ~ paste0("0", district),
          TRUE                      ~ paste0(district)
        )
      )
  } else {
    
    tidy_gridmet <-
      tidy_gridmet %>%
      dplyr::mutate(
        month    = ifelse(format(as.Date(date), "%m") == "04", "apr", "may"),
      ) %>%
      dplyr::relocate(district, date, month, variable, value)
    
  }
  
  return(tidy_gridmet)
}

#' Get a single EDDI data value for each year on April 1 and May 1
#'
#' @param aoi SF
#' @param varname Charcter vector, climateR::params for a list of gridMET climate parameters to choose from
#' @param start_date starting date string (YYYY-MM-DD ). Defaults to "1980-01-01"
#' @param end_date date string (YYYY-MM-DD ). Defaults to yesterday.
#' @param name_col character, name of column in SF object that that uniquely identifies each polygon. Default is "district".
#' @param wide logical, whether data should be return wide (column for each climate variable) or long (a column naming the variable and a column represnting the value of the variable). Default is TRUE, returns a wide dataframe
#' @param verbose logical, should messages print or not. Default is FALSE, no messages print
#' @return dataframe with weekly average climate variable values for each polygon in the provided aoi SF object
#' @export
eddi_from_catalog <- function(
    catalog    = NULL,
    aoi        = NULL,
    varname    = NULL,
    start_date = NULL,
    end_date   = NULL,
    name_col   = "district",
    wide       = TRUE,
    verbose    = FALSE
) {
  
  # end = Sys.Date()
  # start = Sys.Date() - 365
  # catalog = cat
  # varname = "eddi30d"
  # aoi = dists2
  # start_date = start
  # end_date = end
  # name_col   = "district"
  # wide       = TRUE
  # verbose    = FALSE
  
  # rm(shp, aoi, varname, start_date, end_date, wide, verbose)
  # aoi = dists2
  # varname = "eddi30d"
  # start_date = start
  # end_date = end
  # name_col = "district"
  # wide       = TRUE
  # verbose = TRUE
  
  # if no varname, use catalog variable name
  if(is.null(varname)) {
    varname <- unique(catalog$variable)
  }
  
  # if start date is NULL
  if(is.null(start_date)) {
    
    start_date = "1980-01-01"
    
  }
  
  # if end date is NULL
  if(is.null(end_date)) {
    
    end_date = Sys.Date() - 1
    
  }
  
  # make name column name lowercase
  name_col <- tolower(name_col)
  
  # make sure AOI is in correct CRS and is a MULTIPOLYGON
  # # aoi <-
  # shp <-
  #   aoi %>%
  #   sf::st_transform(4326) %>%
  #   sf::st_cast("MULTIPOLYGON")
  
  # make lower case column names
  names(aoi) <- tolower(names(aoi))
  
  message(paste0(
    "Getting gridMET data...\n",
    "-------------------------",
    "\nStart date: ", start_date,
    "\nEnd date: ", end_date
  ))
  
  # get daily gridMET data
  gridmet <- climateR::dap(
    catalog   = catalog,
    AOI       = aoi,
    startDate = start_date,
    endDate   = end_date,
    verbose   = verbose
  )
  
  # gridmet <- climateR::getGridMET(
  #   AOI       = aoi,
  #   varname   = varname,
  #   startDate = start_date,
  #   endDate   = end_date,
  #   verbose   = verbose
  # )
  
  # district names
  district_names <- paste0(aoi[[name_col]])
  
  # remove gridMET "category" data that is accidently returned
  gridmet <- gridmet[names(gridmet) != "category"]
  
  message(paste0("Getting April 1/May 1  EDDI values..."))
  
  # mask and crop variables for each polygon in 'aoi'
  eddi_rasters <- lapply(1:length(gridmet), function(i) {
    # i = 1
    # loop over all polygons and crop/mask SpatRasters
    lapply(seq_len(nrow(aoi)), function(x) {
      # x = 1
      raster_date_snapshots(
        raster   = gridmet[[i]],
        polygon  = aoi[x, ]
      )
    }
    ) %>%
      stats::setNames(district_names)
    
  }) %>%
    stats::setNames(varname)
  
  message(paste0("Calculating means..."))
  
  tidy_gridmet <- lapply(seq_along(eddi_rasters), function(i) {
    
    # lapply counter message
    if(verbose) {
      message(paste0(i, "/", length(eddi_rasters)))
    }
    
    lapply(seq_along(eddi_rasters[[i]]), function(x) {
      
      eddi_rasters[[i]][[x]] %>%
        as.data.frame(xy = F) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), mean)) %>%
        tidyr::pivot_longer(cols = dplyr::everything()) %>%
        tidyr::separate(name, c("variable", "date"), sep = "_", extra = "merge") %>%
        dplyr::mutate(
          date      = as.Date(gsub("_", "-", date)),
          district  = names(eddi_rasters[[i]])[x]
          # units     = unique(terra::units(eddi_rasters[[i]][[x]]))
        ) %>%
        dplyr::relocate(district, date, variable, value)
      
    }) %>%
      dplyr::bind_rows()
    
  }) %>%
    dplyr::bind_rows()
  
  # if wide is TRUE, then pivot the table wider and return that
  if(wide) {
    
    tidy_gridmet <-
      tidy_gridmet %>%
      dplyr::mutate(
        year     = format(as.Date(date), "%Y"),
        new_cols = paste0(
          ifelse(format(as.Date(date), "%m") == "04", "apr", "may"),
          "_",
          variable
        )
      ) %>%
      dplyr::select(-variable, -date) %>%
      tidyr::pivot_wider(
        # id_cols     = c(year, district),
        names_from  = "new_cols",
        values_from = "value"
      ) %>%
      dplyr::mutate(
        district = dplyr::case_when(
          as.numeric(district) < 10 ~ paste0("0", district),
          TRUE                      ~ paste0(district)
        )
      )
  } else {
    
    tidy_gridmet <-
      tidy_gridmet %>%
      dplyr::mutate(
        month    = ifelse(format(as.Date(date), "%m") == "04", "apr", "may"),
      ) %>%
      dplyr::relocate(district, date, month, variable, value)
    
  }
  
  return(tidy_gridmet)
}

# update the "duration" column in the catalog entry that holds the metadata used for getting EDDI 30D data, 
# this function checks the end of the period of record of the EDDI data and updates the end of the string to reflect the current timeperiod
update_duration <- function(cat, days_from_today = 15) {
  
  # end of catalog duration period 
  period_end <- strsplit(cat$duration, "/")[[1]][2]
  
  # new date to replace old period end with
  new_end <- Sys.Date() - days_from_today
  
  # current "period_end" is relatively up to date, do nothing
  if(period_end >= new_end) {
    
    return(cat)
    
  }
  
  new_duration <- gsub("/\\d{4}-\\d{2}-\\d{2}", paste0("/" , new_end), cat$duration)
  
  # update the duration string
  cat$duration <- new_duration
  
  return(cat)
  
}
# cat <- readRDS("eddi_catalog.rds") 
# cat <- update_duration(cat)
# varname = "eddi30d"
# name_col = "district"
# wide = TRUE
# verbose = TRUE
# 
# end = Sys.Date()
# start = Sys.Date() - 365
# 
# cat <- readRDS("eddi_catalog.rds") 
# cat <- update_duration(cat)
# 
# new_eddi <- eddi_from_catalog(
#   catalog    = cat,
#   aoi        = dplyr::mutate(dists2, 
#                              district = as.integer(district)),
#   varname    = "eddi30d",
#   start_date = Sys.Date() - 365,
#   end_date   = Sys.Date(),
#   name_col   = "district",
#   verbose    = TRUE
#   )
# 
# climateR::params %>%
#   dplyr::filter(id == "gridmet", varname == "eddi") %>% 
#   .$variable
# gm2 <-
#   climateR::params %>%
#   dplyr::filter(id == "gridmet", variable %in% c("eddi30d", "eddi1y"), varname == "eddi")
# saveRDS(gm2, "eddi_catalog2.rds")
# 
# gm <- climateR::params %>%
#   dplyr::filter(id == "gridmet", variable == "eddi30d", varname == "eddi")
# saveRDS(gm, "eddi_catalog.rds")
# 
# cat <- readRDS("eddi_catalog.rds") 
# cat$duration
# 
# cat <- update_duration(cat)
# cat$duration
# 
# eddi <- climateR::dap(
#   catalog = gm,
#   AOI = dists2,
#   startDate = start,
#   endDate = end
# )

# mapview::mapview(raster::raster(gridmet$eddi)) + shp[x, ]
# raster   = gridmet[[i]]
# polygon  = shp[x, ]
# define a function to crop and mask a single SpatRaster for a single polygon
#' Internal function used in get_eddi_years for extracting April 1/May 1 EDDI rasters from stack of SpatRasters
#'
#' @param raster
#' @param polygon
#'
#' @return
#' @export
#'
#' @examples
raster_date_snapshots <- function(raster, polygon) {
  
  # CROP AND MASK RASTERS
  msk <- terra::mask(
    terra::crop(raster, polygon),
    polygon
  )
  # library(terra)
  # plot(msk$`eddi30d_2022-08-27`)
  
  # dates
  dates     <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})", "\\1", names(raster)))
  
  # april/may dates
  apr <- as.Date(dates[format(as.Date(dates), "%m") %in% c("04")])
  may <- as.Date(dates[format(as.Date(dates), "%m") %in% c("05")])
  
  # Filter for the closest date to April 1 for each year
  apr_starts <- as.Date(tapply(apr, format(apr, "%Y"), function(x) {find_month_starts(x, "4")}),  origin = "1970-01-01")
  may_starts <- as.Date(tapply(may, format(may, "%Y"), function(x) {find_month_starts(x, "5")}),  origin = "1970-01-01")
  # dates[format(as.Date(dates), "%m") %in% c("04", "05")]
  
  # variable name
  var       <- gsub("_\\d{4}-\\d{2}-\\d{2}", "", names(raster))[1]
  
  # subset layers
  msk <- msk[[names(msk) %in% paste0(var, "_", apr_starts) | names(msk) %in% paste0(var, "_", may_starts)]]
  
  # variable units
  var_units <- terra::units(msk)
  
  # set units
  terra::units(msk) <- var_units
  
  # set names
  names(msk) <- gsub("\\.", "_", gsub("X", paste0(var, "_"), names(msk)))
  
  return(msk)
}

# select dates closest to the start of the 'month' from a date vector
find_month_starts <- function(dates, month = "4") {
  
  # start date of month
  start <- as.Date(paste0(format(dates, "%Y"), paste0("-",
                                                      ifelse(as.numeric(month) < 10, paste0("0", month), month),
                                                      "-01")))
  # find date closest to start date for month
  close <- dates[which.min(abs(dates - start))]
  
  return(close)
}

#######################################
# ----------- TEST GT TABLE -----------
#######################################

# # # linear regression lookup table
# lm_lookup <- readr::read_csv("cpo_linear_regression_lookup_v2.csv")
# district_id <- "06"
# 
# df_tbl <- dplyr::tibble(
#             col1 = c("May 1 SWE",
#                      "May EDDI 30 day",
#                      "Prediction",
#                      "Equation",
#                      "R²"
#             ),
#             col2 = c(
#               500,
#               -2,
#               1950,
#               paste0("Call year ~ May 1 SWE  + May EDDI 30 day"),
#               0.65
#             ),
#             row_name = c("-", "-", "-", "-", "-"),
#             group = c("Predictor Variables", "Predictor Variables", "Prediction", "Model Equation", "Performance")
#           )
# 
# # # model_table <-
#   df_tbl %>%
#   gt::gt(rowname_col = "row_name", groupname_col = "group") %>%
#   gt::tab_header(
#     # title = md("**My Table Title**")
#     title = gt::md(paste0(
#       lm_lookup$district_name[lm_lookup$district == district_id]
#     )
#     ),
#     subtitle = gt::md(paste0("**District: ", district_id, "**"))
#     # title = md(paste0("**District: ", district_num, "**")),
#     # subtitle = md(paste0(district_name))
#   ) %>%
#   gt::tab_style(
#     style = list(
#       gt::cell_text(color = "white")  # Change the color to your preferred color
#     ),
#     locations = gt::cells_column_labels()
#   ) %>%
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     # locations = cells_column_labels(columns = "group")
#     # locations = cells_body(columns = group)
#     locations = gt::cells_row_groups()
#   ) %>%
#   gt::tab_style(
#     style = gt::cell_fill(color = "gold", alpha = 0.8), # highlight R squared value
#     locations = gt::cells_body(rows = 5, columns = "col2") # which cell to highlight
#   ) %>%
#   gt::tab_style(
#       style = gt::cell_fill(color = "#3EB489", alpha = 0.7), # highlight R squared value
#       locations = gt::cells_body(rows = 3, columns = "col2") # which cell to highlight
#     ) %>%
#     gt::tab_style(
#       style = gt::cell_fill(color = "indianred", alpha = 0.7), # highlight prediction value
#       locations = gt::cells_body(rows = c(1, 2), columns = "col2") # column and row of cell to highlight
#     ) %>% 
#   # gt::tab
#   gt::tab_options(
#     # style(gt::cell_text(size = px(16)))
#     table.font.size = 18
#   )


#######################################
#######################################
# district <- "06"
# pred1 = "may_swe"
# pred2 <- "may_eddi1y"
# 
# mod_obj <- lm_list$district_06$model
# mod_data <- lm_list$district_06$model_data
# mod_eq <- lm_list$district_06$equation
# 
# val1 <- 200
# val2 <- -1
# 
# pred <- make_mlr_prediction(
#   model     = mod_obj,
#   pred1 = pred1,
#   pred2 = pred2,
#   val1  = val1,
#   val2  = val2
# )
# 
# pred$equation <- mod_eq
# # long1 <- dplyr::filter(lm_lookup, district == "06")$predictor_long_name
# # long2 <- dplyr::filter(lm_lookup, district == "06")$predictor_long_name2
# 
# model_tbl <- 
#   dplyr::bind_cols(
#     dplyr::filter(lm_lookup, district == model_dist_id()),
#     pred
#   )
# 
# df_tbl <- dplyr::tibble(
#   col1 = c(model_tbl$predictor_long_name,
#            model_tbl$predictor_long_name2,
#            "Prediction",
#            "Equation", 
#            "R2"
#   ),
#   col2 = c(
#     model_tbl$predictor_val1,
#     predictor_val2, 
#     model_tbl$predictor_val2, 
#     model_tbl$equation, 
#     rsquared
#   ),
#   row_name = c("-", "-", "-", "-", "-"),
#   group = c("Predictor Variables", "Predictor Variables", "Prediction", "Model Equation", "Performance")
# )
# 
# df_tbl %>% 
#   gt(rowname_col = "row_name", groupname_col = "group") %>% 
#   tab_header(
#     # title = md("**My Table Title**")
#     title = md(paste0(district_name)),
#     subtitle = md(paste0("**District: ", district_num, "**"))
#     # title = md(paste0("**District: ", district_num, "**")),
#     # subtitle = md(paste0(district_name))
#   ) %>%
#   tab_style(
#     style = list(
#       cell_text(color = "white")  # Change the color to your preferred color
#     ),
#     locations = cells_column_labels()
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     # locations = cells_column_labels(columns = "group")
#     # locations = cells_body(columns = group)
#     locations = cells_row_groups()
#   )
# 
# library(gt)
# lm_list$district_06$equation
# 
# district_name <- model_tbl$district_name
# district_num <- model_tbl$district
# 
# predictor_name1 <- model_tbl$predictor_long_name
# predictor_name2 <- model_tbl$predictor_long_name2
# 
# predictor_val1 <- model_tbl$predictor_val1
# predictor_val2 <- model_tbl$predictor_val2
# 
# rsquared_str <- "R2"
# rsquared <- as.character(round(lm_list$district_06$r2, 2))
# 
# prediction_str <- "Prediction"
# prediction_val <- as.character(round(model_tbl$fitted, 0))
# 
# equation_str <- "Equation"
# model_eq <- paste0("Call year ~ ", predictor_name1, " + ", predictor_name2)
# 
# df_tbl <- dplyr::tibble(
#   col1 = c(model_tbl$predictor_long_name,
#            model_tbl$predictor_long_name2,
#            prediction_str,
#            rsquared_str),
#   col2 = c(predictor_val1, predictor_val2, prediction_val, rsquared)
# )
# library(sjPlot)
# 
# df_tbl <- dplyr::tibble(
#   col1 = c(model_tbl$predictor_long_name,
#            model_tbl$predictor_long_name2,
#            "Prediction",
#            "Equation", 
#            "R2"
#            ),
#   col2 = c(
#     model_tbl$predictor_val1,
#     predictor_val2, 
#     model_tbl$predictor_val2, 
#     model_tbl$equation, 
#     rsquared
#     ),
#   row_name = c("-", "-", "-", "-", "-"),
#   group = c("Predictor Variables", "Predictor Variables", "Prediction", "Model Equation", "Performance")
# )
# md(paste0("District: ", district_num))
# 
# paste0(district_name)
# df_tbl %>% 
#   gt(rowname_col = "row_name", groupname_col = "group") %>% 
#   tab_header(
#     # title = md("**My Table Title**")
#     title = md(paste0(district_name)),
#     subtitle = md(paste0("**District: ", district_num, "**"))
#     # title = md(paste0("**District: ", district_num, "**")),
#     # subtitle = md(paste0(district_name))
#   ) %>%
#   tab_style(
#     style = list(
#       cell_text(color = "white")  # Change the color to your preferred color
#     ),
#     locations = cells_column_labels()
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     # locations = cells_column_labels(columns = "group")
#     # locations = cells_body(columns = group)
#     locations = cells_row_groups()
#   )
# 
# cells_row_groups()
# 
# exibble %>% gt()
# exibble %>% 
# select(-c(fctr, date, time, datetime)) %>% 
#   gt(rowname_col = "row", groupname_col = "group") %>% 
#   sub_missing()
# exibble |> gt()
# district <- "06"
# pred1 = "may_swe"
# pred2 <- "may_eddi1y"
# 
# mod_obj <- lm_list$district_06$model
# mod_data <- lm_list$district_06$model_data
# mod_eq <- lm_list$district_06$equation
# 
# val1 <- 200
# val2 <- -1
# 
# pred <- make_mlr_prediction(
#   model     = mod_obj,
#   pred1 = pred1,
#   pred2 = pred2,
#   val1  = val1,
#   val2  = val2
# )
# 
# pred$equation <- mod_eq
# # long1 <- dplyr::filter(lm_lookup, district == "06")$predictor_long_name
# # long2 <- dplyr::filter(lm_lookup, district == "06")$predictor_long_name2
# 
# model_tbl <- 
#   dplyr::bind_cols(
#     dplyr::filter(lm_lookup, district == "06"),
#     pred
#   )
# 
# library(flextable)
# flextable::flextable(model_tbl)
# names(model_tbl)
# 
# district_name <- model_tbl$district_name
# district_num <- model_tbl$district
# 
# predictor_name1 <- model_tbl$predictor_long_name
# predictor_name2 <- model_tbl$predictor_long_name2
# 
# predictor_val1 <- model_tbl$predictor_val1
# predictor_val2 <- model_tbl$predictor_val2
# 
# rsquared_str <- "R2"
# rsquared <- as.character(round(lm_list$district_06$r2, 2))
# 
# prediction_str <- "Prediction"
# prediction_val <- as.character(round(model_tbl$fitted, 0))
# 
# 
# df_tbl <- dplyr::tibble(
#   col1 = c(predictor_name1, predictor_name2,prediction_str, rsquared_str),
#   col2 = c(predictor_val1, predictor_val2, prediction_val, rsquared)
# )
# tbl <- flextable::flextable(df_tbl)
# tbl
# pars <- as_paragraph(
#   as_chunk(c("District:", "District name:")), " ",
#   as_chunk(c(district_num,
#              district_name)
#   )
# )
# add_header_row(tbl, values = pars, top = FALSE)
# 
# ft_1 <- add_header_row(tbl, values = pars,
#                        colwidths = c(5, 6), top = FALSE)
# new_row <- list(
#   District =district_num,
#   "District name" = district_name
# )
# ft_1 <- flextable::add_header(tbl, values = new_row, top = FALSE)
# tbl <- flextable::flextable(df_tbl)
# flextable::add_header()
# fun <- function(x) {
#   paste0(
#     c("min: ", "max: "),
#     formatC(range(x))
#   )
# }
# new_row <- list(
#   District =district_num,
#   "District name" = district_name
# )
# flextable::add_header_row()
# new_row <- list(
#   Sepal.Length = fun(iris$Sepal.Length),
#   Sepal.Width =  fun(iris$Sepal.Width),
#   Petal.Width =  fun(iris$Petal.Width),
#   Petal.Length = fun(iris$Petal.Length)
# )
# ft01 <- fp_text_default(color = "red")
# ft02 <- fp_text_default(color = "orange")
# 
# pars <- as_paragraph(
#   as_chunk(c("District:", "District name:")), " ",
#   as_chunk(c(district_num,
#              district_name)
#   )
# )
# 
# ft_1 <- flextable(head(mtcars))
# ft_1 <- add_header_row(ft_1, values = pars,
#                        colwidths = c(5, 6), top = FALSE)
# 
# ft_1 <- flextable(data = head(iris))
# ft_1 <- add_header(ft_1, values = new_row, top = FALSE)
# ft_1 <- append_chunks(ft_1, part = "header", i = 2, )
# ft_1 <- theme_booktabs(ft_1, bold_header = TRUE)
# ft_1 <- align(ft_1, align = "center", part = "all")
# ft_1
# tbl
# flextable::add_body_row()
# tbl <- add_header_lines(tbl, "Predictor Variables and Values")
# # Simulated data for demonstration
# ft01 <- fp_text_default(color = "red")
# ft02 <- fp_text_default(color = "orange")
# 
# pars <- as_paragraph(
#   as_chunk(c("(1)", "(2)"), props = ft02), " ",
#   as_chunk(
#     c(
#       "My tailor is rich",
#       "My baker is rich"
#     ),
#     props = ft01
#   )
# )
# 
# ft_1 <- flextable(head(mtcars))
# 
# ft_1
# ft_1 <- add_body_row(ft_1,
#                      values = pars,
#                      colwidths = c(5, 6), top = FALSE
# )
# ft_1
# ft_1 <- add_body_row(ft_1,
#                      values = pars,
#                      colwidths = c(3, 8), top = TRUE
# )
# ft_1 <- theme_box(ft_1)
# ft_1
# library(flextable)
# # Simulated data for demonstration
# set.seed(123)
# data1 <- data.frame(
#   Variable = c("A", "B", "C"),
#   Value = c(10, 20, 30)
# )
# 
# data2 <- data.frame(
#   Category = c("X", "Y", "Z"),
#   Score = c(85, 92, 78)
# )
# rm(tbl1)
# # Create the first flextable
# tbl1 <- flextable(df_tbl)
# 
# # Set title for the first table
# tbl1 <- set_flextable_defaults(
#   font.size = 12
# )(tbl1)
# 
# # Create the second flextable
# tbl2 <- flextable(data2)
# 
# # Set title for the second table
# tbl2 <- set_flextable_defaults(
#   font.size = 12
# )(tbl2)
