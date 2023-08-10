library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)

model_map <- dplyr::tibble(
              district   = c("01", "05", "08", "64"),
              resp_var   = c("avg_call_year","avg_call_year", "avg_call_year","avg_call_year"),
              predictor  = c("swe","fx", "swe","fx")
            )

mod_df <- dplyr::tibble(
                district      = rep(model_map$district, each = 20),
                resp_var      = rep(model_map$resp_var, each = 20),
                predictor     = rep(model_map$predictor, each = 20),
                predictor_val = c(
                  # runif(20, min = 0, max = 600),
                  # runif(20, min = 0, max = 250),
                  # runif(20, min = 0, max = 600),
                  # runif(20, min = 0, max = 250)
                  seq(0, 600, length.out = 20),
                  seq(0, 250, length.out = 20),
                  seq(0, 600, length.out = 20),
                  seq(0, 250, length.out = 20)
                ),
                resp_val = rep(runif(20, min = 1800, max = 2023), 4)
                # resp_val      = runif(80, min = 1800, max = 2023)
                # resp_val      = rep(seq(1800, 2023, length.out = 20), 4)
              )

# rep(runif(20, min = 1800, max = 2023), 4)

dist_id = "05"

tmp <- 
  mod_df %>% 
  dplyr::filter(district == dist_id)

make_lm <- function(model_data) {
  
  # model_data <- tmp
  
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
# paste0(tmp$resp_var[1], " ~ ", paste0(unique(tmp$predictor), collapse = "+"))
# summary(lm(resp_val ~ predictor_val, data = tmp))

lm_model <- lm(resp_val ~ predictor_val, data = tmp)

tmp <- 
  tmp %>% 
  dplyr::mutate(fitted = lm_model$fitted.values)

ggplot2::ggplot() +
  ggplot2::geom_point(data =tmp,  ggplot2::aes(x = resp_val, y = fitted))
  # ggplot2::geom_point(data =tmp,  ggplot2::aes(x = resp_val, y = predictor_val))

summary(lm(resp_val ~ predictor_val, data = tmp))



lm_out$model

make_prediction <- function(model, val) {
  
  # put value into a dataframe as 'predictor_val'
  out <- data.frame(predictor_val = val)
  
  # add prediction as 'fitted' column in out 
  out$fitted <- unname(
                    predict.lm(model, newdata = out)
                    )
  
  return(out)
}






