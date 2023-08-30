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

# Load function data_utils.R file
source('utils.R')

# ############################################################################
# ############################################################################
 # LOAD IN DATA
water_rights <- readr::read_csv("detrended_all_data_final_v2.csv")

# linear regression lookup table
lm_lookup <- readr::read_csv("cpo_linear_regression_lookup.csv")

# subset of districts
dists      <- sf::read_sf("water_districts_subset.gpkg")

avg_yeartype <- readRDS("avg_weekly_calls_by_yeartype.rds")

# districts of interest
doi   = unique(lm_lookup$district)

# districts for Linear Regression models on Page 2
model_dists <- 
  dists %>% 
  # dplyr::filter(DISTRICT %in% c("1", "5", "8", "64")) %>% 
  dplyr::filter(DISTRICT %in% as.numeric(doi)) %>% 
  dplyr::mutate(
    DISTRICT = ifelse(DISTRICT < 10, paste0("0", DISTRICT), as.character(DISTRICT))
  )

# data used to generate linear regression models
mod_df <- readRDS("model_data.rds")

#  list of linear regression models and metrics
lm_list <- readRDS("lin_reg_model_list2.rds")

# ############################################################################
# ############################################################################

tmp <- 
  mod_df %>% 
  dplyr::filter(district == "01")


make_observed_value_plot <- function(df,
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

# ############################################################################
# ################## ADDING MISSING DISTRICT 9 data to RAW ###################
# ############################################################################

wrs <- readr::read_csv("detrended_all_data_final_v2.csv")
raw <- readr::read_csv("/Users/anguswatters/Downloads/cdss_raw_daily_call_data_combined.csv")

wrs %>% 
  dplyr::filter(district == "09") %>% 
  .$wdid %>% 
  unique()

# raw$analysis_date %>% unique() %>% max()
extra_wdids <- c("0902107", "0904288", "0901591", "0905632")

extra_calls <- lapply(1:length(extra_wdids), function(i) {
  
  message(i, "/", length(extra_wdids))
  
  calls <- get_calls2(
    wdid = extra_wdids[i],
    start_date = "1970-01-01",
    end_date = "2023-07-01"
  )
  
  calls 
  
})

extra_calls2 <- 
  extra_calls %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(
    district = 9
  ) %>% 
  dplyr::tibble()

readr::write_csv(extra_calls2, "/Users/anguswatters/Desktop/cpo_data/misc_data/cdss_raw_daily_call_data_district_09.csv")

# find common columns
common <- names(extra_calls2)[names(extra_calls2) %in% names(raw)]

# match column types
extra_calls2[common] <- lapply(common, function(x) {
  match.fun(
    paste0("as.", class(raw[[x]])[1]))(extra_calls2[[x]])
})

# bind district 9 data with the rest of the raw data
raw <- dplyr::bind_rows(raw, extra_calls2)

readr::write_csv(raw, "/Users/anguswatters/Desktop/cpo_data/call_data/cdss_raw_daily_call_data_combined_v2.csv")

# ###########################################################################
# ################### FIX DISTRICT 5 YEAR TYPE DATAFRAME ####################
# ###########################################################################

# water year type by district and year
yeartypes <- readr::read_csv("/Users/anguswatters/Desktop/cpo_data/year_type/water_district_yeartype.csv") %>% 
  dplyr::mutate(
    district = ifelse(district < 10, paste0("0", district), as.character(district))
  )

# replace district 5 data in "yeartypes" with this data
fixed_yeartypes <-
  readr::read_csv("/Users/anguswatters/Desktop/cpo_data/year_type/mainstem_yeartype.csv") %>% 
  dplyr::select(year, type) %>% 
  dplyr::mutate(district = "05") %>% 
  dplyr::relocate(district)

out_yeartypes <- dplyr::bind_rows(
  dplyr::filter(yeartypes, district != 5), 
  fixed_yeartypes
  ) %>% 
  dplyr::rename(year_type = type)

readr::write_csv(out_yeartypes, "/Users/anguswatters/Desktop/cpo_data/year_type/water_district_yeartype_v2.csv")

# ############################################################################
# ################### CREATE YEAR TYPES AVERAGE DATAFRAME ####################
# ############################################################################
# rm(out_yeartypes, yeartypes, fixed_yeartypes)

# water year type by district and year
yeartypes <- readr::read_csv("/Users/anguswatters/Desktop/cpo_data/year_type/water_district_yeartype_v2.csv")

raw <- readr::read_csv("/Users/anguswatters/Desktop/cpo_data/call_data/cdss_raw_daily_call_data_combined_v2.csv")
# tmp_raw <- raw %>% 
#   dplyr::filter(district == 5)
# 
# tmp_calls <- aggreg_weekly_wdid(tmp_raw)

wdid_calls <- aggreg_weekly_wdid(raw)

wdid_calls <- 
  wdid_calls %>% 
  dplyr::left_join(
    dplyr::mutate(
      yeartypes, 
      year = as.character(year)
      ),
    by = c("district", "year")
  ) 

readr::write_csv(wdid_calls, "/Users/anguswatters/Desktop/cpo_data/year_type/weekly_avg_district_call_data.csv")

# ############################################################################
# ################ SUMMARIZE YEAR TYPE DATA FOR DASHBOARD ####################
# ############################################################################

wdid_calls <- readr::read_csv("/Users/anguswatters/Desktop/cpo_data/year_type/weekly_avg_district_call_data.csv")

wdid_calls

avg_yeartype <- 
  wdid_calls %>% 
  aggreg_by_year_type() %>%
  dplyr::mutate(
    year_type = dplyr::case_when(
      year_type == "average" ~ "Average",
      year_type == "wet" ~ "Wet",
      year_type == "dry" ~ "Dry"
    )
  ) %>% 
  na.omit()


saveRDS(avg_yeartype, "avg_weekly_calls_by_yeartype2.rds")
tmp <- readRDS("avg_weekly_calls_by_yeartype.rds")
tmp %>% 
  dplyr::filter(district == 5)

avg_yeartype %>% 
  dplyr::filter(district == "05") 

tmp %>% 
  dplyr::filter(district == 5) %>% 
  na.omit()
avg_yeartype %>% 
  dplyr::filter(district == "05") %>% 
  na.omit()

avg_yeartype %>% 
  dplyr::filter(district == "09") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = week, y = priority_date, color = year_type))
  
# ############################################################################
# ############################################################################

wrs <- readr::read_csv("detrended_all_data_final_v2.csv")
raw <- readr::read_csv("/Users/anguswatters/Downloads/cdss_raw_daily_call_data_combined.csv")

wdid_calls <- aggreg_weekly_wdid(raw)
wdid_calls$district %>% unique()
wdid_calls$analysis_wdid %>% unique()
wdid_id <- "0604255"


# wdid_calls %>% 
#   dplyr::mutate(
#     call_year = lubridate::year(priority_date)
#   ) %>% 
#   dplyr::relocate(call_year) %>% 
#   dplyr::group_by(district, analysis_wdid, year) %>% 
#   dplyr::summarise(
#     call_year = mean(call_year, na.rm = T),
#     year = as.numeric(year)
#   ) %>% 
#   dplyr::ungroup() %>% 
wrs %>% 
  # dplyr::arrange(district) %>% 
  dplyr::filter(district == "1")  %>%
  dplyr::mutate(
    district = paste0("District: ", district)
  ) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = call_year_decimal), size = 2.5) +  
  # ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 15)) +
  # ggplot2::facet_wrap(~district) +
  ggplot2::labs(
    # title = "Annnual average call year",
    y = "Call year",
    x = "Year"
  )  +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title        = ggplot2::element_text(size = 18,
                                              face = "bold"
                                              # hjust = 0.5
                                              ),
    plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
    legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
    legend.text       = ggplot2::element_text(size = 16),
    # legend.position   = "bottom",
    
    axis.title        = ggplot2::element_text(size = 16, face = "bold"),
    axis.text         = ggplot2::element_text(size = 16),
    legend.key.width  = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.key.height = unit(1, "cm")
  ) 
# call_subset <- 
  # wdid_calls %>% 
  # dplyr::filter(analysis_wdid == wdid_id)
  # dplyr::filter(district == "01")

# average call year per year type per district
make_wdid_rightograph_plot(df = call_subset, wdid = wdid_id, highlight_year = 2002)

call_subset

wdid_calls$analysis_wdid %>% unique()
weekly_calls$district %>% unique()

weekly_calls <- 
  raw %>% 
  aggreg_weekly_wdid(omit_na = F) %>% 
  dplyr::left_join(
    dplyr::mutate(
      year_types, 
      year = as.character(year), 
      district = ifelse(district < 10, paste0("0", district), as.character(district))
    ),
    by = c("year", "district")
  )
avg_yeartype

readr::write_csv(weekly_calls, "/Users/anguswatters/Desktop/cpo_data/weekly_wdid_call_data_with_yeartypes.csv")
year_types <- readr::read_csv("/Users/anguswatters/Downloads/water_district_yeartype.csv")

weekly_calls <- aggreg_weekly_district(raw)
avg_yeartype <-  aggreg_by_year_type(weekly_calls) %>%
  dplyr::mutate(
    year_type = dplyr::case_when(
      year_type == "average" ~ "Average",
      year_type == "wet" ~ "Wet",
      year_type == "dry" ~ "Dry"
    )
  )
# saveRDS(weekly_calls, "data/weekly_calls_by_district.rds")
# saveRDS(avg_yeartype, "data/weekly_calls_by_yeartype.rds")

# subset of districts
dists      <- sf::read_sf("data/water_districts_subset.gpkg")

avg_yeartype <- readRDS("data/avg_weekly_calls_by_yeartype.rds")

# districts of interest
doi   = c("01", "05", "08", "64")

# districts for Linear Regression models on Page 2
model_dists <- 
  dists %>% 
  # dplyr::filter(DISTRICT %in% c("1", "5", "8", "64")) %>% 
  dplyr::filter(DISTRICT %in% as.numeric(doi)) %>% 
  dplyr::mutate(
    DISTRICT = ifelse(DISTRICT < 10, paste0("0", DISTRICT), as.character(DISTRICT))
  )

# ############################################################################
# ############################################################################

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
    water_district = click_dist$DISTRICT
  )
)

# buffer to search area for NHD features
buff <- sf::st_transform(
  sf::st_buffer(
    sf::st_transform(pt, 5070),
    1609.3*7
  ),
  4326
)

# buffer bounds
bounds <-
  buff %>%
  sf::st_bbox() %>%
  as.vector()

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
)

shp = sf::read_sf("C:/Users/angus/OneDrive/Desktop/github/dss_co_nasa/data/co-huc10")
shp

sf::write_sf(shp, "D:/nasa_dss/data/co-huc10.gpkg")
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
