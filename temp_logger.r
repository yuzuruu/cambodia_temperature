##############################################################################
# Water surface / air temperature in Kratie
# 10th. April 2024 
# Yuzuru Utsunomiya (Faculty of Economics, Nagasaki University, JAPAN)
#
#
##############################################################################
# 
# ----- load.library -----
library(tidyverse)
library(sf)
library(furrr)
library(khroma)
# ----- read.data -----
# make a list of target files
file_list <- 
  tibble(
    file_path = list.files("./temp_logger") %>% paste0("./temp_logger/",.),
    file_id = stringr::str_sub(file_path, start = -6, end = -5)
  )
# read the data files and combine
log_temp_lux <- 
  file_list %>% 
  group_by(file_id) %>% 
  nest() %>%
  dplyr::mutate(
    temp_lux = purrr::map(
      data, 
      ~
        readr::read_csv(
          file = .$file_path,
          col_names = FALSE,
          skip = 2,
        ) %>% 
        dplyr::select(1:4) %>% 
        data.table::setnames(c("id_data","time","temp","lux")) %>% 
        dplyr::mutate(
          gozengogo = stringr::str_sub(time, start = -2, end = -1),
          time = lubridate::mdy_hms(time),
          id_logger = file_id
        ) 
    )
  ) %>% 
  unnest()%>% 
  ungroup() %>% 
  dplyr::select(
    id_logger, id_data, time, temp, lux
  ) %>% 
  na.omit() %>% 
  dplyr::mutate(
    id_logger = factor(id_logger),
    id_data = factor(id_data),
    place = dplyr::case_when(
      id_logger == "25" ~ "Boeng_Tros_shore_pole_lower", 
      id_logger == "23" ~ "Boeng_Tros_port_higher", 
      id_logger == "03" ~ "Boeng_Tros_port_lower", 
      id_logger == "16" ~ "Boeng_Knay_lower", 
      id_logger == "02" ~ "UKT_poind_lower", 
      id_logger == "10" ~ "UKT_pond_higher", 
      TRUE ~ "hoge"
    )
  ) %>% 
  dplyr::filter(place != "hoge")
# save the data
readr::write_rds(log_temp_lux, "log_temp_lux.rds")
log_temp_lux <- readr::read_rds("log_temp_lux.rds")
# plot the data using line plot
line_log_temp_lux <- 
  log_temp_lux %>% 
  ggplot2::ggplot(
    aes(
      x = time,
      y = temp,
      color = place
    )
  ) +
  geom_line() +
  facet_wrap(~ place) +
  scale_color_okabeito() +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )

