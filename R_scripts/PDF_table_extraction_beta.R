# install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(lubridate)
library(readr)

rm(list = ls())

setwd("C:/Users/smame/OneDrive/Desktop/EMS_Git/mna_app")

# Read in the data from the consultant report ----
pdf_file <- "./data/ZE0803919 Lang FBP 2019 GWM Report.pdf"

## Groundwater PHCs ----
PDF <- pdf_text(pdf_file)[c(38,39)] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  str_remove(., "08-04|08-05|08-11|17-05|17-06|17-07|08-06|08-08|16-02|16-03|17-01|17-04|MW5|MW7|08-07|08-09|08-10|09-01|09-02|09-03|09-04|09-05|16-04|16-05|16-06|17-02|17-03") %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
df_phc <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% 
  slice(11:37,41:59,63:74,79:102) %>%  # drop useless rows
  select(-c(V11:V15)) %>% 
  rename(date = V1, gw_vap_concs = V2, 
         benzene = V3, toluene = V4, ethylbenzene = V5, xylenes = V6, 
         F1 = V7, F2 = V8, F3 = V9, F4 = V10) %>% 
  mutate(well = rep(c("08-04","08-05","08-11","09-04","17-05",
                      "17-06","17-07","08-06","08-08","16-02",
                      "16-03","17-01","17-04","MW5","MW7",
                      "08-07","08-09","08-10","09-01","09-02",
                      "09-03","09-05","16-04","16-05","16-06",
                      "17-02","17-03"), 
                    c(7,3,5,6,2,2,2,6,3,3,3,2,2,3,5,1,3,6,1,2,1,1,3,3,3,2,2)),
         date = dmy(date),
         gw_vap_concs = as.numeric(ifelse(gw_vap_concs == "ND", 0, 
                                          ifelse(gw_vap_concs == "--", NA, gw_vap_concs))),
         benzene = as.numeric(gsub("<", "", benzene)),
         toluene = as.numeric(gsub("<", "", toluene)),
         ethylbenzene = as.numeric(gsub("<", "", ethylbenzene)),
         xylenes = as.numeric(gsub("<", "", xylenes)),
         F1 = as.numeric(gsub("<", "", F1)),
         F2 = as.numeric(gsub("<", "", F2)),
         F3 = as.numeric(gsub("<", "", F3)),
         F4 = as.numeric(gsub("<", "", F4))) %>% 
  relocate(well, .before = "date")

## Groundwater nutrients ----
### Table A4 part 1 ----
PDF <- pdf_text(pdf_file)[40] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  gsub("\\-{3}","NA", .) %>% 
  gsub("(\\-)(\\s)","-", .) %>%
  gsub(" \\(","_\\(", .) %>% 
  gsub("as ","_as", .) %>% 
  gsub(", ","_", .) %>% 
  gsub("Total Organic Carbon","TOC", .) %>% 
  gsub("Total Dissolved Solids","TDS", .) %>% 
  gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
dates <- PDF[[8]]
df_a41 <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% 
  slice(9:33) %>%  # drop useless rows
  setNames(c("parameter","units",dates)) %>% 
  pivot_longer(
    cols = matches("May|Sep|Oct"),
    names_to = "date",
    # names_prefix = "wk",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  mutate(well = rep(rep(c("MW7","08-04","08-06","08-10","08-11","09-04"),c(1,3,2,3,2,3)), nlevels(factor(parameter))))

### Table A4 part 2 ----
well_col <- rep(rep((paste(c(rep(16,5),rep(17,7)), sprintf("%02d", c(2:6,1:7)), sep = "-")),c(3,3,3,3,3,2,1,1,2,1,1,2)), nlevels(factor(df_a41$parameter)))
n_dates <- length(rep((paste(c(rep(16,5),rep(17,7)), sprintf("%02d", c(2:6,1:7)), sep = "-")),c(3,3,3,3,3,2,1,1,2,1,1,2)))
params <- unique(df_a41$parameter)
units1 <- unique(df_a41$units)
n_times <- c(18,1,1,1,4)*25
PDF <- pdf_text(pdf_file)[41] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  gsub("\\-{3}","NA", .) %>% 
  gsub("(\\-)(\\s)","-", .) %>%
  gsub(" \\(","_\\(", .) %>% 
  gsub("as ","_as", .) %>% 
  gsub(", ","_", .) %>% 
  gsub("Total Organic Carbon","TOC", .) %>% 
  gsub("Total Dissolved Solids","TDS", .) %>% 
  gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
dates <- PDF[[6]]
df_a42 <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% 
  slice(7:31) %>%  # drop useless rows
  setNames(dates) %>% 
  pivot_longer(
    cols = matches("May|Sep|Oct"),
    names_to = "date",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  mutate(well = well_col,
         parameter = rep(params, each = n_dates),
         units = rep(c(units1,units1[1]), n_times)) %>% 
  relocate(parameter, .before = date) %>% 
  relocate(units, .after = parameter)

### Bind the nutrient tables together ----
df_nutrients <- bind_rows(df_a41, df_a42) %>% 
  mutate(date = dmy(date)) %>% 
  arrange(.,parameter, well, date) %>% 
  mutate(value = as.numeric(gsub("<|*<", "", value)))

## Monitoring well data ----
### Table A4 part 1 ----
PDF <- pdf_text(pdf_file)[30] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  gsub("\\-{3}|\\-{2}|ND|NS","NA", .) %>% 
  gsub("(\\-)(\\s)","-", .) %>%
  gsub(" \\(","_\\(", .) %>% 
  gsub("as ","_as", .) %>% 
  gsub(", ","_", .) %>% 
  gsub("Total Organic Carbon","TOC", .) %>% 
  gsub("Total Dissolved Solids","TDS", .) %>% 
  gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
# dates <- PDF[[8]]
df_well <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% 
  slice(18,20:32) %>%  # drop useless rows
  setNames(c("well","date","total_depth_mbtoc","ground_elev_m","toc_elev_m",
             "water_depth_mbtoc","water_depth_mbgl","gw_elev_m","lnapl_thickness_mm","well_headsp_vap_ppmv",
             "well_headsp_vap_ch4_ppmv","well_headsp_vap_gw_ppmv","temp_deg","ec_us_cm","tds_mgL","do_mgL","ph","orp_mV")) %>% 
  mutate(across(total_depth_mbtoc:orp_mV, as.numeric)) %>% 
  mutate(date = dmy(date))

# Output the files ----
write_rds(df_nutrients, "./data/lang_nutrients.rds")
write_rds(df_phc, "./data/lang_hydrocarbons.rds")
write_rds(df_well, "./data/lang_well_data.rds")
