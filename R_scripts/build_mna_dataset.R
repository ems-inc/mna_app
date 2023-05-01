library(tidyverse)
library(lubridate)
library(sf)         # Read kml files
library(rgdal)      # Project points

rm(list = ls())

setwd("C:/Users/smame/OneDrive/Desktop/EMS_Git/mna_app")

# See box 4-5 in: 
# National Research Council. 2000. Natural Attenuation for Groundwater
#   Remediation. Washington, DC: The National Academies Press.
#   https://doi.org/10.17226/9792. 

# Read in the data ----
# Generated through 'PDF_table_extraction_beta.R'
df_phc <- read_rds("./data/lang_hydrocarbons.rds")
df_nutrients <- read_rds("./data/lang_nutrients.rds")
df_well <- read_rds("./data/lang_well_data.rds")

# Classify the wells as up or downgradient ----
# This was based on groundwater flow and PHC concentrations
df_phc <- df_phc %>% 
  # mutate(gradient = ifelse(well %in% c("08-03","08-04","09-05","16-01","16-02","16-03","16-05","16-06","17-01","17-04","MW5","MW7"), "downgradient","upgradient"))
  mutate(gradient = ifelse(well %in% c("09-04","17-05","17-06"), "upgradient","downgradient"))

# Add geographic information ----
## Make a list to help with name matching
name_list <- levels(as.factor(df_phc$well))
name_list[1:25] <- paste("S", name_list[1:25], sep = "")

## Read in the kml file for historical boreholes and wrangle
kml_df <- st_read("./data/Historical_Boreholes.kml")
df_geo <- kml_df %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(-Description) %>% 
  rename(name = Name, 
         lon = long) %>% 
  relocate(lon, .after = lat) %>% 
  filter(name %in% name_list)

## Manually set lat/lons here
df_phc <- df_phc %>%
  mutate(longitude = NA, latitude = NA) %>% 
  mutate(longitude = replace(longitude, well == "08-04", df_geo[01,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-04", df_geo[01,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-05", df_geo[02,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-05", df_geo[02,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-06", df_geo[03,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-06", df_geo[03,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-07", df_geo[04,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-07", df_geo[04,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-08", df_geo[05,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-08", df_geo[05,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-09", df_geo[06,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-09", df_geo[06,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-10", df_geo[07,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-10", df_geo[07,2])) %>% 
  mutate(longitude = replace(longitude, well == "08-11", df_geo[08,3])) %>%
  mutate(latitude  = replace(latitude,  well == "08-11", df_geo[08,2])) %>% 
  mutate(longitude = replace(longitude, well == "09-01", df_geo[09,3])) %>%
  mutate(latitude  = replace(latitude,  well == "09-01", df_geo[09,2])) %>% 
  mutate(longitude = replace(longitude, well == "09-02", df_geo[10,3])) %>%
  mutate(latitude  = replace(latitude,  well == "09-02", df_geo[10,2])) %>% 
  mutate(longitude = replace(longitude, well == "09-03", df_geo[11,3])) %>%
  mutate(latitude  = replace(latitude,  well == "09-03", df_geo[11,2])) %>% 
  mutate(longitude = replace(longitude, well == "09-04", df_geo[12,3])) %>%
  mutate(latitude  = replace(latitude,  well == "09-04", df_geo[12,2])) %>% 
  mutate(longitude = replace(longitude, well == "09-05", df_geo[13,3])) %>%
  mutate(latitude  = replace(latitude,  well == "09-05", df_geo[13,2])) %>% 
  mutate(longitude = replace(longitude, well == "16-02", df_geo[14,3])) %>%
  mutate(latitude  = replace(latitude,  well == "16-02", df_geo[14,2])) %>% 
  mutate(longitude = replace(longitude, well == "16-03", df_geo[15,3])) %>%
  mutate(latitude  = replace(latitude,  well == "16-03", df_geo[15,2])) %>% 
  mutate(longitude = replace(longitude, well == "16-04", df_geo[16,3])) %>%
  mutate(latitude  = replace(latitude,  well == "16-04", df_geo[16,2])) %>% 
  mutate(longitude = replace(longitude, well == "16-05", df_geo[17,3])) %>%
  mutate(latitude  = replace(latitude,  well == "16-05", df_geo[17,2])) %>% 
  mutate(longitude = replace(longitude, well == "16-06", df_geo[18,3])) %>%
  mutate(latitude  = replace(latitude,  well == "16-06", df_geo[18,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-01", df_geo[19,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-01", df_geo[19,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-02", df_geo[20,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-02", df_geo[20,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-03", df_geo[21,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-03", df_geo[21,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-04", df_geo[22,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-04", df_geo[22,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-05", df_geo[23,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-05", df_geo[23,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-06", df_geo[24,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-06", df_geo[24,2])) %>% 
  mutate(longitude = replace(longitude, well == "17-07", df_geo[25,3])) %>%
  mutate(latitude  = replace(latitude,  well == "17-07", df_geo[25,2])) %>% 
  mutate(longitude = replace(longitude, well == "MW5", df_geo[26,3])) %>%
  mutate(latitude  = replace(latitude,  well == "MW5", df_geo[26,2])) %>% 
  mutate(longitude = replace(longitude, well == "MW7", df_geo[27,3])) %>%
  mutate(latitude  = replace(latitude,  well == "MW7", df_geo[27,2]))

## Projections
lon_lat <- as.matrix(df_phc[,c("longitude", "latitude")])
east_north <- project(lon_lat, "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
df_phc <- df_phc %>%
  mutate(easting = east_north[,1]) %>%
  mutate(northing = east_north[,2])

# Have a look at the point distribution
df_phc %>% 
  ggplot(aes(x = easting, y = northing, color = gradient, label = well)) + geom_point(aes(size = benzene)) + 
  geom_text(aes(label = well), nudge_x = 8) 

# Aerobic respiration
## Need stoichiometry list that contains constants for each PHC (e.g., toluene, benzene, octane, etc.)
stoich_consts <- list(
  mws_ea = data.frame(
    o2 = 32,
    no3 = 62.01,
    so4 = 96.06,
    fe2 = 55.84,
    ch4 = 16.05,
    co2 = 44.01,
    caco3 = 100.09),
  toluene = data.frame(
    process = c("c7h8","co2","coco3"),
    aerobic_resp = c(0.32,1.07,0),
    denitr = c(0.21,0.69,1.57),
    sulph_reduct = c(0.21,0.71,1.62),
    iron_reduct = c(0.05,0.15,0.35),
    methanogenesis = c(1.28,1.52,0)
    ),
  octane = data.frame(
    process = c("c8h18","co2","coco3"),
    aerobic_resp = c(0.29,0.88,0),
    denitr = c(0.18,0.57,1.29),
    sulph_reduct = c(0.19,0.59,1.33),
    iron_reduct = c(0.04,0.13,0.29),
    methanogenesis = c(1.14,0.77,0)
  ),
  dodecane = data.frame(
    process = c("c12h26","co2","coco3"),
    aerobic_resp = c(0.29,0.89,0),
    denitr = c(0.19,0.58,1.31),
    sulph_reduct = c(0.19,0.59,1.35),
    iron_reduct = c(0.04,0.13,0.29),
    methanogenesis = c(1.15,0.82,0)
  ),
  benzene = data.frame(
    process = c("c6h6","co2","coco3"),
    aerobic_resp = c(0.16,0.55,0),
    denitr = c(0.21,0.71,1.61),
    sulph_reduct = c(0.22,0.73,1.67),
    iron_reduct = c(0.05,0.16,0.36),
    methanogenesis = c(1.3,1.65,0)
  )
)

# Calculate difference between upgradient and downgradient well data
unique(df_well$well)

df_well_upgrad <- df_well %>% 
  mutate(gradient = ifelse(well %in% c("09-04","17-05","17-06"), "upgradient","downgradient")) %>% 
  filter(gradient == "upgradient") %>% 
  summarise(across(total_depth_mbtoc:orp_mV, ~ mean(.x, na.rm = TRUE)))

df_test <- right_join(df_nutrients, df_well, by = c("date","well")) %>% filter(date > "2019-01-01")

df_well_diff <- df_well %>% 
  mutate(do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV,
         do_mgL_diff = do_mgL - df_well_upgrad$orp_mV)


mg_benz_L <- NA    # Sum of aerobic respiration, denitrification, sulphate reduction, Fe reduction, methanogenesis




