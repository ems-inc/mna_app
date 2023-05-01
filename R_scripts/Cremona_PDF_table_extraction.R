# install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(lubridate)
library(readr)
library(stringi)
library(sf)
library(rgdal)

rm(list = ls())

setwd("C:/Users/smame/OneDrive/Desktop/EMS_Git/mna_app")

# Read in the data from the consultant report ----
pdf_file <- "./data/2023-02-07 Updated ESA and RAP DRAFT  22-162-PMC.pdf"
output_dir <- "C:/Users/smame/OneDrive - Environmental Material Science Inc/Projects/Client Based Projects/Nichols_PlainsMidstream/Cremona/Background/data"

# Functions
`%nin%` <- Negate(`%in%`)

## Groundwater PHCs ----
PDF <- pdf_text(pdf_file)[c(42:45)] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  str_remove(., "M21-01|M21-02|M21-03|M21-04|M21-05A|M22-05B|M21-06|M21-08|M21-09|M21-12|NSZD-32 (AKA M21-12)|M21-14|M21-16|M21-17|M21-44|M22-19|M22-21|M22-27|M22-28|M22-29|M22-30|M22-32A|M22-33|M22-36|NSZD-34") %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
df_gw_phc <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% 
  slice(21:65,76:139,150:167,169:213,224:243) %>%  # drop useless rows
  select(-c(V8:V14)) %>% 
  rename(date = V1, benzene = V2, toluene = V3, ethylbenzene = V4, xylenes = V5, 
         F1 = V6, F2 = V7) %>% 
  mutate(well = rep(c("M21-01","M21-02","M21-03","M21-04","M21-05A",
                      "M22-05B","M21-06","M21-08","M21-09","M21-12",
                      "NSZD-32 (AKA M21-12)","M21-14","M21-16","M21-17","M21-44",
                      "M22-19","M22-21","M22-27","M22-28","M22-29",
                      "M22-30","M22-32A","M22-33","M22-36","NSZD-34"), 
                    c(18,17,10,13,16,1,20,5,9,16,4,16,1,6,1,3,5,3,7,1,10,1,2,3,4)),
         date = dmy(date),
         benzene = as.numeric(gsub("<", "", benzene)),
         toluene = as.numeric(gsub("<", "", toluene)),
         ethylbenzene = as.numeric(gsub("<", "", ethylbenzene)),
         xylenes = as.numeric(gsub("<", "", xylenes)),
         F1 = as.numeric(gsub("<", "", F1)),
         F2 = as.numeric(gsub("<", "", F2))) %>% 
  relocate(well, .before = "date")
write.csv(df_gw_phc, paste(output_dir, "Cremona_GW_PhaseII_2022.csv", sep = "/"))

## Surface water PHCs ----
PDF <- pdf_text(pdf_file)[c(46)] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  str_remove(., paste(paste(paste("SW", sprintf("%02d", c(1,2,2,2,3,4,5)), sep = "-"), c("","A","","B","","",""), sep = ""), collapse = "|")) %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
df_sw_phc <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>% #View()
  slice(20:33,35:54) %>%  # drop useless rows
  select(-c(V8:V12)) %>% 
  rename(date = V1, benzene = V2, toluene = V3, ethylbenzene = V4, xylenes = V5, 
         F1 = V6, F2 = V7) %>% 
  mutate(well = rep(paste(paste("SW", sprintf("%02d", c(1,2,2,2,3,4,5)), sep = "-"), c("","A","","B","","",""), sep = ""), 
                    c(3,6,1,8,8,4,4)),
         date = dmy(date),
         benzene = as.numeric(gsub("<", "", benzene)),
         toluene = as.numeric(gsub("<", "", toluene)),
         ethylbenzene = as.numeric(gsub("<", "", ethylbenzene)),
         xylenes = as.numeric(gsub("<", "", xylenes)),
         F1 = as.numeric(gsub("<", "", F1)),
         F2 = as.numeric(gsub("<", "", F2))) %>% 
  relocate(well, .before = "date")
write.csv(df_sw_phc, paste(output_dir, "Cremona_surface_water_PhaseII_2022.csv", sep = "/"))

## Soil PHCs ----
PDF <- pdf_text(pdf_file)[c(48:50)] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") %>% 
  str_remove(., paste(paste(paste(paste("M", rep(c(21,22), c(19,29)), sep = ""), sprintf("%02d", c(1:20,22:25,27:30,5,19,31,32,33,33,34,34,35,35,36:45)), sep = "-"),
                            c(rep("",28), "B", rep("",4), "1", "", "1", "", "1", rep("",10)), sep = ""), collapse = "|")) %>% 
  str_trim() %>% 
  strsplit(split = " ")
PDF <- Filter(length, PDF)  # Remove empty lists
df_phc <- PDF %>% 
  map_df(~.x %>% 
           map(~if(length(.)) . else NA) %>% 
           do.call(what = cbind) %>% 
           as_tibble) %>%# View()
slice(46:61,63:67,69:74,76:83,85:88,
  90:93,95,105,107:108,110:131,
  133:135,137:142,144:147,149:151,153:157,159:169,
  171:172,174:176,178:192,194:200,202:207,
  217:223,225:229,231:240,242:260,262:273) %>%  # drop useless rows
  select(-c(V12:V18)) #%>% View()

date_list <- unlist(PDF[grep("Apr|Oct|Dec", PDF)])[grep("Apr|Oct|Dec", unlist(PDF[grep("Apr|Oct|Dec", PDF)]))]

df_phc <- as_tibble(t(apply(df_phc, 1, function(x) `length<-`(x[x %nin% unique(df_phc$V2[grep("Apr|Oct|Dec|Fine|Grained", df_phc$V2)])], length(x)))))
df_phc <- t(apply(df_phc, 1, function(x) `length<-`(x[x %nin% unique(df_phc$V2[grep("Apr|Oct|Dec|Fine|Grained", df_phc$V2)])], length(x)))) %>% #View()
  as_tibble() %>% 
  select(-c(V10:V11)) %>% 
  rename(depth = V1, benzene = V2, toluene = V3, ethylbenzene = V4, xylenes = V5, 
         F1 = V6, F2 = V7, F3 = V8, F4 = V9) %>% 
  mutate(depth = as.numeric(depth),
         borehole = rep(paste(paste(paste("M", rep(c(21,22), c(19,29)), sep = ""), sprintf("%02d", c(1:20,22:25,27:30,5,19,31,32,33,33,34,34,35,35,36:45)), sep = "-"),
                              c(rep("",28), "B", rep("",4), "1", "", "1", "", "1", rep("",10)), sep = ""), 
                        c(rep(3,5),2,8,4,5,2,6,2,rep(2,2),5,rep(7,2),4,2,3,rep(4,2),2,3,2,3,1,5,rep(2,2),4,3,5,3,4,3,4,1,3,5,4,6,5,4,5,7,10,7)),
         date = dmy(rep(date_list, 
                        c(rep(3,5),2,8,4,5,2,6,2,rep(2,2),5,rep(7,2),4,2,3,rep(4,2),2,3,2,3,1,5,rep(2,2),4,3,5,3,4,3,4,1,3,5,4,6,5,4,5,7,10,7))),
         benzene = as.numeric(gsub("<", "", benzene)),
         toluene = as.numeric(gsub("<", "", toluene)),
         ethylbenzene = as.numeric(gsub("<", "", ethylbenzene)),
         xylenes = as.numeric(gsub("<", "", xylenes)),
         F1 = as.numeric(gsub("<", "", F1)),
         F2 = as.numeric(gsub("<", "", F2)),
         F3 = as.numeric(gsub("<", "", F3)),
         F4 = as.numeric(gsub("<", "", F4))) %>% 
  relocate(any_of(c("borehole","date")), .before = "depth")
df_phc[c(118,125,131),c(3:9)] <- df_phc[c(118,125,131),c(4:10)]
write.csv(df_phc, paste(output_dir, "Cremona_soil_PhaseII_2022.csv", sep = "/"))

## Boreholes ----
PDF <- pdf_text(pdf_file)[c(55:78)] %>%
  readr::read_lines() %>%
  str_squish() %>%
  str_remove_all(., "<[^<[0-9]+]") #%>%
PDF <- PDF[grep("BOREHOLE NO|CO-ORDINATES",PDF)]
PDF <- gsub("CLIENT: Nichols FIELD PERSONNEL: L.PICKERING, A.YEE BOREHOLE NO: |LOCATION: 14-03-032-04-W5M, Cremona, AB CO-ORDINATES: ", "", PDF)
PDF <- gsub("M22-05 B","M22-05B", PDF)
PDF <- gsub("M22-32 A","M22-32A", PDF)
PDF <- PDF %>%
  str_trim() %>%
  strsplit(split = ", | ")
df_borehole <- PDF %>%
  map_df(~.x %>%
           map(~if(length(.)) . else NA) %>%
           do.call(what = cbind) %>%
           as_tibble)
bh_ids <- df_borehole$V1[seq(1,nrow(df_borehole),2)]
bh_nor <- df_borehole$V2[seq(2,nrow(df_borehole),2)]
bh_eas <- df_borehole$V4[seq(2,nrow(df_borehole),2)]
bh_ele <- df_borehole$V6[seq(2,nrow(df_borehole),2)]
df_borehole <- data.frame(borehole = bh_ids,
                          northing = bh_nor,
                          easting = bh_eas,
                          elevation = bh_ele)

df_borehole <- df_borehole %>%
  distinct(borehole, northing, easting, .keep_all = T) %>% 
  mutate(across(ends_with("ing"), as.numeric))
# write.csv(df_borehole, paste(output_dir, "Cremona_boreholes_PhaseII_2022.csv", sep = "/"))

# ## Master well/borehole list
# unique(c(df_gw_phc$well, df_sw_phc$well, df_phc$borehole))
# setdiff(df_borehole$borehole, unique(c(df_gw_phc$well, df_sw_phc$well, df_phc$borehole)))

# These are from a consultant map overlain in Google Earth. So should be accurate.
setwd(output_dir)
kml_df <- st_read("./Boreholes.kml")
df_geo <- kml_df %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  # mutate(Name = substr(Name, 1, 3)) %>% 
  select(-Description) %>% 
  rename(borehole = Name) %>% 
  relocate(latitude, .after = latitude) %>% 
  # filter(name %in% paste("Q", sprintf("%02d", 1:5), sep = "")) %>% 
  arrange(borehole)
# projections
lon_lat <- as.matrix(df_geo[,c("longitude", "latitude")])
east_north <- project(lon_lat, "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
df_geo <- df_geo %>%
  mutate(easting = east_north[,1]) %>%
  mutate(northing = east_north[,2])
df_geo <- left_join(df_geo, df_borehole, by = "borehole") %>% 
  select(-c(northing.y, easting.y)) %>% 
  rename(easting = easting.x,
         northing = northing.x)
write.csv(df_geo, paste(output_dir, "Cremona_boreholes_PhaseII_2022.csv", sep = "/"))


# df_geo %>%
#   ggplot(aes(x = easting, y = northing)) + geom_point() + geom_label(
#     data = df_geo,
#     aes(label = name),
#     nudge_x = 2,
#     nudge_y = 2
#   ) +
#   geom_point(
#     data = df_borehole %>% mutate(easting = as.numeric(easting), northing = as.numeric(northing)),
#     aes(x = easting, y = northing),
#     col = "blue"
#   ) +
#   geom_label(
#     data = df_borehole %>% mutate(easting = as.numeric(easting), northing = as.numeric(northing)),
#     aes(x = easting, y = northing, label = borehole),
#     nudge_x = -2,
#     nudge_y = -2,
#     col = "blue"
#   )



# ## Groundwater nutrients ----
# ### Table A4 part 1 ----
# PDF <- pdf_text(pdf_file)[40] %>%
#   readr::read_lines() %>%
#   str_squish() %>%
#   str_remove_all(., "<[^<[0-9]+]") %>% 
#   gsub("\\-{3}","NA", .) %>% 
#   gsub("(\\-)(\\s)","-", .) %>%
#   gsub(" \\(","_\\(", .) %>% 
#   gsub("as ","_as", .) %>% 
#   gsub(", ","_", .) %>% 
#   gsub("Total Organic Carbon","TOC", .) %>% 
#   gsub("Total Dissolved Solids","TDS", .) %>% 
#   gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
#   str_trim() %>% 
#   strsplit(split = " ")
# PDF <- Filter(length, PDF)  # Remove empty lists
# dates <- PDF[[8]]
# df_a41 <- PDF %>% 
#   map_df(~.x %>% 
#            map(~if(length(.)) . else NA) %>% 
#            do.call(what = cbind) %>% 
#            as_tibble) %>% 
#   slice(9:33) %>%  # drop useless rows
#   setNames(c("parameter","units",dates)) %>% 
#   pivot_longer(
#     cols = matches("May|Sep|Oct"),
#     names_to = "date",
#     # names_prefix = "wk",
#     values_to = "value",
#     values_drop_na = TRUE
#   ) %>% 
#   mutate(well = rep(rep(c("MW7","08-04","08-06","08-10","08-11","09-04"),c(1,3,2,3,2,3)), nlevels(factor(parameter))))
# 
# ### Table A4 part 2 ----
# well_col <- rep(rep((paste(c(rep(16,5),rep(17,7)), sprintf("%02d", c(2:6,1:7)), sep = "-")),c(3,3,3,3,3,2,1,1,2,1,1,2)), nlevels(factor(df_a41$parameter)))
# n_dates <- length(rep((paste(c(rep(16,5),rep(17,7)), sprintf("%02d", c(2:6,1:7)), sep = "-")),c(3,3,3,3,3,2,1,1,2,1,1,2)))
# params <- unique(df_a41$parameter)
# units1 <- unique(df_a41$units)
# n_times <- c(18,1,1,1,4)*25
# PDF <- pdf_text(pdf_file)[41] %>%
#   readr::read_lines() %>%
#   str_squish() %>%
#   str_remove_all(., "<[^<[0-9]+]") %>% 
#   gsub("\\-{3}","NA", .) %>% 
#   gsub("(\\-)(\\s)","-", .) %>%
#   gsub(" \\(","_\\(", .) %>% 
#   gsub("as ","_as", .) %>% 
#   gsub(", ","_", .) %>% 
#   gsub("Total Organic Carbon","TOC", .) %>% 
#   gsub("Total Dissolved Solids","TDS", .) %>% 
#   gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
#   str_trim() %>% 
#   strsplit(split = " ")
# PDF <- Filter(length, PDF)  # Remove empty lists
# dates <- PDF[[6]]
# df_a42 <- PDF %>% 
#   map_df(~.x %>% 
#            map(~if(length(.)) . else NA) %>% 
#            do.call(what = cbind) %>% 
#            as_tibble) %>% 
#   slice(7:31) %>%  # drop useless rows
#   setNames(dates) %>% 
#   pivot_longer(
#     cols = matches("May|Sep|Oct"),
#     names_to = "date",
#     values_to = "value",
#     values_drop_na = TRUE
#   ) %>% 
#   mutate(well = well_col,
#          parameter = rep(params, each = n_dates),
#          units = rep(c(units1,units1[1]), n_times)) %>% 
#   relocate(parameter, .before = date) %>% 
#   relocate(units, .after = parameter)
# 
# ### Bind the nutrient tables together ----
# df_nutrients <- bind_rows(df_a41, df_a42) %>% 
#   mutate(date = dmy(date)) %>% 
#   arrange(.,parameter, well, date) %>% 
#   mutate(value = as.numeric(gsub("<|*<", "", value)))
# 
# ## Monitoring well data ----
# ### Table A4 part 1 ----
# PDF <- pdf_text(pdf_file)[30] %>%
#   readr::read_lines() %>%
#   str_squish() %>%
#   str_remove_all(., "<[^<[0-9]+]") %>% 
#   gsub("\\-{3}|\\-{2}|ND|NS","NA", .) %>% 
#   gsub("(\\-)(\\s)","-", .) %>%
#   gsub(" \\(","_\\(", .) %>% 
#   gsub("as ","_as", .) %>% 
#   gsub(", ","_", .) %>% 
#   gsub("Total Organic Carbon","TOC", .) %>% 
#   gsub("Total Dissolved Solids","TDS", .) %>% 
#   gsub("Chemical Oxygen Demand","Chemical_Oxygen_Demand", .) %>% 
#   str_trim() %>% 
#   strsplit(split = " ")
# PDF <- Filter(length, PDF)  # Remove empty lists
# # dates <- PDF[[8]]
# df_well <- PDF %>% 
#   map_df(~.x %>% 
#            map(~if(length(.)) . else NA) %>% 
#            do.call(what = cbind) %>% 
#            as_tibble) %>% 
#   slice(18,20:32) %>%  # drop useless rows
#   setNames(c("well","date","total_depth_mbtoc","ground_elev_m","toc_elev_m",
#              "water_depth_mbtoc","water_depth_mbgl","gw_elev_m","lnapl_thickness_mm","well_headsp_vap_ppmv",
#              "well_headsp_vap_ch4_ppmv","well_headsp_vap_gw_ppmv","temp_deg","ec_us_cm","tds_mgL","do_mgL","ph","orp_mV")) %>% 
#   mutate(across(total_depth_mbtoc:orp_mV, as.numeric)) %>% 
#   mutate(date = dmy(date))
# 
# # Output the files ----
# write_rds(df_nutrients, "./data/lang_nutrients.rds")
# write_rds(df_phc, "./data/lang_hydrocarbons.rds")
# write_rds(df_well, "./data/lang_well_data.rds")
