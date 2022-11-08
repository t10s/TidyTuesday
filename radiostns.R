library(tidyverse)

#read data
stat_stns <- read.csv("state_stations.csv")
stn_info <- read.csv("station_info.csv")
stn_contour <- read.csv("FM_service_contour_current.txt", sep = "|")
xref <- read.csv("xref_cdbs_lm_app_id_transfer.dat", sep = "|") #downloaded extra from FCC site (https://enterpriseefiling.fcc.gov/dataentry/public/tv/lmsDatabase.html)

#join dfs
stn_info_all <- inner_join(stat_stns, stn_info, by = "call_sign")
religious <- c("Religious", "Christian", "Gospel", "gospel", "Worship", "worship")
classical <- c("Classical", "classical")
sports <- c("Sports", "sports")
pop_rock <- c("pop", "Pop", "Rock", "rock", "hits", "Hits", "top", "Top", "mainstream", "Mainstream")

#group stns and remove nas
stn_info_all <- stn_info_all %>%  mutate(type = case_when(str_detect(format, paste0(religious, collapse = "|")) ~ "Religious",
                                                          str_detect(format, paste0(classical, collapse = "|")) ~ "Classical",
                                                          str_detect(format, paste0(sports, collapse = "|")) ~ "Sports",
                                                          str_detect(format, paste0(pop_rock, collapse = "|")) ~ "Rock/Pop/Mainstream")) %>%
  na.omit(type)

#join map
colnames(stn_info_all)[6] <- "region"
stn_info_all <- stn_info_all %>% mutate(region = tolower(region))
map_stn <- inner_join(map, stn_info_all)

ggplot(map_stn, aes(x = long, y = lat) +
         geom_polygon(aes(group = group, fill = city)))



#cross ref ids - figure out how to plot contours!!


