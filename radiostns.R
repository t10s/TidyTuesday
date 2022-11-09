library(tidyverse)

#read data
stat_stns <- read.csv("state_stations.csv")
stn_info <- read.csv("station_info.csv")
stn_contour <- read.csv("FM_service_contour_current.txt", sep = "|")
fmq <- read.csv("fmq", sep = "|", header = FALSE)
fmq_slim <- select(fmq, c(2,11,38,39))
colnames(fmq_slim)[3] <- "application_id"
colnames(fmq_slim)[4] <- "lms_application_id"
stn_contour_xref <- inner_join(fmq_slim, stn_contour, by="application_id") %>%
  filter(V2 != "-           ")
colnames(stn_contour_xref)[1] <- "Call Sign"
colnames(stn_contour_xref)[2] <- "City"
map <- map_data("state")

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
colnames(stat_stns)[6] <- "region"
stat_stns <- mutate(stat_stns, region = tolower(region))

#split lat and long to sep cols
x <- stn_contour_xref[8]
x <- x %>% 
  mutate(transmitter_site = str_sub(transmitter_site, 1,-1)) %>% 
  separate(transmitter_site, into = c('lat', 'long'), sep = ',')

y <- select(stn_contour_xref, c(1,2,8))
y <- y %>% 
  mutate(transmitter_site = str_sub(transmitter_site, 1,-1)) %>% 
  separate(transmitter_site, into = c('lat', 'long'), sep = ',')
y <- transform(y, long = as.numeric(long))
y <- transform(y, lat = as.numeric(lat))


ggplot() +
  geom_polygon(data = map, aes(x=long, y=lat, group=group), colour = "black") +
  coord_map() +
  geom_point(data = y, aes(x=long, y=lat), colour = "red") +
  theme_minimal()





