library(tidyverse)

#read data
stat_stns <- read.csv("state_stations.csv")
stn_info <- read.csv("station_info.csv")
stn_contour <- read.csv("FM_service_contour_current.txt", sep = "|")
fmq <- read.csv("fmq", sep = "|", header = FALSE)
fmq_slim <- select(fmq, c(2,38,39))
fmq_slim <- fmq_slim %>% mutate(across(where(is.character), str_trim)) #remove whitespace from char type
colnames(fmq_slim)[1] <- "call_sign"
colnames(fmq_slim)[2] <- "application_id"
colnames(fmq_slim)[3] <- "lms_application_id"

stn_contour_slim <- select(stn_contour, c(1,2,3,8))
colnames(stn_contour_slim)[4] <- "transmitter_site"

stn_contour_xref <- merge(fmq_slim, stn_contour_slim, by = "application_id") %>%
  filter(call_sign != "-           ")
stn_contour_xref_slim <- select(stn_contour_xref, c(2,6))

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

#join coordinates with station type data
q <- left_join(stn_contour_xref_slim, stn_info_all, by = "call_sign") %>% 
  na.omit() %>%
  select(c(1,2,4,7,13))

#split lat and long to sep cols
x <- q %>% 
  mutate(transmitter_site = str_sub(transmitter_site, 1,-1)) %>% 
  separate(transmitter_site, into = c('lat', 'long'), sep = ',')
x$long <- as.numeric(x$long)
x$lat <- as.numeric(x$lat)
x <- x %>% filter(long >= -130)


ggplot() +
  geom_polygon(data = map, aes(x=long, y=lat, group=group), col = "black", alpha = 0) +
  coord_map() +
  geom_point(data = x, aes(x=long, y=lat, colour = type)) +
  theme_minimal()





