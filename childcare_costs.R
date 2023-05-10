#tidytuesday 09.05.2023 - childcare costs USA

library(tidyverse)
library(mapdata)
library(ggmap) #for theme_nothing()
library(cowplot)

#read data
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

# get county data for map
data <- map_data('county')

# aggregate childcare data
# aggregated var: # of males in science, management, business, and arts jobs
agg_data1 <- childcare_costs %>% select(c(1,40)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(memp_m))

# aggregated var: # of females in science, management, business, and arts jobs
agg_data2 <- childcare_costs %>% select(c(1,41)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(femp_m))

# aggregated var: # of males in service jobs
agg_data3 <- childcare_costs %>% select(c(1,43)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(memp_service))

# aggregated var: # of females in service jobs
agg_data4 <- childcare_costs %>% select(c(1,44)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(femp_service))

# aggregated var: # of males in sales and office jobs
agg_data5 <- childcare_costs %>% select(c(1,46)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(memp_sales))

# aggregated var: # of females in sales and office jobs
agg_data6 <- childcare_costs %>% select(c(1,47)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(femp_sales))

# aggregated var: # of males in natural resources, construction, and maintenance jobs
agg_data7 <- childcare_costs %>% select(c(1,49)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(memp_n))

# aggregated var: # of females in natural resources, construction, and maintenance jobs
agg_data8 <- childcare_costs %>% select(c(1,50)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(femp_n))

# aggregated var: # of males in production, transportation, and moving jobs
agg_data9 <- childcare_costs %>% select(c(1,52)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(memp_p))

# aggregated var: # of females in production, transportation, and moving jobs
agg_data10 <- childcare_costs %>% select(c(1,53)) %>% group_by(county_fips_code) %>% summarise(Percentage = mean(femp_p))

# join county code data
counties_slim <- counties %>% select(c(1,2))
# remove the word county to match with data df and change to lowecase
counties_slim <- counties_slim %>% 
  mutate(county = str_remove_all(county_name, ' County')) %>% 
  mutate(county = tolower(county)) %>% select(c(1,3))
# add county code to data
colnames(counties_slim)[2] <- 'subregion'
data_code <- left_join(data, counties_slim, by='subregion')
# join map data to agg_data to have a look at data
agg_data1_county_map <- left_join(data_code, agg_data1, by = 'county_fips_code')
agg_data2_county_map <- left_join(data_code, agg_data2, by = 'county_fips_code')
agg_data3_county_map <- left_join(data_code, agg_data3, by = 'county_fips_code')
agg_data4_county_map <- left_join(data_code, agg_data4, by = 'county_fips_code')
agg_data5_county_map <- left_join(data_code, agg_data5, by = 'county_fips_code')
agg_data6_county_map <- left_join(data_code, agg_data6, by = 'county_fips_code')
agg_data7_county_map <- left_join(data_code, agg_data7, by = 'county_fips_code')
agg_data8_county_map <- left_join(data_code, agg_data8, by = 'county_fips_code')
agg_data9_county_map <- left_join(data_code, agg_data9, by = 'county_fips_code')
agg_data10_county_map <- left_join(data_code, agg_data10, by = 'county_fips_code')





# plot dfs 1-10. names: m, se, sa, n, p for jobs and f/m for gender
mm <- ggplot() +
  geom_polygon(data=agg_data1_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% male civilians in management, business, science, and arts jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
mm

mf <- ggplot() +
  geom_polygon(data=agg_data2_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% female civilians in management, business, science, and arts jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
mf

sem <- ggplot() +
  geom_polygon(data=agg_data3_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% male civilians in service jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
sem

sef <- ggplot() +
  geom_polygon(data=agg_data4_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% female civilians in service jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
sef

sam <- ggplot() +
  geom_polygon(data=agg_data5_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% male civilians in sales and office jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
sam

saf <- ggplot() +
  geom_polygon(data=agg_data6_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% female civilians in sales and office jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
saf

nm <- ggplot() +
  geom_polygon(data=agg_data7_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% male civilians in natural resources, construction, and maintenance jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
nm

nf <- ggplot() +
  geom_polygon(data=agg_data8_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% female civilians in natural resources, construction, and maintenance jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
nf

pm <- ggplot() +
  geom_polygon(data=agg_data9_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% male civilians in production, transportation, and moving jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
pm

pf <- ggplot() +
  geom_polygon(data=agg_data10_county_map, aes(x=long, y=lat, group=group, fill=Percentage)) +
  scale_fill_gradient(low='#56B1F7', high='#132B43', trans='log10') +
  coord_fixed(1.3) +
  theme_void() +
  ggtitle('% female civilians in production, transportation, and moving jobs') +
  theme(plot.title = element_text(size=10, hjust=.5))
pf


# combine plots into 1 large plot m, se, sa, n, p
legend <- get_legend(
  # create some space to the left of the legend
  pf + theme(legend.box.margin = margin(0, 0, 0, 2))
)
p1 <- plot_grid(mm + theme(legend.position = 'none'), 
                  mf + theme(legend.position = 'none'),
                  sem + theme(legend.position = 'none'),
                  sef + theme(legend.position = 'none'),
                  sam + theme(legend.position = 'none'),
                  saf + theme(legend.position = 'none'),
                  nm + theme(legend.position = 'none'),
                  nf + theme(legend.position = 'none'),
                  pm + theme(legend.position = 'none'),
                  pf + theme(legend.position = 'none'),
          ncol=2, nrow=5)

p2 <- plot_grid(p1, legend, rel_widths = c(3, .4))


plot_fin <- add_sub(p2, 'Data from National Database of Childcare Costs (gray=no data)', size = 9, x=0, hjust=0)
ggdraw(plot_fin)
