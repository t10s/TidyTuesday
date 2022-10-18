library(tidyverse)
library(wesanderson)

dat <- read_csv("episodes.csv")

#plot - x=episode,y=season,fill=directed or written
dat <- dat %>%  mutate(Season_Year = case_when(original_release_date == "2016-07-15" ~ "Season 1 (2016)",
                                               original_release_date == "2017-10-27" ~ "Season 2 (2017)",
                                               original_release_date == "2019-07-04" ~ "Season 3 (2019)",
                                               original_release_date == "2022-05-27" | original_release_date == "2022-07-01" ~ "Season 4 (2022)"))

ggplot(dat, aes(factor(episode), Season_Year, fill = written_by)) +
  geom_tile(colour="white", width = 1, height = .4) +
  scale_fill_discrete() +
  theme_minimal() +
  theme(axis.line = element_line(size=.7)) +
  xlab("Episode") + ylab("Season (Year)") +
  ggtitle("Stranger Things Episode Authors (Season 1-4)") +
  theme(plot.title = element_text(hjust = .5))