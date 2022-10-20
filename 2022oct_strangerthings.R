library(tidyverse)
library(tidytext)
library(stopwords)
library(ggfittext)

#episodes dataset
dat <- read_csv("episodes.csv")

#season/year
dat <- dat %>%  mutate(Season_Year = case_when(original_release_date == "2016-07-15" ~ "Season 1 (2016)",
                                               original_release_date == "2017-10-27" ~ "Season 2 (2017)",
                                               original_release_date == "2019-07-04" ~ "Season 3 (2019)",
                                               original_release_date == "2022-05-27" | original_release_date == "2022-07-01" ~ "Season 4 (2022)"))

#dialogue dataset
dat1 <- read_csv("stranger_things_all_dialogue.csv")
#one word per row
data <- dat1 %>% unnest_tokens(word, dialogue)
#have a look at stop words 
get_stopwords("en", "snowball") %>% print(n=Inf)
#remove stopwords and na
data_clean <- data %>% anti_join(get_stopwords("en", "smart")) %>% na.omit(word)
word_count <- data_clean %>% 
  group_by(season, episode) %>% 
  count(word, sort = TRUE)
#get counts for words
word_count_top <- word_count %>% 
  group_by(season, episode) %>%
  top_n(n=3)
#keep only top three per episode
word_count_top <- word_count_top %>% unite(group, season, episode) %>%
  group_by(group) %>%
  mutate(words = paste(word, collapse=", ")) %>%
  distinct(group, .keep_all = TRUE) %>%
  arrange(group)
#attach col
dat$words <- word_count_top$words

#plot
ggplot(dat, aes(factor(episode), Season_Year, fill = written_by, label = words)) +
  geom_tile(aes(height = .65),colour = "white") +
  scale_fill_discrete(name = "Episode Author") +
  #geom_text(aes(label = words)) 
  geom_fit_text(reflow = TRUE, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(size = 1)) +
  xlab("Episode") + ylab("Season (Year)") +
  labs(title = "Words with top 3 highest counts per Episode",
       subtitle = "Colours indicate episode authors",
       caption = "Data from 8flix.com - prepped by Dan Fellowes & Jonathan Kitt") +
  theme(plot.title = element_text(hjust = .5, size = 16),
        plot.subtitle = element_text(hjust = .5, size = 14),
        axis.title = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 11))
  
  