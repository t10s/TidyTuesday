library(tidyverse)
library(ggrepel)
library(gganimate)

data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
data_names <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


agg <- data %>%
  separate(date, c('year', 'month', 'day'), sep='-') %>%
  filter(year != 2023) %>%
  group_by(stock_symbol, year) %>%
  summarise(mean_open = mean(open),
            mean_high = mean(high),
            mean_low = mean(low),
            mean_close = mean(close),
            mean_adj_close = mean(adj_close),
            mean_vol = mean(volume)) %>%
  mutate(mean_vol_log = log(mean_vol, base=100),
         year = as.numeric(year))

p <- ggplot(agg, aes(mean_vol_log, mean_high, col=stock_symbol, label = stock_symbol)) +
  geom_point(aes(size=mean_vol), show.legend = F) +
  #geom_label_repel(aes(label=stock_symbol), show.legend = F) +
  geom_text(vjust=-1, show.legend = F) +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = .5),
        plot.title = element_text(size=14, hjust =.5),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10)) +
  xlab('Mean Volume traded (log100)') + ylab('Mean highest price') +
  transition_time(year) +
  ease_aes('linear', interval = .001) +
  labs(title = 'Mean traded volume and highest stock price by year: {round(frame_time,0)}') 

animate(p, nframes=200, fps=20, end_pause=30, width=600, height=400)

anim_save('2023_01_07_plot.gif', p)
