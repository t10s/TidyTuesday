#plot for goal difference for home and away games from 1993-Nov 2022 for Arsenal, Liverpool, Torrenham, Everton, Man City, & Man Utd
#dataset: https://www.kaggle.com/datasets/irkaal/english-premier-league-results

library(tidyverse)


data <- read_csv("eplmatches.csv")
#select clubs
names <- c("Arsenal", "Everton", "Liverpool", "Manchester Utd", "Manchester City", "Tottenham")
home_Set <- filter(data, Home %in% names)
away_set <- filter(data, Away %in% names)
#compute goal diff
home_Set$diff <- home_Set$HomeGoals - home_Set$AwayGoals
away_set$diff <- away_set$AwayGoals - away_set$HomeGoals
#add var for home or away and team name
home_Set$type <- "home"
home_Set$team <- home_Set$Home
away_set$type <- "away"
away_set$team <- away_set$Away
#merge
compl_set <- rbind(home_Set, away_set)
compl_set <- compl_set %>% mutate(Year = case_when(
  Season_End_Year <= 2002 ~ "1993-2002",
  Season_End_Year >= 2003 & Season_End_Year <= 2012 ~ "2003-2012",
  Season_End_Year >= 2013 ~ "2013-2022"))
compl_set <- compl_set %>% mutate(team = replace(team, team == "Manchester Utd", "M. Utd"))
lvl_order <- c("home", "away")

#plot
ggplot(compl_set, aes(x = team, y = diff)) +
  geom_boxplot(aes(shape = factor(type, level = lvl_order)), outlier.alpha = 0, show.legend = FALSE) +
  geom_point(aes(col = factor(type, level = lvl_order)), alpha = .3, position = position_jitterdodge(jitter.width = .3, jitter.height = 1)) +
  theme_minimal() +
  facet_wrap(~Year) +
  theme(axis.line = element_line(size = .5, color = "black", linetype = 1), legend.position = "right") +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14)) +
  theme(panel.spacing.x = unit(1.3,"line")) +
  ylab("Goal Difference") + xlab("Club")+
  labs(col='Legend') +
  theme(strip.text.x = element_text(size = 15)) +
  ggtitle("Goal Difference for Home and Away games from 1993 - Nov. 2022") +
  theme(plot.title = element_text(hjust = .5))

