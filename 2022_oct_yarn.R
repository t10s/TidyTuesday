setwd("C:/Users/tarin/Documents/TidyTuesday/Datasets/2022oct")

library(tidyverse)
library(ggridges)

dat <- read.csv("yarn.csv")

#count number of yarns per company and filter those below 150
counts <- dat %>% select(yarn_company_name) %>%
  group_by(yarn_company_name) %>%
  summarize(count=n()) %>%
  filter(count >= 150)
names <- c(unique(counts$yarn_company_name))

#prep data to compute yardage by rating
data <- dat %>% select(rating_average, yarn_weight_name, yarn_company_name, yardage) %>%
  filter(yarn_company_name %in% names) %>%
  na.omit()
#summarize
data_means <- data %>% group_by(yarn_company_name, yarn_weight_name) %>%
  summarize(across(everything(), list(mean)))
#add grouping for yardage
data_means_ <- data_means %>% mutate(group=case_when(yardage_1 <= 117 ~ "<=117",
                                      yardage_1 >= 118 & yardage_1 <= 279 ~ "117:279",
                                      yardage_1 >= 280 & yardage_1 <= 312 ~ "280:312",
                                      yardage_1 >= 313 ~ ">=313")) %>%
  na.omit()
colnames(data_means_)[5] <- "Yardage"
#roundabout way to selectively label
data_means_ <- data_means_ %>% mutate(group2=case_when(rating_average_1 >= 4.5 | rating_average_1 <= 3 ~ "lab",
                                                      rating_average_1 <= 4.44 | rating_average_1 >= 3.1 ~ "nolab"))
data_means_ <- data_means_ %>% mutate(CompanyName=case_when(group2 == "lab" ~ yarn_company_name,
                                                            group2 == "nolab" ~ " "))
#set level order for colour
lvl_order <- c("<=117", "117:279", "280:312", ">=313")

#plot
ggplot(data_means_, aes(x = yarn_weight_name, y = rating_average_1)) +
  geom_point(aes(colour = Yardage), alpha = .5, position = position_jitter(seed=1, width = .2)) +
  #guides(colour=guide_legend(title=" Yardage Legend")) +
  geom_text(data=subset(data_means_, group2 == "lab"), aes(x = yarn_weight_name, y = rating_average_1, label=CompanyName),
            size = 2.5, nudge_x = .05, nudge_y = .05, check_overlap = TRUE, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1)) +
  coord_cartesian(ylim = c(2, 5)) + 
  xlab("Yarn Weight Name") + ylab("Mean Rating") +
  ggtitle("Mean Ratings for yarn companies with >150 products by yarn weight name grouped by yardage") +
  labs(subtitle = "Only Companies with rating above 4.5 or below 3 are labelled") +
  theme(plot.title = element_text(hjust=.5, vjust = 2.5, size = 15),
        plot.subtitle = element_text(hjust =.5, size = 13)) 
  


#plot ggridges
ggplot(data_means_, aes(rating_average_1, Yardage, fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE) +
  scale_fill_viridis_c(name = "Average Rating", option = "C") +
  xlab("Ratings Rating") + ylab("Yardage") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1)) +
  ggtitle("Ratings for Different Yardages") +
  theme(plot.title = element_text(hjust = .5))

