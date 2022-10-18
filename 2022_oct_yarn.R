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

#set level order for colour
lvl_order <- c("<=117", "117:279", "280:312", ">=313")
  


#plot ggridges
ggplot(data_means_, aes(rating_average_1, Yardage, fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE) +
  scale_fill_viridis_c(name = "Average Rating", option = "C") +
  xlab("Ratings Rating") + ylab("Yardage") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 1)) +
  ggtitle("Ratings for Different Yardages") +
  theme(plot.title = element_text(hjust = .5))

