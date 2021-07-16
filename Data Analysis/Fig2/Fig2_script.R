# William R. Morgan, College of Wooster
# created: 2020-10-19
# updated: 2021-01-02

# Script for dot plots with 95% CI for Kleinschmit manuscipt 

#################################################################
### The usual preliminary steps
# make these packages and associated functions available to use in this script
library(tidyverse)
# clear the Global Environment
rm(list = ls()) 

#################################################################
### Figure 3 data
# Source: Cells A1-I374 of the the "Pre-Post Scores" worksheet
# Figure2data <- read_csv("Fig3_Pre_Post_Assessment_Instrument_Dataset.csv", n_max = 373)
Figure3data <- read_csv("Fig3_Pre_Post_Assessment_Instrument_Dataset.csv", n_max = 373)

# add column with flag for either quiz time <240 seconds
# Figure3data <- Figure2data %>% 
#   mutate(Sincere = `Pre Duration (seconds)` > 240 & `Post Duration (seconds)` > 240)

# make longer format for easy plotting
Figure3data_long <- Figure3data %>% 
  pivot_longer(cols = c(Pre, Post), names_to = "When", values_to = "Score") %>% 
  mutate(When = factor(When, 
                       levels = c("Pre", "Post"),
                       labels = c("Pre-Quiz", "Post-Quiz"),
                       ordered = TRUE))

# dotplot with error bars and mean
Figure3data_long %>% 
  ggplot(aes(x=When, y=Score)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.8, method = "histodot", binwidth = 0.5,
               fill = "white", alpha = 0.3, show.legend = FALSE) +
  stat_summary(fun.data = "mean_cl_normal", mapping = aes(group = When), 
               color = "black", geom = "errorbar", width= 0.05) +
               # color = "red", geom = "crossbar", width= 0.1, fatten = .5, stroke = 1.2) +
               # color = "red", geom = "crossbar", width= 0.2) +
  stat_summary(fun.data = "mean_cl_normal", mapping = aes(group = When), 
               color = "black", geom = "point", shape = 3, size = 1, stroke = 1.2) +
  geom_text(data = count(Figure3data_long, When), 
            aes(label=n, y=min(Figure3data_long$Score, na.rm = TRUE)-1), 
            show.legend = FALSE) +
  labs(x = NULL, 
       y = "Assessment Score") +
  # scale_fill_grey(na.value = "red") +
  theme_classic(base_size = 13) +
  # theme_light(base_size = 15) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = rel(1.0)))
