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
### Figure 4 data
# Source:  "Fig4 Pre-Post Scores" tab (columns A-G) 
Figure4data <- read_csv("Fig4_Pre_Post_Assessment_Instrument_Dataset.csv")

# make longer format for easy plotting
Figure4data_long <- Figure4data %>% 
  pivot_longer(cols = everything(), names_to = "Course", values_to = "Gain") %>% 
  na.omit() %>%
  mutate(Course = factor(Course,
                         levels = c("Gen Bio-RI",
                                    "Bioinfo & Comp Bio-RI",
                                    "Mol Bio-RI",
                                    "Virology-RI",
                                    "Gen Bio-PUI",
                                    "Mol Biotech-PUI",
                                    "Dev Bio-PUI"),
                         labels = c("Gen Bio-RI",
                                    "Bioinfo & Comp Bio-RI",
                                    "Mol Bio-RI",
                                    "Virology-RI",
                                    "Gen Bio-PUI",
                                    "Mol Biotech-PUI",
                                    "Dev Bio-PUI"),
                         ordered = TRUE))

# reconstruct current plot (less boxplot)
Figure4data_long %>% 
  ggplot(aes(x=Course, y=Gain)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.8, method = "histodot", binwidth = 0.5,
               fill = "white", alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_normal", mapping = aes(group = Course), 
               color = "black", geom = "errorbar", width = 0.1) +
  stat_summary(fun.data = "mean_cl_normal", mapping = aes(group = Course), 
               color = "black", geom = "point", shape = 3, size = 1, stroke = 1.2) +
  # stat_summary(fun.data = "mean_cl_normal", mapping = aes(group = Course), 
  #              color = "black", geom = "pointrange", fatten = 1) +
  geom_text(data = count(Figure4data_long, Course), 
            aes(label=n, y=min(Figure4data_long$Gain)-1), 
            show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, 
       y = "Learning Gains (Post - Pre Score)") +
  # theme_light(base_size = 15) +
  theme_classic(base_size = 15) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x=element_text(angle=15, vjust = 0.8)) 

