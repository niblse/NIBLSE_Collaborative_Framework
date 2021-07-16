# William R. Morgan, College of Wooster
# created: 2021-06-24

# One-sample t-tests for Kleinschmit data 
# Do all classes show significant learning gains (on average)?

#################################################################
### The usual preliminary steps
# make these packages and associated functions available to use in this script
library(tidyverse)
# clear the Global Environment
rm(list = ls()) 

#################################################################
### Figure 3 data
# Source:  "S19F20S20 Agg_by Instit" tab (columns AV-BB) 
Figure3data <- read_csv("Figure4_Post-Pre Learning Gains_1.1.21_update.csv")

# make longer format & simplify labels to avoid errrors
Figure3data_long3 <- Figure3data %>% 
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
                         labels = c("GenBioRI",
                                    "BioinfoCompBioRI",
                                    "MolBioRI",
                                    "VirologyRI",
                                    "GenBioPUI",
                                    "MolBiotechPUI",
                                    "DevBioPUI")))

###########################
# Conduct one-sample t-tests with p-value adjustment for multiple tests

library(rstatix)

onesample.tests <- Figure3data_long3 %>%
  group_by(Course) %>%
  t_test(Gain ~ 1, mu = 0) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

# present a clean table of results

library(knitr)

onesample.tests %>% kable()

# Conclusion: The average learning gains were significantly different from zero (adj. p-value < 0.001) for all courses.
