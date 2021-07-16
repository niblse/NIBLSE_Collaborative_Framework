---
  title: "Difference GLM"
output:
  pdf_document: default
html_document: default
word_document: default
---
  
  ```{r}
GLM1 <- glm (
  Difference ~  Level+Institution+Topic,
  data = NIBLSEdata)
summary(GLM1)
```

```{r}
GLM2 <- glm (
  Difference ~  Institution + Topic,
  data = NIBLSEdata)
summary(GLM2)
```

```{r}
GLM3 <- glm (
  Difference ~  Level+Topic,
  data = NIBLSEdata)
summary(GLM3)
```

```{r}
GLM4 <- glm (
  Difference ~  Institution+Level,
  data = NIBLSEdata)
summary(GLM4)
```

```{r}
GLM5 <- glm (
  Difference ~  Topic,
  data = NIBLSEdata)
summary(GLM5)
```
```{r}
GLM6 <- glm (
  Difference ~  Institution,
  data = NIBLSEdata)
summary(GLM6)
```

```{r}
GLM7 <- glm (
  Difference ~  Level,
  data = NIBLSEdata)
summary(GLM7)
```

```{r}
plot(GLM1)
#Deviances
anova(GLM1,GLM2,GLM3,GLM4,GLM5,GLM6,GLM7)
# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
```

```{r}
library(tidyverse)
DifferencebyInstitution <- NIBLSEdata %>%
  group_by(Institution) %>%
  summarise(mean_Difference = mean(Difference),  # calculates the mean of each group
            sd_Difference = sd(Difference), # calculates the standard deviation of each group
            n_Difference = n(),  # calculates the sample size per group
            SE_Difference = sd(Difference)/sqrt(n()))  # calculates the standard error of each group
```

```{r}
DifferencebyLevel <- NIBLSEdata %>%
  group_by(Level) %>%
  summarise(mean_Difference = mean(Difference),  # calculates the mean of each group
            sd_Difference = sd(Difference), # calculates the standard deviation of each group
            n_Difference = n(),  # calculates the sample size per group
            SE_Difference = sd(Difference)/sqrt(n()))  # calculates the standard error of each group
```

```{r}
DifferencebyTopic <- NIBLSEdata %>%
  group_by(Topic) %>%
  summarise(mean_Difference = mean(Difference),  # calculates the mean of each group
            sd_Difference = sd(Difference), # calculates the standard deviation of each group
            n_Difference = n(),  # calculates the sample size per group
            SE_Difference = sd(Difference)/sqrt(n()))  # calculates the standard error of each group
```

Filtered Analysis
```{r}
GLM1f <- glm (
  Difference ~  Level+Institution+Topic,
  data = NIBLSEfilter)
summary(GLM1f)
```



```{r}
GLM2f <- glm (
  Difference ~  Institution + Topic,
  data = NIBLSEfilter)
summary(GLM2f)
```

```{r}
GLM3f <- glm (
  Difference ~  Level+Topic,
  data = NIBLSEfilter)
summary(GLM3f)
```

```{r}
GLM4f <- glm (
  Difference ~  Institution+Level,
  data = NIBLSEfilter)
summary(GLM4f)
```
