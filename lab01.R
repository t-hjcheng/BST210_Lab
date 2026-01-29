# Question 1 --------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(ggplot2)
d <- read.csv("data/lab01 dataset/ebchf3.csv") 
# specify local filepath, in quotations

# Question 2 --------------------------------------------------------------
#View the first few rows of the data.
head(d)
dim(d)    #Take a loot at the total amount of id and variables
d$csmok

# Question 3 -------------------------------------------------------------
table(d$csmok)

d %>% 
  group_by(csmok) %>%
  summarise(n=n()) %>% 
  ungroup()

?summarise    #Use ? to access Help
# If you want to save it into a new dataset 
d_new <- d %>% 
  group_by(csmok) %>%
  summarise(n=n()) %>% 
  ungroup()


d %>% 
  group_by(csmok) %>%
  summarise(n=n(),
            pct = n()/nrow(d)) %>%
  ungroup()

table(d$csmok)/nrow(d)

# Question 4 --------------------------------------------------------------
#Create a subset of the data with only smokers
d.smok <- d %>% filter(csmok==1)

# Question 5 --------------------------------------------------------------
#Perform a two-sample t-test (unequal variances) evaluating whether the mean SBP differs by current smoking status
d.nonsmok <- d %>% filter(csmok==0)
t.test(d.smok$sbp, d.nonsmok$sbp)

# Question 6 --------------------------------------------------------------
#Fit a linear regression evaluating whether the mean SBP differs by smoking status. Write down the model that you are fitting. Interpret the estimated regression coefficients. According to the model, what is the average SBP among current smokers?
fit.csmok <- lm(sbp ~ csmok, data = d)
summary(fit.csmok)

# Question 7 -------------------------------------------------------------



# Question 8 --------------------------------------------------------------




# Question 9 --------------------------------------------------------------






# Question 10 -------------------------------------------------------------






# Question 11 -------------------------------------------------------------





# Question 12 -------------------------------------------------------------








