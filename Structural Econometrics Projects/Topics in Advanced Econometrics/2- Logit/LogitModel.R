library(tidyverse)
library(mlogit)
library(readr)

data_multi <- read_csv('C:/Users/moham/My Drive/Projects/Structural Econometrics Projects/Topics in Advanced Econometrics/2- Logit/commute_multinomial.csv')


data_df <- as.data.frame(data_multi)

data_dfidx <- dfidx(data_df, shape = 'wide', choice = 'mode', varying = 3:10)

model_2a <- mlogit(formula = mode ~ cost | 1 | time,
                   data = data_dfidx)

summary(model_2a)