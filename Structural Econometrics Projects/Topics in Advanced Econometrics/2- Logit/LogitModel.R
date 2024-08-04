library(tidyverse)
library(mlogit)
library(readr)

data_multi <- read_csv('C:/Users/moham/My Drive/Projects/Structural Econometrics Projects/Topics in Advanced Econometrics/2- Logit/commute_multinomial.csv')


data_df <- as.data.frame(data_multi)

data_dfidx <- dfidx(data_df, shape = 'wide', choice = 'mode', varying = 3:10)

model_2a <- mlogit(formula = mode ~ cost | 1 | time,
                   data = data_dfidx)

data_df_counter <- data_df %>%
  mutate(time.bus = 0.8 * time.bus)
## Convert counterfactual data to dfidx format
data_counter_dfidx <- dfidx(data_df_counter, shape = 'wide',
                            choice = 'mode', varying = 3:10)


agg_choices_obs <- predict(model_2a, newdata = data_dfidx)
## Calculate aggregate choices using counterfactual data
agg_choices_counter <- predict(model_2a, newdata = data_counter_dfidx)
## Calculate difference between aggregate choices
colSums(agg_choices_counter - agg_choices_obs)

## Calculate log-sum values using observed data
logsum_obs <- logsum(model_2a, data = data_dfidx)
## Calculate log-sum values using counterfactual data
logsum_counter <- logsum(model_2a, data = data_counter_dfidx)
## Calculate change in consumer surplus from subsidy
sum((logsum_counter - logsum_obs) / -coef(model_2a)[4])

