library(tidyverse)
library(gmm)

### Create functions for use with maximum likelihood
## Function to summarize MLE model results
summarize_mle <- function(model, names){
  ## Extract model parameter estimates
  parameters <- model$par
  ## Calculate parameters standard errors
  std_errors <- model$hessian %>%
    solve() %>%
    diag() %>%
    sqrt()
  ## Calculate parameter z-stats
  z_stats <- parameters / std_errors
  ## Calculate parameter p-values
  p_values <- 2 * pnorm(-abs(z_stats))
  ## Summarize results in a list
  model_summary <- tibble(names = names,
                          parameters = parameters,
                          std_errors = std_errors,
                          z_stats = z_stats,
                          p_values = p_values)
  ## Return model_summary object
  return(model_summary)
}

## Function to conduct likelihood ratio test
test_likelihood_ratio <- function(model_rest, model_unrest){
  ## Calculate likelihood ratio test statistic
  test_stat <- 2 * (model_rest$value - model_unrest$value)
  ## Calculate the number of restrictions
  df <- length(model_unrest$par) - length(model_rest$par)
  ## Test if likelihood ratio test statistic is greater than critical value
  test <- test_stat > qchisq(0.95, df)
  ## Calculate p-value of test
  p_value <- 1 - pchisq(test_stat, df)
  ## Return test result and p-value
  return(list(reject = test, p_value = p_value))
}

data_multi <- read_csv('C:/Users/moham/My Drive/Projects/Structural Econometrics Projects/Topics in Advanced Econometrics/2- Logit/commute_multinomial.csv')
## Function to calculate log-likelihood for heating choice
ll_fn_1a <- function(params, data){
  ## Extract individual parameters with descriptive names
  beta_1 <- params[1]
  beta_2 <- params[2]
  ## Calculate representative utility for each alternative given the parameters
  model_data <- data %>%
    mutate(utility_bike = beta_1 * cost.bike + beta_2 * time.bike,
           utility_bus = beta_1 * cost.bus + beta_2 * time.bus,
           utility_car = beta_1 * cost.car + beta_2 * time.car,
           utility_walk = beta_1 * cost.walk + beta_2 * time.walk)
  ## Calculate logit choice probability denominator given the parameters
  model_data <- model_data %>%
    mutate(prob_denom = exp(utility_bike) + exp(utility_bus) +
             exp(utility_car) + exp(utility_walk))
  ## Calculate logit choice probability for each alt given the parameters
  model_data <- model_data %>%
    mutate(prob_bike = exp(utility_bike) / prob_denom,
           prob_bus = exp(utility_bus) / prob_denom,
           prob_car = exp(utility_car) / prob_denom,
           prob_walk = exp(utility_walk) / prob_denom)
  ## Calculate logit choice probability for chosen alt given the parameters
  
  model_data <- model_data %>%
    mutate(prob_choice = prob_bike * (mode == 'bike') +
             prob_bus * (mode == 'bus') + prob_car * (mode == 'car') +
             prob_walk * (mode == 'walk'))
  ## Calculate log of logit choice probability for chosen alt given the params
  model_data <- model_data %>%
    mutate(log_prob = log(prob_choice))
  ## Calculate the log-likelihood for these parameters
  ll <- sum(model_data$log_prob)
  return(-ll)
}

model_1a <- optim(par = rep(0, 2), fn = ll_fn_1a, data = datmodel$hessiana_multi,
                  method = 'BFGS', hessian = TRUE)
## Summarize model results
model_1a %>%
  summarize_mle(c('cost', 'time'))


