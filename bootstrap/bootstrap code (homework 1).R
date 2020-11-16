library(dplyr)
library(broom)
library(formattable)

directory <- paste0(here::here(), "/bootstrap")
setwd(directory)

load("income_credit_card_data.RData")

income_credit_card_data <- income_credit_card_data %>%
  mutate(age_sq = age ^ 2)

bootstrap <- function(x, M) {
  
  theta <- c(1:M)
 
  for (j in 1:M){
    data <- sample_n(x, size = nrow(x), replace = TRUE) 
    reg <- lm(data$spending ~ data$age + data$age_sq + data$incper)
    reg %>%
      summary() %>% 
      coef() %>% 
      .[2, 1] -> beta_1
    reg %>%
      summary() %>% 
      coef() %>% 
      .[3, 1] -> beta_2
    theta[j] <- -0.5 * beta_1 / beta_2
  }
  
  theta_var <- var(theta)
  
  return(cat(paste0("The variance of theta is: ", theta_var)))
}

bootstrap(x = income_credit_card_data, M = 100)





