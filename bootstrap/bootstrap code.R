
rm(list=ls())  #removes all variables
options(scipen=8)   #Sets scientific notation 

library(dplyr)
library(broom)

directory <- paste0(here::here(), "/bootstrap")
setwd(directory)

load("income_credit_card_data.RData")
attach(income_credit_card_data)
#Data is from Greene (1992).  (pages.stern.nyu.edu/~wgreene/Text/Edition7/tablelist8new.htm)
#variable names:  
#age is age
#income is monthly income 
#incper is Income divided by number of dependents
#spending is Average monthly credit card expenditure (for Cardholders) 


#what is the sample size?
n <- length(income)


#regress spending on age and income
reg1 <- lm(spending ~ age + income) %>% tidy()
#extract the intercept on age 
beta_age <- reg1[2, 2] %>% pull()
#extract the standard error of beta_age
se_beta_age <- reg1[2, 3] %>% pull()
#square it to get the variance
var_beta_age <- se_beta_age^2


#so R has calculated the se of beta_age for me.
#But what if I wanted to calculate it using the bootstrap?
#For starters how do I draw random samples in R?
head(income_credit_card_data, n = 10)  #print first 10 observations
new_sample_index0 <- sample(1:10, size = 10, replace = TRUE) #Creates a sample of 10 indices to use to draw observations from.  Basically, this is a random sample of 10 integers between 1 and 10.   
new_sample_index0
new_age_sample0 <- age[new_sample_index0]  #Selects new observations from the sample.  If, say, the first number in new_sample_index0=5, then I want to select the 5th observation of age as my first new observation of age.
new_age_sample0   #new sample for age (drawn randomly with replacement from the first 10 observations)  


#now do this for the whole sample for SPENDING, AGE, and INCOME.
new_sample_index00 <- sample(1:n, size = n, replace = TRUE) #Creates a sample of N indices to use to draw observations from
new_age_sample00 <- age[new_sample_index00]
new_spending_sample00 <- spending[new_sample_index00]
new_income_sample00 <- income[new_sample_index00]
#now run a regression with the new sample
new_reg00 <- lm(new_spending_sample00 ~ new_age_sample00 + new_income_sample00) %>% tidy()
#extract the intercept on age 
new_beta_age00 <- new_reg00[2, 2] %>% pull()
new_beta_age00  #And now I have a NEW beta for age.


#But I want to do this whole process M times!
#To do that, I need a loop!  
M <- 10000      #M is how many new samples I want resampled from the data
#create matrices for the new variables that are simply full of zeros
new_age_samples <- array(0, c(n,M))  #new age matrix of N rows (for each observation) and M column (for each new sample)
new_income_samples <- array(0, c(n,M))  #new income matrix of N rows (for each observation) and M column (for each new sample)
new_spending_samples <- array(0, c(n,M))  #new spending matrix of N rows (for each observation) and M column (for each new sample)
#create a matrix for the new sampling index 
new_sample_indices <- array(0, c(n,M))  
#I needed to create those variables before I run the loop.
for (j in 1:M){
  new_sample_indices[, j] <- sample(1:n, size=n, replace = TRUE) #Creates M samples of N indices to use to draw observations from
  new_age_samples[, j] <- age[new_sample_indices[, j]]
  new_income_samples[, j] <- income[new_sample_indices[, j]]
  new_spending_samples[, j] <- spending[new_sample_indices[, j]]
}
#Each column of the new age, income, and spending variables is a new sample, which was resampled from the data
#Now that I have M samples, I can run the regression M times, obtain M estimates of the age beta, and use that to calculate its variance 
#create a vector for the NEW betas for age
new_betas_age <- array(0,c(M))
for (j in 1:M){
  #regress the jth sample of spending on the jth sample of age and the jth sample of income
  new_regs <- lm(new_spending_samples[, j] ~ new_age_samples[, j] + new_income_samples[, j]) %>% tidy() 
  #Note, I don't need to save everything from each of the M regressions. I only need to save the betas on each regression.  Thus the new_reg=lm() command does not have an index attached to it.  That way it simply gets replaced with each iteration of the loop. 
  new_betas_age[j] <- new_regs[2, 2] %>% pull() #save the new betas for age
}
#Now I have M betas for age calculated from M new samples of the data.
#With that I will calculate the variance of the beta for age.
var_beta_age_boot=var(new_betas_age)  


#Print the variance of the beta for age using the bootstrap.
var_beta_age_boot
#Print the variance of the beta for age that R calculated automatically (saved from earlier in the code).
var_beta_age
#Notice the two variances are "close", but not the same.  This is because this bootstrap technique is a nonparametric process that relies on arguably fewer/different assumptions.  


##########################################################
#Okay, so the variance of beta that R calculated automatically is similar to the variance of beta calculated from the bootstrap.
#However, the real benefit of the bootstrap is calculating the variance of complicated transformations of the parameter estimates.  

 
#Consider regressing income on age and age squared.
age_squared <- age ^ 2   #Create age squared variable 
reg2 <- lm(income ~ age + age_squared) %>% tidy()

#At what age is income maximized?
#A little calculus tells us that income is maximized when age=-beta1/(2*beta2)
#So what we care about is a transformation of the parameters, theta=-beta1/(2*beta2)
#If I know estimates of beta1 and beta2 (beta1_hat and beta2_hat), then calculating an estimate of theta (theta_hat) is rather straightforward.
beta1_hat <- reg2[2, 2] %>% pull()
beta2_hat <- reg2[3, 2] %>% pull() 
theta_hat = -beta1_hat / (2 * beta2_hat)
theta_hat


#But how do I calculate the variance of theta_hat?   Answer: The bootstrap!!!


M=10000      #M is how many new samples I want resampled from the data
#create matrices for the new variables that are simply full of zeros
new_age_samples2 <- array(0, c(n,M))  #new age matrix of N rows (for each observation) and M column (for each new sample)
new_income_samples2 <- array(0, c(n,M))  #new income matrix of N rows (for each observation) and M column (for each new sample)
new_age_sqar_samples <- array(0, c(n,M))  #new squared age matrix of N rows (for each observation) and M column (for each new sample)
#create a matrix for the new sampling index 
new_sample_indices2 <- array(0, c(n,M))  
#I needed to create those variables before I run the loop.
for (j in 1:M){
  new_sample_indices2[, j] <- sample(1:n, size=n, replace = TRUE) #Creates M samples of N indices to use to draw observations from
  new_age_samples2[, j] <- age[new_sample_indices2[, j]]
  new_income_samples2[, j] <- income[new_sample_indices2[, j]]
  new_age_sqar_samples[, j] <- age_squared[new_sample_indices2[, j]]
}
#Each column of the new age, income, and age squared is a new sample, which was resampled from the data
#Now that I have M samples, I can run the regression M times, obtain M estimates of the theta=-beta1/(2*beta2), and use that to calculate its variance 
#create a vector for the new thetas hats (just full of zeros)
new_theta_hats <- array(0, c(M))
for (j in 1:M){
  #regress the jth sample of income on the jth sample of age and the jth sample of age squared
  new_regs2 <- lm(new_income_samples2[, j] ~ new_age_samples2[, j] + new_age_sqar_samples[, j]) %>% tidy()
  #Note, I don't need to save everything from each of the M regressions. I only need to save the theta_hats on each regression.  Thus the new_reg2=lm() command does not have an index attached to it.  That way it simply gets replaced with each iteration of the loop. 
  new_beta1_hat <- new_regs2[2, 2] %>% pull() 
  new_beta2_hat <- new_regs2[3, 2] %>% pull()
  new_theta_hats[j] = -new_beta1_hat / (2 * new_beta2_hat) #save the new theta hats
}
#Now I have M theta hats calculated from M new samples of the data.
#With that I will calculate the variance of theta hat
var_theta_hat <- var(new_theta_hats)
var_theta_hat  
#And there we have it: an estimate for the variance for theta hat.  This, of course, can be used to compute confidence intervals and other useful things.    




