#Installing missing packages
    
    list.of.packages <- c("ggplot2", "Rcpp","tidyverse","magrittr","dplyr","bayesplot","rstanarm","broom","MASS","corrplot")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    
#Loading required Libraries
    
    library(tidyverse)
    library(magrittr)
    library(dplyr)
    library(bayesplot)
    library(rstanarm)
    library(broom)
    library(MASS)
    library(corrplot)
    library(betareg)
    library(loo)
    library(rstanarm)
    
    
    options(mc.cores = parallel::detectCores())
    ECData = read_rds("ECData.rds")
    
#Subsetting the required data for modelling
    beta_data = ECData[,c(1:5,10,15,17:18)]
    
#Renaming the column values as 0,1 for modelling purpose
    beta_data$author_hidden = ifelse(beta_data$author_hidden == 't',0,1)
    beta_data$has_video = ifelse(beta_data$has_video == 't',0,1)
    beta_data$comments_enabled = ifelse(beta_data$comments_enabled == 't',0,1)
    beta_data$has_link = ifelse(beta_data$has_link == TRUE,0,1)
    
#Transposing the data 
    beta_data = t(beta_data)
    colnames(beta_data)  = c(paste0("X",1:133),"label")
    beta_data = as.data.frame(beta_data)

#Checking data types 
  sapply(beta_data,mode)
  
#Subsetting the data for only 10 models
  
  beta_10 = beta_data[,1:10]
  
  
  #Permutation
  
  comb_data = expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))
  colnames(comb_data) = c('has_link','author_hidden','has_video','comments_enabled')  
  comb_data = data.frame(cbind(ECData[2:17,1:5]),comb_data)
  t_comb_data = t(comb_data)
  colnames(t_comb_data)  = c(paste0("X",1:16))
  
  
#Modelling 
  
  ####Trying out a model
  
  ######Thompson Sampling
  
  data_trial = t_comb_data[4:7,]
  N = 4
  d = 16
  ads_selected = integer(0)
  total_reward = 0
  for (n in 1:N) {
    ad = sample(1:16, 1)
    ads_selected = append(ads_selected, ad)
    reward = data_trial[n, ad]
    total_reward = total_reward + reward
  }

  total_reward
  
  hist(ads_selected,
       col = 'blue',
       main = 'Histogram of ads selections - RANDOM',
       xlab = 'Ads',
       ylab = 'Number of times each ad was selected',
       breaks= seq(0,17,by=1))
  
  axis(1, at=c(0,135), labels=c("",""), lwd.ticks=0)
  axis(1, at=seq(0,135, by=1), lwd=0, lwd.ticks=1)
  
  
  
  #Thompson Sampling
  

  ads_selected = integer(0)
  
  numbers_of_rewards_1 = integer(d) 
  numbers_of_rewards_0 = integer(d)
  # These two variables will be put in place in the for loops
  total_reward = 0
  for (n in 1:N) {
    ad = 0
    max_random = 0
    for (i in 1:d) {
      random_beta = rbeta(n = 1,
                          shape1 = numbers_of_rewards_1[i] + 1,
                          shape2 = numbers_of_rewards_0[i] + 1)
      if (random_beta > max_random) {
        max_random = random_beta
        ad = i
      }
    }
    ads_selected = append(ads_selected, ad)
    reward = data_trial[n, ad]
    if (reward == 1) {
      numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
    } else {
      numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
    }
    total_reward = total_reward + reward
  }
  
  total_reward
  
  
  
  hist(ads_selected,
       col = 'blue',
       main = 'Histogram of ads selections (Thompson Sampling R)',
       xlab = 'Ads',
       ylab = 'Number of times each ad was selected',
       breaks= seq(0,17,by=1))
  
  axis(1, at=c(0,17), labels=c("",""), lwd.ticks=0)
  axis(1, at=seq(0,17, by=1), lwd=0, lwd.ticks=1)

  
  #This trial does not make sense because you are studying the outcome of the parameters for each message type. 
  #You should be considering the results from the users whether or not they opened the message.
  #Based on this concept,build a model and check the reweards.
  
  
  
  #Function to Calculate beta parameters
  
  
  Beta_Parameters <- function(data) {
    
    mean = mean(data)
    variance = var(data)
    alpha <- mean * varaince
    beta <- variance * (1 - mean)  
    return(Beta_Parameters = list(alpha = alpha, beta = beta))
  }
  
  
  #For the 16 Message types get the beta parameters.
  
  
  
  
  

  
  #Modelling 
  
  
  #####Stan Application
  
  
  fit1 = stan_betareg(comb_data$openrate ~ has_link + author_hidden + has_video + comments_enabled ,data = comb_data,link = "logit",seed = 12345)
  round(coef(fit1),2)
  prior_summary(fit1)
  
  
  
  