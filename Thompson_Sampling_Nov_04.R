###For Thompson Sampling go to line 50


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
library(ggplot2)

options(mc.cores = parallel::detectCores())
ECData = read_rds("ECData.rds")


######## Data Preparation #########


#Subsetting the required data for modelling
beta_data = ECData[,c(1:5,10,12:15,17:18)]

#Renaming the column values as 0,1 for modelling purpose
beta_data$author_hidden = ifelse(beta_data$author_hidden == 't',0,1)
beta_data$has_video = ifelse(beta_data$has_video == 't',0,1)
beta_data$comments_enabled = ifelse(beta_data$comments_enabled == 't',0,1)
beta_data$has_link = ifelse(beta_data$has_link == TRUE,0,1)

#Permutation
comb_data = expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))
colnames(comb_data) = c('has_link','author_hidden','has_video','comments_enabled')  
comb_data = data.frame(cbind(beta_data[2:17,c(1:5,7:9)],comb_data))
t_comb_data = t(comb_data)
colnames(t_comb_data)  = c(paste0("X",1:16))



###### Thompson Sampling

d = 16  #Number of arms
total_reward = 0  #would you calculate this and how will you use this?



#Modying John's Algorithm for Logistic


######   Attempt 1  

###### John's attempt at TS
# learning the underlying theta

set.seed(1234)
n = 1000              #Number of users
arm = numeric(n)      #Best arm for each user.

# Changing the parameter to be updated as the number of uniq_opens
#alpha = beta = numeric(d)
#alpha[1] = beta[1] = 1 # prior

#Taking the outcome to be uniq opens
y = numeric(n) # experiment outcome for each user

p = comb_data[,2]
nn = comb_data[,1]

theta = t_comb_data["openrate",]
#theta = c(0.99, rep(0.01,15))
thetahat = matrix(0, nrow = n, ncol = d)

view(comb_data)


for(ii in 1:n){
  
  fit.1 <- glm(cbind(p, nn - p) ~ polarity + punc_intensity + title_length ,  data = comb_data, family = binomial(link = logit))
  
  #Error : Error in family$linkfun(mustart) : Value 1.67458 out of range (0, 1)  
  #This error made me go crazyyyyy
  
  thetahat[ii,] = predict(fit.1,type = "response")
  arm[ii] = which.max(thetahat[ii,]) #choose an arm
  
  # perform the experiment using the "population" theta
  
  kk = arm[ii]
  y[ii] = rbinom(1,nn[kk],p = theta[arm[ii]]) #How many of the delivered messages will the ii user open?
  #comb_data[ii,2] = rbernoulli(1,p = theta[arm[ii]])  #Why not taking theta_hat here?
  #Keep a count of y for each arm somewhere
  
  p[kk] = p[kk] + y[ii]
  #This is exploration
  #What about exploitation How to deduct regret as in the case of beta parameter in beta regression?
  
}

print(thetahat[ii,])
#print(t(t_comb_data[6:9,arm]))

print(cbind(theta,thetahat[n,]))
#Take expected value print(cbind(theta,colSums(thetahat)/n))




####### Attempt 2

#Taking more data / all data from ecdata
#Model data

type = sample(1:16,134,replace = T)
logit_data = data.frame(cbind(ECData[,c(1:5,12:14)],type))


set.seed(1234)
n = 1000
arm = numeric(n)

# Changing the parameter to be updated as the number of uniq_opens
#alpha = beta = numeric(d)
#alpha[1] = beta[1] = 1 # prior

#Taking the outcome to be uniq opens
y = numeric(n) # experiment outcome

theta = logit_data[,3]
#theta = c(0.99, rep(0.01,15))
thetahat = matrix(0, nrow = n, ncol = 134)


for(ii in 1:n){
  
  fit.1 <- glm(cbind(uniq_opens, delivered - uniq_opens) ~ polarity + punc_intensity + title_length , 
               data = logit_data, family = binomial(link = logit))
  
  
  #Error : Error in family$linkfun(mustart) : Value 1.67458 out of range (0, 1)  
  #This error made me go crazyyyyy
  
  thetahat[ii,] = predict(fit.1,type = "response")[[ii]]
  
  arm[ii] = which.max(thetahat[ii,]) # choose an arm
  
  # perform the experiment using the "population" theta
  
  logit_data[ii,2] = rbinom(1,logit_data[ii,1],p = theta[arm[ii]])
  #comb_data[ii,2] = rbernoulli(1,p = theta[arm[ii]])  #Why not taking theta_hat here?
  
  #Keep a count of y for each arm somewhere
  
  kk = arm[ii]
  logit_data[kk,2] = logit_data[kk,2] + logit_data[ii,2]
  
}

print(thetahat[ii,])
print(cbind(theta,thetahat[n,]))
#Take expected value print(cbind(theta,colSums(thetahat)/n))



######### End John's stuff

#Calculating regret as the difference between maximum count of opened messages (or open rate) and the maximun count of opened messages obtained by        Thompson Sampling.
regret = max(t_comb_data[2,]) - t_comb_data[2,message]

#Calculate the reward as the count of messages opened (uniq_opens)
reward = t_comb_data[2, message]   
##Apply the theta_k i.e the random_beta on this message  
#Update this particular message for open_rate (alpha,beta) using the reward obtained above.

print(message)
print(reward)
print(regret)

numbers_of_rewards_1[message] = numbers_of_rewards_1[message] + reward
numbers_of_rewards_0[message] = numbers_of_rewards_0[message] - regret


print(numbers_of_rewards_0)
print(numbers_of_rewards_1)


#Trying to plot the beta distribution with the updated alphas and betas.

x = seq(0,0.65,length = 10000)

y = dbeta(x,numbers_of_rewards_1[[1]],numbers_of_rewards_0[[1]])
y1 = dbeta(x,numbers_of_rewards_1[[2]],numbers_of_rewards_0[[2]])
y2 = dbeta(x,numbers_of_rewards_1[[3]],numbers_of_rewards_0[[3]])
y3 = dbeta(x,numbers_of_rewards_1[[4]],numbers_of_rewards_0[[4]])
y4 = dbeta(x,numbers_of_rewards_1[[5]],numbers_of_rewards_0[[5]])
y5 = dbeta(x,numbers_of_rewards_1[[6]],numbers_of_rewards_0[[6]])
y6 = dbeta(x,numbers_of_rewards_1[[7]],numbers_of_rewards_0[[7]])
y7 = dbeta(x,numbers_of_rewards_1[[8]],numbers_of_rewards_0[[8]])
y8 = dbeta(x,numbers_of_rewards_1[[9]],numbers_of_rewards_0[[9]])
y9 = dbeta(x,numbers_of_rewards_1[[10]],numbers_of_rewards_0[[10]])
y10 = dbeta(x,numbers_of_rewards_1[[11]],numbers_of_rewards_0[[11]])
y11 = dbeta(x,numbers_of_rewards_1[[12]],numbers_of_rewards_0[[12]])
y12 = dbeta(x,numbers_of_rewards_1[[13]],numbers_of_rewards_0[[13]])
y13 = dbeta(x,numbers_of_rewards_1[[14]],numbers_of_rewards_0[[14]])
y14 = dbeta(x,numbers_of_rewards_1[[15]],numbers_of_rewards_0[[15]])
y15 = dbeta(x,numbers_of_rewards_1[[16]],numbers_of_rewards_0[[16]])




dev.new()
plot(x,y,type="l", col="blue",ylim = c(0,25),xlim=c(0,0.6),lwd=2,ylab='Density')
lines(x,y1,col = "orange",lwd = 2)    ###Check plot. 
lines(x,y2,col="red",lwd=2)
lines(x,y3,col="green",lwd=2)
lines(x,y4,col="gray",lwd=2)
lines(x,y5,col="blue4",lwd=2)
lines(x,y6,col="brown",lwd=2)
lines(x,y7,col="#009999",lwd=2)
lines(x,y8,col="chartreuse3",lwd=2)
lines(x,y9,col="peachpuff3",lwd=2)
lines(x,y10,col="darkgoldenrod1",lwd=2)
lines(x,y11,col="cornflowerblue",lwd=2)
lines(x,y12,col="yellow",lwd=2)
lines(x,y13,col="#E7298A",lwd=2)
lines(x,y14,col="cyan",lwd=2)
lines(x,y15,col="darkorchid",lwd=2)


#Questions and Other Rough Thoughts:

#1. Where am considereing Binomial ? Is it the way I define my alpha-beta parameters and the way I calculate reward, all of which include count of messages opened? Can I do this using open-rate to calculate alpha-beta parameters? How would I calculate reward in this case?

#2. How many times should the experiment be repeated (What would be the time period)? What would be the stopping criteria to the update the alpha and beta parameters?

#3. Which Approach:
#Find best message based on the maximum theta_k that we get after final iteration. 
#Use this selected message to build a model and make predictions.
#Set threshold for the predicted theta_k and label 0/1 to be the outcome of (message opened/not opened)
#Compute the Total reward for selected message.

#Or 

#Build the models for all message types and do the predictions.
#Calculate Total rewards for all the message types and find the one with maximum Total_Reward based on predictions.
#Suggest to use the selected message type (with the known feature levels) to optimize the open rates.


##### Further Thoughts

#1. In this prototype I am simulating x.For x ,in actual experiment we could use parameters 
#(polarity,punctuation intensity and title length (As we are using beta distribution, scale title_length to lie between  0 & 1)).

