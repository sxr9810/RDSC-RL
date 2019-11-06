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
beta_data = ECData[,c(1:5,10,15,17:18)]

#Renaming the column values as 0,1 for modelling purpose
beta_data$author_hidden = ifelse(beta_data$author_hidden == 't',0,1)
beta_data$has_video = ifelse(beta_data$has_video == 't',0,1)
beta_data$comments_enabled = ifelse(beta_data$comments_enabled == 't',0,1)
beta_data$has_link = ifelse(beta_data$has_link == TRUE,0,1)

#Permutation
comb_data = expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))
colnames(comb_data) = c('has_link','author_hidden','has_video','comments_enabled')  
comb_data = data.frame(cbind(ECData[2:17,1:5]),comb_data)
t_comb_data = t(comb_data)
colnames(t_comb_data)  = c(paste0("X",1:16))




###### Thompson Sampling

d = 16  #Number of arms
total_reward = 0  #would you calculate this and how will you use this?

#Thompson Sampling

numbers_of_rewards_1 = t_comb_data[2,]                        #prior alpha parameter - uniq_opens
numbers_of_rewards_0 = t_comb_data[1,] - t_comb_data[2,]      #prior beta parameter -  Not_Opened = delivered - Opened
#Where is Binomial distribution coming into picture? 
#Is setting parameters using N(delivered) where we are considering the binomial distribution ?

message = 0                                                    #Initializing the Message to 0
max_random = 0                                                #Initializing the maximum random estimate (Theta_k)

set.seed(123)
#Following the steps for Algorithm 2:Bern Thompson Sampling : Page 7- file:///C:/Users/srumao/Desktop/RL/A%20Tutorial%20on%20Thompson%20Sampling.pdf
# for (i in 1:d) 
# {
#   
#   #Randomly sampling theta_k estimate from the posterior beta distribution for the paramters(alpha,beta) defined above. 
#   random_beta = rbeta(n = 1,
#                       shape1 = numbers_of_rewards_1[i],             
#                       shape2 = numbers_of_rewards_0[i] )
#   if (random_beta > max_random) 
#   {
#     max_random = random_beta
#     #Returning the message at which theta_k is maximum
#     message = i   
#   }
#   
# }


###### John's attempt at TS
# learning the underlying theta
set.seed(1234)
n = 1000
arm = numeric(n)
alpha = beta = numeric(d)
alpha[1] = beta[1] = 1 # prior
y = numeric(n) # experiment outcome
theta = t_comb_data["openrate",]
#theta = c(0.99, rep(0.01,15))
thetahat = matrix(0, nrow = n, ncol = d)

for( ii in 1:n){
  thetahat[ii,] = rbeta(d,shape1 = alpha,shape2 = beta)
  
  arm[ii] = which.max(thetahat[ii,]) # choose an arm
  
  
  # perform the experiment using the "population" theta
  
  y[ii] = rbernoulli(1,p = theta[arm[ii]])
  
  #Keep a count of y for each arm somewhere
  
  kk = arm[ii]
  alpha[kk] =    alpha[kk] + y[ii]
  beta[kk] =   beta[kk] + 1-y[ii]
}
print(thetahat[ii,])
#print(t(t_comb_data[6:9,arm]))

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

