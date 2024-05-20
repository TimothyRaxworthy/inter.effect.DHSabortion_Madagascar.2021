library(haven)
library(tidyverse)
library(lme4)
library(lmerTest)
library(rstan)
library(rstanarm)
library(data.table)
library(optimx)

# Fit Bayesian models
## Unconditional Bayesian model 

dta <-  haven::read_dta("data.analysis_2024.03.13.dta")

#Bayesian model
uncond <- stan_glmer(preg_term ~ 1 + (1|int_code) + (1|clust_code), #Only random intercepts 
                     data = dta, 
                     chain = 4, #Split two chains across 2 cores
                     iter = 2500, 
                     init = 'random', 
                     family = binomial, 
                     seed = 12345, 
                     cores = 4)

saveRDS(uncond, file = "uncond.rds")

## Extract both variance parameters and credible intervals
#Create data frame
df <- as.data.frame(uncond)

#extract variance due to sample clustering
sigma_k = as.data.frame(as.numeric(df$`Sigma[clust_code:(Intercept),(Intercept)]`))
k <- sigma_k$`as.numeric(df$\`Sigma[clust_code:(Intercept),(Intercept)]\`)`
k <- as.data.frame(k)

#Order object containing sigma k's
setorder(k)

#Median and 2.5% and 97.5% credible interval
k$k[125]
k$k[4875]
median(k$k)
theta_k <- median(k$k)

#extract variance due to interviewers
sigma_j = as.data.frame(as.numeric(df$`Sigma[int_code:(Intercept),(Intercept)]`))
j <- sigma_j$`as.numeric(df$\`Sigma[int_code:(Intercept),(Intercept)]\`)`
j <- as.data.frame(j)

#order object containing sigma j's
setorder(j)

#Median and 2.5% and 97.5% credible interval for estimated interviewer variance
median(j$j)
j$j[125]
j$j[4875]
theta_j <- median(j$j)

#pi^2/3 = Fixed variance for logistic model
sigma_e <- 3.2898681337

#ICC due to interviewer
theta_j/(sigma_e + theta_j + theta_k)

#ICC due to sample clustering
theta_k/(sigma_e + theta_j + theta_k)


## Individual Level predictors

# make sure region and marital are factor variable
indi_predict <- stan_glmer(preg_term ~ age + as.factor(marital) + rural + 
                             as.factor(region) + educat + wealth + (1|int_code) + (1|clust_code), 
                           data = dta, 
                           chain = 4, 
                           iter = 2500, 
                           cores = 4, 
                           init = 'random', 
                           family = binomial,
                           seed = 12345)



saveRDS(indi_predict, file = "indi_predict.rds")

#same as before
df <- as.data.frame(indi_predict)

sigma_k = as.data.frame(as.numeric(df$`Sigma[clust_code:(Intercept),(Intercept)]`))
k_1 <- sigma_k$`as.numeric(df$\`Sigma[clust_code:(Intercept),(Intercept)]\`)`

k_1 <- as.data.frame(k_1)
setorder(k_1)
median(k_1$k_1)
k_1$k_1[125]
k_1$k_1[4875]
theta.k_1 <- median(k_1$k_1)


sigma_j = as.data.frame(as.numeric(df$`Sigma[int_code:(Intercept),(Intercept)]`))
j_1 <- sigma_j$`as.numeric(df$\`Sigma[int_code:(Intercept),(Intercept)]\`)`

j_1 <- as.data.frame(j_1)
setorder(j_1)
median(j_1$j_1)
j_1$j_1[125]
j_1$j_1[4875]
theta.j_1 <- median(j_1$j_1)

#Icc interviewer

theta.j_1/(sigma_e + theta.j_1 + theta.k_1)


#icc for cluster
theta.k_1/(sigma_e + theta.j_1 + theta.k_1)


## Run saturated model with group level predictors
#Difficult to get sampling started, set to one chain 
group_predict <- stan_glmer(preg_term ~ age + as.factor(marital) + rural + as.factor(region) 
                            + educat + wealth + as.factor(fw_exp_survey) + 
                              (1|int_code) 
                            + (1|clust_code), 
                            data = dta, 
                            chain = 1, 
                            iter = 10000, 
                            cores = 1, 
                            init = 'random', 
                            family = binomial,
                            seed = 12345)

saveRDS(group_predict, file = "group_predict.rds")


#same as before

df <- as.data.frame(group_predict)

sigma_k = as.data.frame(as.numeric(df$`Sigma[clust_code:(Intercept),(Intercept)]`))
k_2 <- sigma_k$`as.numeric(df$\`Sigma[clust_code:(Intercept),(Intercept)]\`)`

k_2 <- as.data.frame(k_2)

setorder(k_2)
median(k_2$k_2)
k_2$k_2[125]
k_2$k_2[4875]
theta.k_2 <- median(k_2$k_2)


sigma_j = as.data.frame(as.numeric(df$`Sigma[int_code:(Intercept),(Intercept)]`))
j_2 <- sigma_j$`as.numeric(df$\`Sigma[int_code:(Intercept),(Intercept)]\`)`

j_2 <- as.data.frame(j_2)
setorder(j_2)
median(j_2$j_2)
j_2$j_2[125]
j_2$j_2[4875]
theta.j_2 <- median(j_2$j_2)

#Icc interviewer
theta.j_2/(theta.k_2 + theta.j_2 + sigma_e)

#icc for cluster
theta.k_2/(theta.k_2+theta.j_2+sigma_e)