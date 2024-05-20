library(haven)
library(tidyverse)
library(lme4)
library(lmerTest)
library(rstan)
library(rstanarm)
library(data.table)
library(optimx)
library(arm)

## Fit glmer model with  only classified random intercepts by interviewer
glm_level1 <- glmer(preg_term ~ age_rescale + marital + rural + region + educat 
                    + wealth + (1|int_code), data = data_analysis, nAGQ = 7, 
                    family = binomial(link = "logit"))

summary(data_analysis$age_rescale)
summary(data_analysis$age_centered)

#Warning Convergence Issues

length(fixef(glm_level1)) #37 coefficients
length(getME(glm_level1,"theta")) #1

tt <- getME(glm_level1,"theta")
ll <- getME(glm_level1,"lower")
min(tt[ll==0]) #not a problem


#Create initial estimated parameters
start <- getME(glm_level1,c("theta","beta"))
strt_params <- list("theta" = start$theta, "fixef" = start$beta)

#Run model with start values
glm_level1_update <- glmer(preg_term ~ age_rescale + marital + rural + region + educat 
                           + wealth + (1|int_code), data = data_analysis, nAGQ = 7, 
                           family = binomial(link = "logit"), start = strt_params)
glm_level1_update

#Obtain the interviewer variance for level one params
sigma_null <- (getME(glm_level1,c("theta")))^2

#Examine model
summary(glm_level1)
summary(glm_level1_update)
saveRDS(glm_level1_update, file = "glm_level1.rds")


## Create coefficient chart for level 1 model

glm_mod <-summary(glm_level1_update)

#extract coefficients & SE's
glm_mod$coefficients
se <- glm_mod$coefficients[-c(38:60),2]
coef <- glm_mod$coefficients[-c(38:60),1]

#Put in names of each coefficient
name <- c( 
  "(Intercept)",
  "age",
  "married", 
  "cohabiting", 
  "widowed", 
  "divorced", 
  "seperated", 
  "rural residence",
  "Analamanga", 
  "Vakinankaratra", 
  "Itasy", 
  "Bongolava", 
  "Haute matsiatra", 
  "Amoron i mania", 
  "Vatovavy fitovinany", 
  "Ihorombe", 
  "Atsimo atsinanana", 
  "Atsinanana", 
  "Analanjirofo", 
  "Alaotra mangoro", 
  "Boeny", 
  "Sofia", 
  "Betsiboka", 
  "Melaky",
  "Atsimo andrefana", 
  "Androy", 
  "Anosy", 
  "Menabe", 
  "Diana", 
  "Sava", 
  "Primary Education",
  "Secondary Education",
  "HS and Above",
  "2nd wealth quintile",
  "3rd wealth quintile",
  "4th wealth quintile",
  "5th wealth quintile")


model.level1.params <- data.frame(name, coef, se)
model.level1.params$moe <- 1.96*se
model.level1.params$name <- factor(x = model.level1.params$name, ordered = is.ordered(model.level1.params$name))
model.level1.params

p <- ggplot(model.level1.params, aes(coef, fct_inorder(name), 
                                     xlab("Regression Coefficients"), 
                                     xmin = coef-moe, xmax = coef+moe))
p + geom_pointrange(size=0.2) + geom_vline(xintercept = 0) +
  labs(x = NULL, y = "Regression Coefficients",
       title ="95% CI for level 1 coefficients in fully saturated model") +
  theme_bw()


ggsave(filename = "level1_predictors.pdf", device = "pdf", path = "/Users/timra/Desktop/Explaining_Interviewer_Effects/", width = 8, height = 5.71, units = "in", dpi = "print")




## Level 2 Models


glm_level2 <- glmer(data = data_analysis, preg_term ~ marital + rural + age_rescale 
                    + region + educat + wealth + fw_residence + fw_age_centered + 
                      fw_marital + fw_child + fw_educat + fw_religion + fw_exp_dhs + 
                      fw_exp_survey + fw_instat + fw_french + (1|int_code), 
                    nAGQ = 7, 
                    family = binomial(link = "logit"))

start_level2 <- getME(glm_level2,c("theta","beta"))

strt_params_level2 <- list("theta" = start_level2$theta, "fixef" = start_level2$beta)

glm_level2_update <- glmer(data = data_analysis, preg_term ~ marital + rural + age_rescale 
                           + region + educat + wealth + fw_residence + fw_age_centered + 
                             fw_marital + fw_child + fw_educat + fw_religion + fw_exp_dhs + 
                             fw_exp_survey + fw_instat + fw_french + (1|int_code), 
                           nAGQ = 7, 
                           family = binomial(link = "logit"),
                           start = strt_params_level2)

summary(glm_level2_update)
saveRDS(glm_level2_update, file = "glm_level2")




## Make level 2 predictor chart.
glm_mod <-summary(glm_level2_update)
glm_mod$coefficients


se <- glm_mod$coefficients[-c(38:60),2]
coef <- glm_mod$coefficients[-c(38:60),1]

name <- c( 
  "(Intercept)",
  "married", 
  "cohabiting", 
  "widowed", 
  "divorced", 
  "seperated", 
  "rural residence",
  "age",
  "Analamanga", 
  "Vakinankaratra", 
  "Itasy", 
  "Bongolava", 
  "Haute matsiatra", 
  "Amoron i mania", 
  "Vatovavy fitovinany", 
  "Ihorombe", 
  "Atsimo atsinanana", 
  "Atsinanana", 
  "Analanjirofo", 
  "Alaotra mangoro", 
  "Boeny", 
  "Sofia", 
  "Betsiboka", 
  "Melaky",
  "Atsimo andrefana", 
  "Androy", 
  "Anosy", 
  "Menabe", 
  "Diana", 
  "Sava", 
  "Primary Education",
  "Secondary Education",
  "HS and Above",
  "2nd wealth quintile",
  "3rd wealth quintile",
  "4th wealth quintile",
  "5th wealth quintile")

model.level1.params <- data.frame(name, coef, se)
library(ggplot2)

model.level1.params$moe <- 1.96*se
model.level1.params$name <- factor(x = model.level1.params$name, ordered = is.ordered(model.level1.params$name))
model.level1.params

p <- ggplot(model.level1.params, aes(coef, fct_inorder(name), 
                                     xlab("Regression Coefficients"), 
                                     xmin = coef-moe, xmax = coef+moe))
p + geom_pointrange(size=0.2) + geom_vline(xintercept = 0) +
  labs(x = NULL, y = "Regression Coefficients",
       title ="95% CI for level 1 coefficients in fully saturated model") +
  theme_bw()


ggsave(filename = "level1_predictors.pdf", device = "pdf", path = "/Users/timra/Desktop/Explaining_Interviewer_Effects/", width = 8, height = 5.71, units = "in", dpi = "print")


## Level 2 predictors

glm_mod <-summary(glm_level2_update)
glm_mod$coefficients
se <- glm_mod$coefficients[-c(2:37),2]
coef <- glm_mod$coefficients[-c(2:37),1]

model.level2.params <- data.frame(coef, se)
model.level2.params

name <- c(
  "(Intercept)",
  "town residence",
  "rural residence",
  "age",
  "married",
  "cohabiting",
  "widowed",
  "divorced",
  "seperated",
  "# of children",
  "HS and above",
  "protestant",
  "muslim",
  "other religion",
  "exp. DHS",
  "exp. survey",
  "INSTAT",
  "speaks French"
  
)

model.level2.params <- data.frame(name, coef, se)
model.level2.params


model.level2.params$moe <- 1.96*se
model.level2.params$name <- factor(x = model.level2.params$name, ordered = is.ordered(model.level2.params$name))

p <- ggplot(model.level2.params, aes(coef, fct_inorder(name), 
                                     xlab("Regression Coefficients"), 
                                     xmin = coef-moe, xmax = coef+moe))
p + geom_pointrange(size=0.2) + geom_vline(xintercept = 0) +
  labs(x = NULL, y = "Regression Coefficients",
       title ="95% CI for level 2 coefficients in fully saturated model") +
  theme_bw()


ggsave(filename = "level2_predictors.pdf", device = "pdf", path = "/Users/timra/Desktop/Explaining_Interviewer_Effects/", width = 8, height = 5.71, units = "in", dpi = "print")



## Nested models
### Demographic model

glm_level2_demo <- glmer(data = data_analysis, preg_term ~ marital + rural 
                         + age_rescale + region + educat + wealth + 
                           fw_residence + fw_age_scaled + fw_child + 
                           fw_educat + (1|int_code), 
                         nAGQ = 7, 
                         family = binomial(link = "logit"))

start_level2_demo <- getME(glm_level2_demo,c("theta","beta"))
strt_params_demo <- list("theta" = start_level2_demo$theta, "fixef" = start_level2_demo$beta)


glm_level2_demo_update <- glmer(data = data_analysis, preg_term ~ marital + rural 
                                + age_rescale + region + educat + wealth + 
                                  fw_residence + fw_age_scaled + fw_child + 
                                  fw_educat + (1|int_code), 
                                nAGQ = 7, 
                                family = binomial(link = "logit"),
                                start = strt_params_demo)

saveRDS(glm_level2_demo_update, file = "glm_demo.rds")

summary(glm_level2_demo_update)



### Social Model
glm_level2_social <- glmer(data = data_analysis, preg_term ~ marital + rural +
                             age_rescale + region + educat + wealth + fw_marital 
                           + fw_religion +(1|int_code), 
                           nAGQ = 7, 
                           family = binomial(link = "logit"))


start_level2_social <- getME(glm_level2_social,c("theta","beta"))
strt_params_social <- list("theta" = start_level2_social$theta, "fixef" = start_level2_social$beta)


glm_level2_social_update <- glmer(data = data_analysis, preg_term ~ marital + 
                                    rural 
                                  + age_rescale + region + educat + wealth + fw_marital
                                  + fw_religion + (1|int_code), 
                                  nAGQ = 7, 
                                  family = binomial(link = "logit"),
                                  start = strt_params_social)

saveRDS(glm_level2_social_update, file = "glm_social.rds")

summary(glm_level2_social_update)



### Skill nested model


#Include fw_french + fw_instat + fw_exp_survey + fw_dhs
glm_level2_skill <- glmer(data = data_analysis, preg_term ~ marital + rural +
                            age_rescale + region + educat + wealth + fw_instat +
                            fw_french + fw_exp_survey + fw_exp_dhs 
                          +(1|int_code), 
                          nAGQ = 7, 
                          family = binomial(link = "logit"))


start_level2_skill <- getME(glm_level2_skill,c("theta","beta"))
strt_params_skill <- list("theta" = start_level2_skill$theta, "fixef" = start_level2_skill$beta)


glm_level2_skill_update <- glmer(data = data_analysis, preg_term ~ marital + 
                                   rural 
                                 + age_rescale + region + educat + wealth + fw_instat 
                                 + fw_french + fw_exp_survey + fw_exp_dhs  + 
                                   (1|int_code), 
                                 nAGQ = 7, 
                                 family = binomial(link = "logit"),
                                 start = strt_params_skill)

saveRDS(glm_level2_skill_update, file = "glm_skill.rds")


## Binned residual plot



#full model
data_analysis$phat<-predict(glm_level2_social_update,type="response")



binnedplot(data_analysis$phat,data_analysis$resid, main = 
             "Binned residual plot for model including interviewer social 
           characteristics")

