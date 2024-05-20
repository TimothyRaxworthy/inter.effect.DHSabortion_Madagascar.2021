#Load in packages
library(haven)
library(tidyverse)
library(lme4)
library(lmerTest)
library(rstan)
library(rstanarm)
library(data.table)
library(optimx)

#Load in data

##This is the individual recode data from DHS Mada 2021
full_data <- haven::read_dta(".../Individual Recode/MDIR81FL.DTA")

# Gather respondent variables for analysis
age <- full_data$v012 #Age variable
marital <- full_data$v501 #Marital
rural <- full_data$v025 - 1 #Re-code into dummy variable
region <- relevel(factor(full_data$v024, ordered = FALSE)
                  , ref = "10") #Set capital as ref category
educat <- full_data$v106 #Education variable
wealth <- full_data$v190a - 1 #Wealth variable, poorest = 0
preg_term <- full_data$v228 #Outcome already binary
int_code <- full_data$v028 #Numeric Interviewer identification code
clust_code <- full_data$v001 #Numeric Cluster identification code

#Create reference data object
dta <- as.data.frame(cbind(age, marital, rural, region, educat, wealth, int_code,
                           clust_code, preg_term))




## interviewer level predictors
### Merge in interviewer questionnaire

#Insert correct path for interviewer data
inter_data <- read_dta(".../Fieldworker Questionnaire/MDFW81FL.dta")
#First data frame for interviewer data
df1 <- inter_data #n = 200
df1$int_code <- as.numeric(inter_data$fw101) #Numeric for R 
#Second data frame for respondent data
df2<-dta #n = 18,869
df2$int_code <- as.numeric(dta$int_code) #Numeric for R
#Make sure code for linking is Named the same between datasets!

#interviewers actually conducted interviews for this recode of data
main_data_intcode <- unique(df2$int_code) # 98 unique identifiers 

df1 <- df1 %>%
  filter(int_code %in% main_data_intcode) #Remove interviewers not present

#Merge interviewers 
m <- merge(df2, df1, all.y=TRUE) 
head(m)
head(inter_data)

#Select variables of interest
data_analysis <- m %>%
  select(int_code, age, marital, rural, region, educat, wealth, clust_code, 
         preg_term, fw102, fw103, fw104, fw106, fw107, fw108, fw109, fw111, 
         fw112, fw115, fw116, fw117, fw118, fw113a)

#Rename variables 
data_analysis <- data_analysis %>%
  rename(fw_region = fw102) %>%
  rename(fw_residence = fw103) %>%
  rename(fw_age = fw104) %>%
  rename(fw_marital = fw106) %>%
  rename(fw_child = fw107) %>%
  rename(fw_child_death = fw108) %>% #All have never had a child died, not considered further
  rename(fw_educat = fw109) %>%
  rename(fw_religion = fw111) %>%
  rename(fw_ethnic = fw112) %>% #Variable is completely missing, not considered further
  rename(fw_exp_dhs = fw115) %>%
  rename(fw_exp_survey = fw116) %>%
  rename(fw_instat = fw117) %>%
  rename(fw_employ = fw118) %>% #Contain missing so variable not considered further
  rename(fw_french = fw113a)

write_dta(data_analysis, "data.analysis_2024.03.13.dta")



### Recode variables for model

#Read in saved data object
data_analysis <- haven::read_dta("data.analysis_2024.03.13.dta")

#respondent variables <- recode for R
data_analysis$age_centered<- scale(data_analysis$age, scale = FALSE) #centered
data_analysis$age_rescale <- scale(data_analysis$age, 
                                   scale = TRUE) #rescaled & centered
data_analysis$marital <- relevel(factor(data_analysis$marital, ordered = FALSE), 
                                 ref = "0") #create factor, never union as ref
data_analysis$region <- relevel(factor(data_analysis$region, ordered = FALSE),
                                ref = "1") #Create factor, capital as ref
data_analysis$educat <- relevel(factor(data_analysis$educat, ordered = FALSE),
                                ref="0") #Create factor, no education as ref
data_analysis$wealth <- relevel(factor(data_analysis$wealth, ordered = FALSE),
                                ref="0") #create factor, poorest as ref

#interviewer variables
data_analysis$fw_region <- relevel(factor(data_analysis$fw_region, ordered = 
                                            FALSE), ref = "10") #create factor, capital as ref
#Residence is coded as follows 1). Urban, 2). Town, 3). Rural
data_analysis$fw_residence <- relevel(factor(data_analysis$fw_residence,
                                             ordered = FALSE), ref= "1") #create factor, urban as ref
data_analysis$fw_age_centered <- scale(data_analysis$fw_age, 
                                       scale = F) #Centered
data_analysis$fw_age_scaled <- scale(data_analysis$fw_age, 
                                     scale = T) #centered & scaled
data_analysis$fw_marital <- relevel(factor(data_analysis$fw_marital, ordered = FALSE), 
                                    ref = "6") #create factor, never union as ref
#only two education categories:  secondary & high school or above
data_analysis$fw_educat <- relevel(factor(data_analysis$fw_educat, ordered = FALSE), 
                                   ref = "3") #create factor, secondary as Ref
data_analysis$fw_religion <- relevel(factor(data_analysis$fw_religion, ordered = FALSE)
                                     ,ref = "1") # ref = catholic



#experience variables
data_analysis$fw_exp_dhs <- data_analysis$fw_exp_dhs - 1 #Dummy variable
data_analysis$fw_exp_survey <- data_analysis$fw_exp_survey - 1 #Dummy variable
data_analysis$fw_instat <- data_analysis$fw_instat - 1 #dummy variable
data_analysis$fw_french[data_analysis$fw_french == ""] <- "B" #a = french b = no french
data_analysis$fw_french <- relevel(factor(data_analysis$fw_french, ordered = FALSE
), ref = "B") #create factor, no french as ref
#none have child deaths 2 = No, 1 = Yes
data_analysis$fw_child_death <- data_analysis$fw_child_death - 1 #Create dummy

#Check variables to be coded correctly
summary(data_analysis$fw_french)
summary(data_analysis$fw_exp_dhs)
summary(data_analysis$fw_exp_survey)
summary(data_analysis$fw_instat)
summary(data_analysis$educat)
summary(data_analysis$marital)
summary(data_analysis$wealth)


# Additional Re-code done for model fit using maximum likelihood.
# For Bayesian model we do not care for coefficient interpretation 
