---
title: "V.4. longitudinal project"
author: "Catherine Rachel"
date: "2024-05-05"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r library}
library(haven)
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
library(nlme)
library(performance)
library(Matrix)

#setwd("/Users/liangqiran/Desktop/longitudinal/finalproj")

```


```{r data}
final=read_excel("Final.xlsx")

#unique ID for each person
final.pid <- final %>% 
  mutate(PID = (ER30001 * 1000) + ER30002) %>%
  relocate(PID) #putting at beginning of dataset

#checking for duplicates by PID
sum(duplicated(final.pid$PID)) #should return 0

final=final.pid
```

## DATA cleaning
```{r recoded data}
## FOR 5 WAVES OF DATA : 2011,2013,2015,2017,2019

###Long form data

#Outcome Peer drug use
#TIME Variable (Age)
final$baseage <- mean(final$ER34104[final$ER34104 > 0])

l.final = final %>% 
  pivot_longer(., cols= c("TA111020","TA131055","TA151095","TA171928","TA192089"), values_to = "peer_drug_use", names_to = "peer_col") %>% 
  mutate(age = case_when(
    peer_col == "TA111020" ~ baseage + 0,
    peer_col == "TA131055" ~ baseage + 2,
    peer_col == "TA151095" ~ baseage + 4,
    peer_col == "TA171928" ~ baseage + 6,
    peer_col == "TA192089" ~ baseage + 8,
  )) %>% select(., c("PID","age", "peer_drug_use"))
l.final$peer_drug_use[l.final$peer_drug_use %in% c(8,9)]=NA #recode 8,9 with NA

discrimination = final %>%
  pivot_longer(., cols = c("TA111130", "TA131222", "TA151282", "TA171977", "TA192156"), 
               values_to = "discrimination", names_to = "discrimination_col") %>%
  pull(discrimination)
#recoded to 9=NA
discrimination_C = discrimination
discrimination_C[discrimination_C==9]=NA
l.final$discrimination= discrimination_C


###RACE_ETHNIC variable 
##since the codebook for few waves are different for race, the variables are combined by individual wave
# 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
re1=function(x,y){
  # x is ethnicity, y is race
  #recoded hispanic variable: 0= Not hispanic, 1= Hispanic, NA
  hisp=ifelse(x==0,0,
              ifelse(x %in% c(1:7),1,NA))
  #recoded combined race variable: 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
  re=ifelse(y==1 & hisp==0,1,
            ifelse(y==2 & hisp==0,2,
                   ifelse(y==4 & hisp==0,3,
                        ifelse(y %in% c(8,9), NA,
                          ifelse((y %in% c(3,5,6,7) & hisp==0),4,
                             ifelse(hisp==1,0,NA))))))
  print(re)
}
re11=final %>%select(., c("TA111056", "TA111057"))
rac11=re1(re11$TA111056,re11$TA111057)

re13=final %>%select(.,c("TA131091","TA131092"))
rac13=re1(re13$TA131091,re13$TA131092)

re15=final %>% select(., c("TA151131","TA151132"))
rac15=re1(re15$TA151131, re15$TA151132)

#second function as the coding for race for the next two waves is different
re2=function(x,y){
  # x is ethnicity, y is race
  #recoded hispanic variable: 0= Not hispanic, 1= Hispanic, NA
  hisp=ifelse(x==0,0,
              ifelse(x %in% c(1:7),1,NA))
  #recoded combined race variable: 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
  re=ifelse(hisp==1|y==2,0,
            ifelse(hisp==0 & y==1,1,
                   ifelse(hisp==0 & y==3,2,
                          ifelse(hisp==0 & y==4,3,
                                 ifelse(hisp==0 & y %in% c(5:8), 4,NA)))))
}

re17= final %>% select(., c("TA171960","TA171955"))
rac17=re2(re17$TA171960, re17$TA171955)

re19= final%>% select(., c("TA192136", "TA192131"))
rac19=re2(re19$TA192136, re19$TA192131)

rac=data.frame(rac11=rac11, rac13=rac13, rac15=rac15, rac17=rac17, rac19=rac19)
race_ethnic=rac %>% 
  pivot_longer(cols=c(rac11,rac13,rac15,rac17,rac19), values_to = "race_ethnic") %>%
  mutate(time = case_when( #time=wave
   name == "rac11" ~ 1,
   name == "rac13" ~ 2,
   name == "rac15" ~ 3,
   name == "rac17" ~ 4,
   name == "rac19" ~ 5
  )) %>%
  select(.,c("time", "race_ethnic"))
race=race_ethnic$race_ethnic
l.final$race=race

###Education of head of the household - provided as number of grades completed
#Since the education is only asked if there is a new family unit, the baseline is set during the first wave 2011 of this study
education_hd= rep(final$ER52405, each=5)
##recode the no.of grades into categories of education level 
#0- <=12 as <=HS/GED
#1- >=13 and <=15 - some college
#2- 16- college degree
#3- 17- At least some post-graduate work
education_head=ifelse(education_hd<=12,0,ifelse(education_hd %in%c(13,14,15),1,ifelse(education_hd==16,2,ifelse(education_hd==17,3,NA))))
l.final$education_head=education_head


#View(l.final)
```
##univariate analysis
```{r univariate analysis} 
###univariate analysis
# freq=list()
# for (i in 1:5) {
#   data_time <- l.final %>% filter(time == i) %>% select(., -PID)
#   fr_pr=function(x){
#     a=table(x, useNA = "always")
#     b=round(prop.table(table(x, useNA="always")) * 100,2)
#     return(data.frame(n=a, perc=b))
#   }
#   freq[[i]] <- lapply(data_time, fr_pr)
# }  
# freq[[1]] #at wave=1
# freq[[2]] #for wave=2
# freq[[3]] #for wave=3
# freq[[4]] #for wave=4
# freq[[5]] #for wave 5

l.filtered = l.final %>% 
              group_by(PID) %>%
              filter(sum(!is.na(peer_drug_use)) >= 2 
                     &sum(!is.na(discrimination)) >= 2 
                     &sum(!is.na(race)) >= 2 
                     &sum(!is.na(education_head)) >= 2)
```

------------------------------------------------------------------------------
##a. descriptive
```{r}
l.filtered$peer_drug_use <- as.factor(l.filtered$peer_drug_use)
l.filtered$discrimination <- as.factor(l.filtered$discrimination)
l.filtered %>%
  select(-PID)
demo_table1 <- l.filtered %>%
  filter(time == 1) %>%
  select(sex, race, education_head, risk_behav, peer_drug_use, discrimination, multiple) %>%
  tbl_summary() %>%
  bold_labels()
demo_table2 <- l.filtered %>%
  filter(time == 2) %>%
  select(sex, race, education_head, risk_behav, peer_drug_use, discrimination, multiple) %>%
  tbl_summary() %>%
  bold_labels()
demo_table3 <- l.filtered %>%
  filter(time == 3) %>%
  select(sex, race, education_head, risk_behav, peer_drug_use, discrimination, multiple) %>%
  tbl_summary() %>%
  bold_labels()
demo_table4 <- l.filtered %>%
  filter(time == 4) %>%
  select(sex, race, education_head, risk_behav, peer_drug_use, discrimination, multiple) %>%
  tbl_summary() %>%
  bold_labels()
demo_table5 <- l.filtered %>%
  filter(time == 5) %>%
  select(sex, race, education_head, risk_behav, peer_drug_use, discrimination, multiple) %>%
  tbl_summary() %>%
  bold_labels()

descriptive_table1 <- tbl_merge(
  list(demo_table1, demo_table2, demo_table3, demo_table4, demo_table5),
  tab_spanner = c("**Wave 1**", "**Wave 2**", "**Wave 3**", "**Wave 4**", "**Wave 5**"))

#save
descriptive_table1 %>%
  as_gt() %>%
  gt::gtsave(filename = "descriptive_table1.png")
```

--------------------------------------------

##b. Describe the growth in your outcome 
```{r}
set.seed(0)
sample <- final[sample(4615, size = 80), ]
```


DATA cleaning for the sample
```{r recoded data}
## FOR 5 WAVES OF DATA : 2011,2013,2015,2017,2019

###Long form data
#RISKY BEHAVIOR - OUTCOME

l.sample= sample %>% 
  pivot_longer(., cols= c("TA111020","TA131055","TA151095","TA171928","TA192089"), values_to = "peer_drug_use", names_to = "peer_col") %>% 
  mutate(age = case_when(
    peer_col == "TA111020" ~ baseage + 0,
    peer_col == "TA131055" ~ baseage + 2,
    peer_col == "TA151095" ~ baseage + 4,
    peer_col == "TA171928" ~ baseage + 6,
    peer_col == "TA192089" ~ baseage + 8,
  )) %>% select(., c("PID","age", "peer_drug_use"))
  
l.sample$peer_drug_use[l.sample$peer_drug_use %in% c(8,9)]=NA #8=DK, 9=NA/refused so recoded to NA


discrimination = sample %>%
  pivot_longer(., cols = c("TA111130", "TA131222", "TA151282", "TA171977", "TA192156"), 
               values_to = "discrimination", names_to = "discrimination_col") %>%
  pull(discrimination)
#recoded to 9=NA
discrimination_C = discrimination
discrimination_C[discrimination_C==9]=NA
l.sample$discrimination= discrimination_C


###RACE_ETHNIC variable 
##since the codebook for few waves are different for race, the variables are combined by individual wave
# 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
re1=function(x,y){
  # x is ethnicity, y is race
  #recoded hispanic variable: 0= Not hispanic, 1= Hispanic, NA
  hisp=ifelse(x==0,0,
              ifelse(x %in% c(1:7),1,NA))
  #recoded combined race variable: 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
  re=ifelse(y==1 & hisp==0,1,
            ifelse(y==2 & hisp==0,2,
                   ifelse(y==4 & hisp==0,3,
                        ifelse(y %in% c(8,9), NA,
                          ifelse((y %in% c(3,5,6,7) & hisp==0),4,
                             ifelse(hisp==1,0,NA))))))
  print(re)
}
re11=sample %>%select(., c("TA111056", "TA111057"))
rac11=re1(re11$TA111056,re11$TA111057)

re13=sample %>%select(.,c("TA131091","TA131092"))
rac13=re1(re13$TA131091,re13$TA131092)

re15=sample %>% select(., c("TA151131","TA151132"))
rac15=re1(re15$TA151131, re15$TA151132)

#second function as the coding for race for the next two waves is different
re2=function(x,y){
  # x is ethnicity, y is race
  #recoded hispanic variable: 0= Not hispanic, 1= Hispanic, NA
  hisp=ifelse(x==0,0,
              ifelse(x %in% c(1:7),1,NA))
  #recoded combined race variable: 0= Hispanic/Latino, 1= Non-hispanic white, 2= Non-hispanic black, 3= Non-hispanic asian,    4= Non-hispanic other including more than one, NA
  re=ifelse(hisp==1|y==2,0,
            ifelse(hisp==0 & y==1,1,
                   ifelse(hisp==0 & y==3,2,
                          ifelse(hisp==0 & y==4,3,
                                 ifelse(hisp==0 & y %in% c(5:8), 4,NA)))))
}

re17= sample %>% select(., c("TA171960","TA171955"))
rac17=re2(re17$TA171960, re17$TA171955)

re19= sample%>% select(., c("TA192136", "TA192131"))
rac19=re2(re19$TA192136, re19$TA192131)

rac=data.frame(rac11=rac11, rac13=rac13, rac15=rac15, rac17=rac17, rac19=rac19)
race_ethnic=rac %>% 
  pivot_longer(cols=c(rac11,rac13,rac15,rac17,rac19), values_to = "race_ethnic") %>%
  mutate(time = case_when( #time=wave
   name == "rac11" ~ 1,
   name == "rac13" ~ 2,
   name == "rac15" ~ 3,
   name == "rac17" ~ 4,
   name == "rac19" ~ 5
  )) %>%
  select(.,c("time", "race_ethnic"))
race=race_ethnic$race_ethnic
l.sample$race=race

###Education of head of the household - provided as number of grades completed
#Since the education is only asked if there is a new family unit, the baseline is set during the first wave 2011 of this study
education_hd= rep(sample$ER52405, each=5)
##recode the no.of grades into categories of education level 
#0- <=12 as <=HS/GED
#1- >=13 and <=15 - some college
#2- 16- college degree
#3- 17- At least some post-graduate work
education_head=ifelse(education_hd<=12,0,ifelse(education_hd %in%c(13,14,15),1,ifelse(education_hd==16,2,ifelse(education_hd==17,3,NA))))
l.sample$education_head=education_head

#View(l.sample)
```

```{r}
l.samplefiltered = l.sample %>% 
              group_by(PID) %>%
              filter(sum(!is.na(peer_drug_use)) >= 2 
                     &sum(!is.na(discrimination)) >= 2 
                     &sum(!is.na(race)) >= 2 
                     &sum(!is.na(education_head)) >= 2)
```

### i. individual growth plot
####Individual non-parametric trajectories
```{r}
l.samplefiltered$high_risk <- ifelse(l.samplefiltered$peer_drug_use <= 2, 0,
                                     ifelse(l.samplefiltered$peer_drug_use >= 3, 1, NA))

ggplot(data = l.samplefiltered, aes(x = age, y = high_risk)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(PID))
```

####Non-Parametric standardization
```{r}
ggplot(data = l.samplefiltered, aes(x = age, y = peer_drug_use)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(PID))
```
### ii.	Individual OLS regressions conducted and visualized with the mean trajectory line. 
```{r}
l.samplefiltered$age_c <- l.samplefiltered$age - mean(l.samplefiltered$age)

```

```{r}
ggplot(data = l.samplefiltered, aes(x = age_c, y =high_risk)) +
  geom_smooth(aes(group = as.factor(PID)), method = "glm",
              color = "black", se = FALSE) +
  geom_smooth(method = "glm", color = "red", se = FALSE)
```




-----------------------------------------------
## c.	What are the sample means of the estimated intercepts and slopes?
### i. What are the sample means of the estimated intercepts and slopes? 

```{r}
l.filtered$high_risk <- ifelse(l.filtered$peer_drug_use <= 2, 0,
                                     ifelse(l.filtered$peer_drug_use >= 3, 1, NA))
l.filtered$high_risk <- as.numeric(as.character(l.filtered$high_risk))

l.filtered$age_c <- l.filtered$age - mean(l.filtered$age)
l.filtered$age_c <- as.numeric(l.filtered$age_c)

models <- l.filtered %>% 
  group_by(PID) %>% 
  filter(all(!is.na(high_risk), !is.na(age_c))) %>%
  do(model = glm(high_risk ~ age_c, data = .))

models[[2]][[1]]

intercept <- slope <- NULL

for(i in 1:nrow(models)){
  intercept[i] <- models[[2]][[i]][["coefficients"]][1]
  slope[i] <- models[[2]][[i]][["coefficients"]][2]
}



```

###ii. What are the sample variances (i.e., standard deviations) of the estimated intercepts and slopes? 
```{r}
summary(intercept)
var(intercept)
sd(intercept)

summary(slope)
var(slope)
sd(slope)
```

###iii. What is the correlation between the estimated intercepts and slopes (i.e., covariance)? 
```{r}
cor(intercept, slope)
```

###iv. Without having conducted the full multi-level model what are your initial thoughts concerning the intercept and slope of the level one model and the level 2 model? 


```{r}
l.naomit=na.omit(l.filtered)
l.naomit<- within(l.naomit, {
  high_risk <- factor(high_risk, levels = 0:1, labels = c("no", "yes"))
  discrimination <- factor(discrimination,levels = 1:6)
  race <- factor(race, levels = 0:4, labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other"))
  education_head <- factor(education_head, levels = 0:3, labels = c("<=High School", "Some College", "4- Year College", "Any Post Graduate Work"))
})

#unconditional mean model
uc.mean=glmer(high_risk~1 + (1 | PID), data=l.naomit, family="binomial")
summary(uc.mean)

# AIC
AIC(uc.mean)
# BIC
BIC(uc.mean)
#  Deviance
-2*logLik(uc.mean)
#Likelihood
logLik(uc.mean)
# ICC Calculation
icc(uc.mean)

#unconditional growth model
uc.growth=glmer(high_risk~ age + (age | PID), data=l.naomit, family="binomial")
summary(uc.growth)

# AIC
AIC(uc.growth)
# BIC
BIC(uc.growth)
#  Deviance
-2*logLik(uc.growth)
#Likelihood
logLik(uc.growth)
# ICC Calculation
icc(uc.growth)

#conditional model with main IV only
c.iv1=glmer(high_risk~ discrimination+age+discrimination*age+ (1|PID), 
            data=l.naomit, 
            family="binomial")

summary(c.iv1) 
# AIC
AIC(c.iv1)
# BIC
BIC(c.iv1)
#  Deviance
-2*logLik(c.iv1)
#Likelihood
logLik(c.iv1)
# ICC Calculation
icc(c.iv1)

l.naomit$pred.iv1=predict(c.iv1,type="response")
data_predicted_prob <- l.naomit %>%
  group_by(age, discrimination) %>%
  summarise(avg_prob = mean(pred.iv1))

ggplot(data = data_predicted_prob) +
  geom_line(aes(x = age, y = avg_prob, 
                color = discrimination))


#conditional growth with all IVs
c.iv2=glmer(high_risk~ discrimination+age +
                       discrimination*age +
                       race + education_head +
                      (1|PID), 
            data=l.naomit, 
            family="binomial")

summary(c.iv2) 
# AIC
AIC(c.iv2)
# BIC
BIC(c.iv2)
#  Deviance
-2*logLik(c.iv2)
#Likelihood
logLik(c.iv2)
# ICC Calculation
icc(c.iv2)

l.naomit$pred.iv2=predict(c.iv2, type="response")

data_predicted_prob2 <- l.naomit %>%
  group_by(age, discrimination, race) %>%
  summarise(avg_prob = mean(pred.iv2))


data_predicted_prob3 <- l.naomit %>%
  group_by(age, discrimination, education_head) %>%
  summarise(avg_prob = mean(pred.iv2))


ggplot(data = data_predicted_prob) +
  geom_line(aes(x = age, y = avg_prob, 
                color = discrimination))

ggplot(data = data_predicted_prob2) +
  geom_line(aes(x = age, y = avg_prob, 
                color = discrimination)) +
  facet_wrap(vars(race))

ggplot(data = data_predicted_prob3) +
  geom_line(aes(x = age, y = avg_prob, 
                color = discrimination)) +
  facet_wrap(vars(education_head))

```
