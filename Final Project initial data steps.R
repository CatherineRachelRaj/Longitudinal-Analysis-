---
title: "LA project"
output:
  word_document: default
  pdf_document: default
date: "2024-04-09"
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

```{r recoded data}
## FOR 5 WAVES OF DATA : 2011,2013,2015,2017,2019

###Long form data
#RISKY BEHAVIOR - OUTCOME

l.final= final %>% 
  pivot_longer(., cols=c("TA111126","TA131218","TA151278", "TA171976","TA192157"), values_to = "risk_behav", names_to = "risk_col") %>%
  mutate(time = case_when( #time=wave
   risk_col == "TA111126" ~ 1,
   risk_col == "TA131218" ~ 2,
   risk_col == "TA151278" ~ 3,
   risk_col == "TA171976" ~ 4,
   risk_col == "TA192157" ~ 5
  )) %>% select(., c("PID","time", "risk_behav"))
  
  
peer= final %>% pivot_longer(., cols= c("TA111020","TA131055","TA151095","TA171928","TA192089"), values_to = "peer_drug_use", names_to = "peer_col") %>% 
  pull(peer_drug_use)
peer[peer %in% c(8,9)]=NA #8=DK, 9=NA/refused so recoded to NA
l.final$peer_drug_use=peer


cocaine= final%>%
  pivot_longer(., cols= c("TA110940","TA130973", "TA150995","TA171836","TA191998"), values_to = "cocaine", names_to = "coc_col") %>%
  pull(cocaine)
#recoded to 1= has tried, 0= not tried, NA
cocaine_C=ifelse(cocaine==1,1, 
               ifelse(cocaine %in% c(0,5),0,NA))
l.final$cocaine = cocaine_C

amphetamines= final %>%
  pivot_longer(., cols= c("TA110924","TA130957","TA150979","TA171862","TA192024"), values_to = "amphetamines", names_to = "amp_col") %>%
  pull(amphetamines)
#recoded to 1= has tried, 0= not tried, NA
amphetamines_C=ifelse(amphetamines==1,1,
                      ifelse(amphetamines %in% c(0,5),0,NA))
l.final$amphetamines=amphetamines_C


marijuana=final %>%  
  pivot_longer(., cols= c("TA110932","TA130965", "TA150987", "TA171828", "TA191990"), values_to = "marijuana", names_to = "marj_col") %>%
  pull(marijuana)
#recoded to 1= has tried, 0= not tried, NA
marijuana_C=ifelse(marijuana==1,1,
                      ifelse(marijuana %in% c(0,5),0,NA))
l.final$marijuana=marijuana_C


steroids= final %>%
  pivot_longer(., cols= c("TA110961", "TA130994","TA151016","TA171870", "TA192032"), values_to = "steroids", names_to = "ster_col") %>%
  pull(steroids)
#recoded to 1= has tried, 0= not tried, NA
steroids_C=ifelse(steroids==1,1,
                      ifelse(steroids %in% c(0,5),0,NA))
l.final$steroids=steroids_C


barbiturates= final %>%
  pivot_longer(., cols= c("TA110945", "TA130978", "TA151000","TA171878", "TA192040"), values_to = "barbiturates", names_to = "barb_col") %>%
  pull(barbiturates)
#recoded to 1= has tried, 0= not tried, NA
barbiturates_C=ifelse(barbiturates==1,1,
                      ifelse(barbiturates%in% c(0,5),0,NA))
l.final$barbiturates=barbiturates_C

tranquilizers= final %>%
  pivot_longer(., cols= c("TA110953", "TA130986", "TA151008", "TA171886", "TA192048"), values_to = "tranquilizers", names_to = "tranq_col") %>%
  pull(tranquilizers)
#recoded to 1= has tried, 0= not tried, NA
tranquilizers_C=ifelse(tranquilizers==1,1,
                      ifelse(tranquilizers %in% c(0,5),0,NA))
l.final$tranquilizers=tranquilizers_C


discrimination = final %>%
  pivot_longer(., cols = c("TA111130", "TA131222", "TA151282", "TA171977", "TA192156"), 
               values_to = "discrimination", names_to = "discrimination_col") %>%
  pull(discrimination)
#recoded to 9=NA
discrimination_C = discrimination
discrimination_C[discrimination_C==9]=NA
l.final$discrimination= discrimination_C


##NEW VARIABLES
#no.of substances used
no_sub=l.final %>% select(., c("cocaine","amphetamines","marijuana","steroids","barbiturates","tranquilizers")) %>% mutate(no_sub=rowSums(., na.rm=F))
#multi-drug user: if no_sub >1 TRUE, if no_sub<=1 F, NA
no_sub$multiple=ifelse(no_sub$no_sub>1,T,
                       ifelse(no_sub$no_sub<=1,F,NA))
l.final$no_sub= no_sub$no_sub
l.final$multiple= no_sub$multiple


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

###Sex of individual
#1- Male , 2- Female
#there are no missings
sex=rep(final$ER32000,each=5)
l.final$sex=sex

View(l.final)
```

```{r univariate analysis} 
###univariate analysis
freq=list()
for (i in 1:5) {
  data_time <- l.final %>% filter(time == i) %>% select(., -PID)
  fr_pr=function(x){
    a=table(x, useNA = "always")
    b=round(prop.table(table(x, useNA="always")) * 100,2)
    return(data.frame(n=a, perc=b))
  }
  freq[[i]] <- lapply(data_time, fr_pr)
}  
freq[[1]] #at wave=1
freq[[2]] #for wave=2
freq[[3]] #for wave=3
freq[[4]] #for wave=4
freq[[5]] #for wave 5

l.filtered = l.final %>% 
              group_by(PID) %>%
              filter(sum(!is.na(risk_behav)) >= 2 
                     &sum(!is.na(multiple)) >= 2 
                     &sum(!is.na(race)) >= 2 
                     &sum(!is.na(education_head)) >= 2)
```
