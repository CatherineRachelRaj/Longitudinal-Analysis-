---
title: "LA project"
output: pdf_document
date: "2024-04-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r library}
library(dplyr)
library(tidyverse)
library(haven)
```


```{r data}
final=read_dta("finalproj_2023.dta")

#alcohol-head is only avaiable upto 2015
#psych prob available upto 2015

## FOR 5 WAVES OF DATA 
#subsetting variables: race, risky behaviors, peer drug use, substances - cocaine, amphetamine, marijuana, steroids, barbiturates, tranquilizers
final_subset=final %>% 
  select(., c("PID","TA111126","TA131218","TA151278", "TA171976","TA192157", #risky behavior
              "TA111056","TA111057", "TA131091","TA131092", "TA151131","TA151132","TA171955","TA171960","TA192131","TA192136", #race and ethnicity
              "TA111020","TA131055","TA151095","TA171928","TA192089", #peer drug use
              "TA110940","TA130973", "TA150995","TA171836","TA191998", #cocaine
              "TA110924","TA130957","TA150979","TA171862","TA192024", #amphetamines
              "TA110932","TA130965", "TA150987", "TA171828", "TA191990", #marijuana
              "TA110936", "TA130969","TA150991", "TA171832", "TA191994", #marijuana freq
              "TA110961", "TA130994","TA151016","TA171870", "TA192032", #steroids
              "TA110945", "TA130978", "TA151000","TA171878", "TA192040", #barbiturates
              "TA110953", "TA130986", "TA151008", "TA171886", "TA192048" #tranquilizers
              ))


###Long form data
#RISKY BEHAVIOR - OUTCOME
l.final= final_subset %>% 
  pivot_longer(., cols=c("TA111126","TA131218","TA151278", "TA171976","TA192157"), values_to = "risk_behav", names_to = "risk_col") %>%
  pivot_longer(., cols= c("TA111020","TA131055","TA151095","TA171928","TA192089"), values_to = "peer_drug_use", names_to = "peer_col") %>% 
  pivot_longer(., cols= c("TA110940","TA130973", "TA150995","TA171836","TA191998"), values_to = "cocaine", names_to = "coc_col") %>%
  pivot_longer(., cols= c("TA110924","TA130957","TA150979","TA171862","TA192024"), values_to = "amphetamines", names_to = "amp_col") %>%
  pivot_longer(., cols= c("TA110932","TA130965", "TA150987", "TA171828", "TA191990"), values_to = "marijuana", names_to = "marj_col") %>%
  pivot_longer(., cols= c("TA110936", "TA130969","TA150991", "TA171832", "TA191994"), values_to = "marijuana_freq", names_to = "marjf_col") %>%
  pivot_longer(., cols= c("TA110961", "TA130994","TA151016","TA171870", "TA192032"), values_to = "steroids", names_to = "ster_col") %>%
  pivot_longer(., cols= c("TA110945", "TA130978", "TA151000","TA171878", "TA192040"), values_to = "barbiturates", names_to = "barb_col") %>%
  pivot_longer(., cols= c("TA110953", "TA130986", "TA151008", "TA171886", "TA192048"), values_to = "tranquilizers", names_to = "tranq_col") %>%
  pivot_longer(., cols= c("TA111056","TA131091","TA151131","TA171960", "TA192136"), values_to = "hispanic", names_to = "hisp_col") %>%
  pivot_longer(., cols= c("TA111057","TA131092","TA151132","TA171955", "TA192131"), values_to = "race", names_to = "rac_col") %>%
   mutate(time = case_when( #time=wave
    name == "TA111126" ~ 1,
    name == "TA131218" ~ 2,
    name == "TA151278" ~ 3,
    name == "TA171976" ~ 4,
    name == "TA192157" ~ 5
  )) 

View(l.final)
```
