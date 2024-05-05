```{r with discrimination as the outcome}
#### Continuous outcome
###unconditional growth model
l.filtered$wave <- l.filtered$time - 1
uc.growth=lmer(discrimination ~ wave + (wave | PID), data = l.filtered, REML = FALSE)
summary(uc.growth)
icc(uc.growth)# no warning about singularity 

### conditional growth model with main IV polysubstance use only
c.iv1=lmer(discrimination ~ as.factor(multiple)*wave + (wave | PID), data = l.filtered, REML = FALSE)
summary(c.iv1) 
icc(c.iv1)# no warning about singularity 

###conditional growth with main IV and additional time varying predictor
c.iv2=lmer(discrimination ~ as.factor(multiple)*wave + race*wave + race*as.factor(multiple)+ (wave | PID), data = l.filtered, REML = FALSE)
summary(c.iv2) 
icc(c.iv2)# no warning about singularity 
``
 
```{r with polysubstance use as the outcome} 
#### Binary outcome
###unconditional growth model
l.filtered$wave = l.filtered$time - 1
#recoded True to 1 and False to 0
l.filtered$multiple=ifelse(l.filtered$multiple==T,1,
                       ifelse(l.filtered$multiple==F,0,NA))
uc.growth= glmer(multiple ~ wave + (wave | PID), 
                   family = "binomial", 
                   data = l.filtered)
summary(uc.growth) 
icc(uc.growth) # no warnings

### conditional growth model with main IV discrimination only
c.iv1=glmer(multiple ~ discrimination*wave + (1 + wave | PID), 
               family = "binomial", 
               data = l.filtered) 
summary(c.iv1)  
icc(c.iv1)# no warning about singularity 


###conditional growth with main IV and additional time varying predictor
c.iv2 = glmer(multiple ~ discrimination*wave + race*wave + race*discrimination + (1 + wave | PID), 
               family = "binomial", 
               data = l.filtered)
summary(c.iv2) #some warnings but works
icc(c.iv2)# no warning about singularity 

###conditional growth with main IV and additional 2 time varying predictors
c.iv3 = glmer(multiple ~ discrimination*wave 
                          + race*wave 
                          +education_head*wave  
                          + race*discrimination  
                          + race*education_head
                          + discrimination*education_head
                          + discrimination*education_head*race + (1 + wave | PID), 
               family = "binomial", 
               data = l.filtered)
summary(c.iv3) #some warnings but works 
icc(c.iv3)# no warning about singularity 


```
