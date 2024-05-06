```{r with peer_drug_use as the outcome} 
#### Continuous outcome - peer_drug_use used for high-risk social ties
###unconditional growth model
l.filtered$wave <- l.filtered$time - 1
uc.growth=lmer(peer_drug_use ~ wave + (wave | PID), data = l.filtered, REML = FALSE)
summary(uc.growth)
icc(uc.growth)# no warning about singularity 

### conditional growth model with main IV polysubstance use only
c.iv1=lmer(peer_drug_use ~ discrimination*wave + (wave | PID), data = l.filtered, REML = FALSE)
summary(c.iv1) 
icc(c.iv1)# no warning about singularity 

###conditional growth with main IV and additional time varying predictor
c.iv2=lmer(peer_drug_use ~ discrimination*wave + race*wave + race*discrimination + (wave | PID), data = l.filtered, REML = FALSE)
summary(c.iv2) 
icc(c.iv2)# no warning about singularity 

###conditional growth with main IV and additional time varying predictor
c.iv3=lmer(peer_drug_use ~ discrimination*wave 
                           + race*wave 
                           + education_head*wave
                           + race*discrimination 
                           + race*education_head
                           + discrimination*education_head
                           + (wave | PID), data = l.filtered, REML = FALSE)
summary(c.iv3) 
icc(c.iv3)# no warning about singularity 

```
