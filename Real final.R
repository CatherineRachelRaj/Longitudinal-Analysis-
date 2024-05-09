```{r with peer_drug_use as the outcome} 
#### Continuous outcome - peer_drug_use used for high-risk social ties
l.filtered<- within(l.filtered, {
  discrimination <- factor(discrimination,levels = 1:6)
  race <- factor(race, levels = 0:4, labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other"))
  education_head <- factor(education_head, levels = 0:3, labels = c("<=High School", "Some College", "4- Year College", "Any Post Graduate Work"))
})

#unconditonal mean model
uc.mean=lmerTest::lmer(peer_drug_use~1 + (1 | PID), data=l.filtered,REML=FALSE)
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


###unconditional growth model
uc.growth=lmerTest::lmer(peer_drug_use~ age + (age | PID), data=l.filtered,REML=FALSE)
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


### conditional growth model with main IV education_head only
c.iv1=lmerTest::lmer(peer_drug_use~ education_head*age+ (age | PID), 
            data=l.filtered, 
            REML=FALSE)

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

###conditional growth with main IV and all additional time-invariant predictors
c.iv2=lmerTest::lmer(peer_drug_use~ education_head*age +
                       race + discrimination +
                      (age|PID), 
            data=l.filtered, 
            REML = FALSE)

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
```

```{r plots}
#plot for unconditional growth model
age.levels=round(as.numeric(levels(as.factor(l.filtered$age))),0)
fixedef.grow=fixef(uc.growth)
pdu=fixedef.grow[[1]]+fixedef.grow[[2]]*age.levels

grow.model=data.frame(Age=age.levels, Social_Ties=pdu)
p1=ggplot(data=grow.model, aes(x=Age, y=Social_Ties))+
          geom_line(col="purple") +
          ggtitle("Unconditional Growth model") +
          theme_classic()+
          labs(x="Age", y="High-Risk Social Ties")
p1 + theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center, bold, and increase title font size
  axis.title.x = element_text(size = 12, face="bold"),  # Increase font size for x-axis label
  axis.title.y = element_text(size = 12, face="bold")   # Increase font size for y-axis label
)

#plot for conditional model with main IV only
fixedc.iv1=fixef(c.iv1)
dis1=fixedc.iv1[[1]]+fixedc.iv1[[5]]*age.levels
dis2=fixedc.iv1[[1]]+fixedc.iv1[[5]]*age.levels+fixedc.iv1[[2]]+fixedc.iv1[[6]]*age.levels
dis3=fixedc.iv1[[1]]+fixedc.iv1[[5]]*age.levels+fixedc.iv1[[3]]+fixedc.iv1[[7]]*age.levels
dis4=fixedc.iv1[[1]]+fixedc.iv1[[5]]*age.levels+fixedc.iv1[[4]]+fixedc.iv1[[8]]*age.levels

ed_labels=c("<=High School", "Some College", "4 year College Degree", "Any Post Graduate")
civ1.model=data.frame(Age=rep(age.levels, times=4), Social_ties=c(dis1,dis2,dis3,dis4), Education=rep(ed_labels,each=5))
civ1.model$Education= as.factor(civ1.model$Education)

p2=ggplot(data=civ1.model, aes(x=Age, y=Social_ties, col=Education))+
   geom_line() + 
   ggtitle("Growth Plot of High-Risk Social Ties for \nEach level of Education of Household Head")+
   theme_classic()+
   labs(x="Age",y="High-Risk Social Ties")
p2 + theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 15),  # Center, bold, and increase title font size
  axis.title.x = element_text(size = 12, face="bold"),  # Increase font size for x-axis label
  axis.title.y = element_text(size = 12, face="bold")   # Increase font size for y-axis label
)


###plot for conditional model with main IV and additional time-invariant predictors

fixedef.iv2=fixef(c.iv2)
#for race = Hispanic controlling for discrimination
r0dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels #race0 and ed=0
r0dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels #race=0 and ed=1
r0dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels
r0dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels

#for race=Non-Hispanic White
r1dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[6]] #r-1 and ed=0
r1dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[6]] #r=1 and ed=1
r1dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[6]] #r=1 and ed=2
r1dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[6]]

#for race=Non-Hispanicn Black 
r2dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[7]]
r2dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[7]]
r2dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[7]]
r2dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[7]]


#for race=Non-Hispanic Asian
r3dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[8]]
r3dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[8]]
r3dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[8]]
r3dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[8]]


#for race=Non-Hispanic Other
r4dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[9]]
r4dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[9]]
r4dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[9]]
r4dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[9]]

race_labels=c("Hispanic", "Non-Hispanic White","Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other")
race_model=data.frame(Age=rep(age.levels, times=20),
                      Social_ties=c(r0dis1,r0dis2,r0dis3,r0dis4,
                                    r1dis1,r1dis2,r1dis3,r1dis4,
                                    r2dis1,r2dis2,r2dis3,r2dis4,
                                    r3dis1,r3dis2,r3dis3,r3dis4,
                                    r4dis1,r4dis2,r4dis3,r4dis4),
                      Education=rep(ed_labels, each=5),
                      Race=rep(race_labels,each=20))
race_model$Education= as.factor(race_model$Education)


p3=ggplot(data = race_model) +
  geom_line(aes(x = Age, y = Social_ties, col =Education)) +
  facet_wrap(~ Race)+ggtitle("Growth plot of High-Risk Social Ties for \n level of Education of Household Head \n for Different Levels of Race \n (controlling for Perceived Discrimination score)")+
  theme_classic()+
  labs(y="High-Risk Social Ties")
  
p3 + theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 15),  # Center, bold, and increase title font size
  axis.title.x = element_text(size = 12, face="bold"),  # Increase font size for x-axis label
  axis.title.y = element_text(size = 12, face="bold")   # Increase font size for y-axis label
)

#for discr=1 controlling for race
ed0dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels
ed0dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels
ed0dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels
ed0dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels


#for discr=2 controlling for race
ed1dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels++fixedef.iv2[[10]]
ed1dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[10]]
ed1dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[10]]
ed1dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[10]]

#for discr=3 controlling for race
ed2dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels++fixedef.iv2[[11]]
ed2dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[11]]
ed2dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[11]]
ed2dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[11]]


#for discr=4 controlling for race
ed3dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels++fixedef.iv2[[12]]
ed3dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[12]]
ed3dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[12]]
ed3dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[12]]

#for discr=5 controlling for race
ed4dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels++fixedef.iv2[[13]]
ed4dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[13]]
ed4dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[13]]
ed4dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[13]]

#for discr=6 controlling for race
ed5dis1=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels++fixedef.iv2[[14]]
ed5dis2=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[2]]+fixedef.iv2[[15]]*age.levels+fixedef.iv2[[14]]
ed5dis3=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[3]]+fixedef.iv2[[16]]*age.levels+fixedef.iv2[[14]]
ed5dis4=fixedef.iv2[[1]]+fixedef.iv2[[5]]*age.levels+fixedef.iv2[[4]]+fixedef.iv2[[17]]*age.levels+fixedef.iv2[[14]]



ed_labels=c("<=High School", "Some College", "4 year College Degree", "Any Post Graduate")

ed_model= data.frame(Age=rep(age.levels, times=24),
                     Social_ties=c(ed0dis1,ed0dis2,ed0dis3,ed0dis4,
                                   ed1dis1,ed1dis2,ed1dis3,ed1dis4,
                                   ed2dis1,ed2dis2,ed2dis3,ed2dis4,
                                   ed3dis1,ed3dis2,ed3dis3,ed3dis4,
                                   ed4dis1,ed4dis2,ed4dis3,ed4dis4,
                                   ed5dis1,ed5dis2,ed5dis3,ed5dis4),
                     Education=rep(ed_labels, each=5), 
                     Discrimination=rep(1:6, each=20)
              )
ed_model$Education= as.factor(ed_model$Education)
ed_model$Discrimination= as.factor(ed_model$Discrimination)


p4=ggplot(data = ed_model) +
  geom_line(aes(x = Age, y = Social_ties, col = Education)) +
  facet_wrap(~Discrimination)+ggtitle("Growth plot of High-Risk Social Ties for \n each level of Education of Household Head \n for each score of Discrimination \n (controlling for Race)")+
  theme_classic()+
  labs(y="High-Risk Social Ties")
  
p4 + theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 15),  # Center, bold, and increase title font size
  axis.title.x = element_text(size = 12, face="bold"),  # Increase font size for x-axis label
  axis.title.y = element_text(size = 12, face="bold")   # Increase font size for y-axis label
)

```
