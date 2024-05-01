```{r library}
library(lme4)
```
```{r unconditional models}
#unconditional mean model
uc.mean=lmer(risk_behav ~ 1 + (1 |PID), data = l.filtered,REML = FALSE)
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
uc.growth=lmer(risk_behav~ time + (time | PID), data = l.filtered, REML = FALSE)
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
```
