```{r}
# a growth model with the main IV only
c.growth.1 <- lmer(risk_behav~ discrimination * time + (time | PID), data = l.filtered, REML = FALSE)
summary(c.growth.1)

# AIC
AIC(c.growth.1)
# BIC
BIC(c.growth.1)
#  Deviance
-2*logLik(c.growth.1)
#Likelihood
logLik(c.growth.1)
# ICC Calculation
icc(c.growth.1)
```

```{r}
# a growth model with the main IV only
c.growth.2 <- lmer(risk_behav~ multiple * time + (time | PID), data = l.filtered, REML = FALSE)
summary(c.growth.2)

# AIC
AIC(c.growth.2)
# BIC
BIC(c.growth.2)
#  Deviance
-2*logLik(c.growth.2)
#Likelihood
logLik(c.growth.2)
# ICC Calculation
icc(c.growth.2)
```
