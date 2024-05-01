```{r}
#unconditional mean model
uc.mean=lmer(risk_behav ~ 1 + (1 |PID), data = l.filtered,REML = FALSE)
summary(uc.mean)

#unconditional growth model
uc.growth=lmer(risk_behav~ time + (time | PID), data = l.filtered, REML = FALSE)
summary(uc.growth)

```
