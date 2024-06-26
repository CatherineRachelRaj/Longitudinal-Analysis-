Multi-level model: 

Level 1 model: Level 1 model describes within person change i.e change in person over time. For outcome Risky Behavior, level 1 model is:

$$ Social \quad ties_{ij} = \pi_{0i} + \pi_{1i}(Wave)+\epsilon_{ij}$$
where $Social \quad ties _{ij}$ is the level of high-risk social ties of participant i at wave j, $\pi_{0i}$ is the initial level of high-risk social ties, $\pi_{1i}(Wave)$ is slope i.e rate of change in individual given by wave and $\epsilon_{ij}$ is the random error for participant at time j 


Level 2 model describes the between person differences in change. For outcome Depression and time invariant predictors race and sex, level 2 model is:
Intercept model: 
$$
\begin{align}
&\pi_{0i} = \gamma_{00} + \gamma_{01}(Race)_i  \\
& + \gamma_{02}(Education\quad of \quad head)_i \\
& + \gamma_{03}(Discrimination)_i \\
& + \gamma_{04}(Race*Education\quad of \quad head)_i  \\
& + \gamma_{05}(Discrimination * Education\quad of \quad head)_i \\
& + \gamma_{06}(Race*Discrimination*Education\quad of \quad head)_i +\zeta_{0i}
\end{align}
$$
Slope model:
$$
\begin{align}
&\pi_{1i} = \gamma_{10} + \gamma_{11}(Race)_i  \\
&+ \gamma_{12}(Education\quad of \quad head)_i \\
& + \gamma_{13}(Discrimination)_i \\
&+ \gamma_{14}(Race*Education\quad of \quad head)_i  \\
&+ \gamma_{15}(Discrimination * Education\quad of \quad head)_i \\
&+ \gamma_{16}(Race*Discrimination*Education\quad of \quad head)_i +\zeta_{0i}
\end{align}
$$


where $\pi_{0i}$ and $\pi_{1i}$ are intercept and slope parameters for individual i. 
$\gamma_{00}$ and $\gamma_{10}$ are fixed effect intercepts representing the average intercept and slope across all groups.
$\gamma_{01}$, $\gamma_{02}$ and $\gamma_{03}$are fixed effects coefficients representing the effects of race, education level of household head and everyday discrimination on intercept parameters.
$\gamma_{11}$, $\gamma_{12}$ and $\gamma_{13}$are fixed effects coefficients representing the effects of race, education level of household head and everyday discrimination on slope parameters. 
$\zeta_{0i}$ and $\zeta_{1i}$ are random effects representing the individual-level variability in the intercept and slope parameters respectively. 


Composite model:

$$
\begin{align}
& Y_{ij} = [\gamma_{00}+\gamma_{10}(Wave)_{ij}+ \gamma_{01}(Race)_i \\
& + \gamma_{02}(Education\quad of \quad head)_i \\
& +\gamma_{03}(Discrimination)_i \\
& +\gamma_{04}(Race*Education \quad of \quad head)_i \\
& +\gamma_{05}(Discrimination*Education \quad of \quad head)_i \\
& +\gamma_{06}(Race*Discrimination)_i \\
& +\gamma_{07}(Race_i*Discimination_i*Education\quad of \quad head_i*Wave_{ij})] \\
& +[\zeta_{0i}+\zeta_{1i}(Wave)_{ij}+\epsilon_{ij}]
\end{align}
$$


```{r library}
library(lme4)
```
```{r unconditional models}
#unconditional mean model
uc.mean=lmer(peer_drug_use ~ 1 + (1 |PID), data = l.filtered,REML = FALSE)
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
l.filtered$wave <- l.filtered$time - 1
uc.growth=lmer(peer_drug_use ~ wave + (wave | PID), data = l.filtered, REML = FALSE)
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
