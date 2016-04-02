pd = read.csv(file.choose()) ### choose panel ###

library(plyr)
library(psych)
library(multcomp)
library(rms)
library(lme4)

pd$rjoblose = 5- pd$joblose

vars = c("rjoblose", "degree", "age", "realinc", "panelwave", "idnum")
sub = pd[,vars]

# Overall trend in job lose concern
library(ggplot2)
g_trend <- ggplot(sub, aes(x = panelwave, y = rjoblose))
(g_trend <- g_trend + stat_summary(fun.y=mean, geom="line", lwd = 1.25))

# Empirical growth curves for idnum < 200 (& overall)
g_growth <- ggplot(subset(sub, idnum<200), 
                   aes(x = panelwave, y = rjoblose, group = idnum, color = factor(idnum)))
no_legend <- theme(legend.position="none")

g_id <- g_growth + geom_line() + no_legend 
g_id + stat_summary(fun.y=mean, geom="line", aes(group=1), lty = 2, color="black") 


# individual regression lines for idnum < 200 (& overall)
g_reg <- g_growth + stat_smooth(method = lm, se = F) + no_legend
g_reg + stat_summary(fun.y=mean, geom="smooth", aes(group=1), lty = 2, color = "black")



# ols with clustered & robust SEs
summary(lm(rjoblose ~ factor(panelwave), sub))

robcov(ols(rjoblose ~ factor(panelwave), x = T, y = T, data = sub), 
       cluster = sub$idnum)

##random effects
lmer.joblose1 <- lmer(rjoblose ~ panelwave + (1|idnum), data = sub, REML = F)
summary(lmer.joblose1)

##random slopes too
lmer.joblose2 <- lmer(rjoblose ~ panelwave + (1 + panelwave | idnum), data = sub, REML = F)
summary(lmer.joblose2)

## are random slopes necessary?
anova(lmer.joblose1, lmer.joblose2)

## set college-educated
sub$ba=ifelse(sub$degree>=3,1,0)
table(sub$ba)

# include college-educated
lmer.joblose3<- update(lmer.joblose1, ~ . + ba)
summary(lmer.joblose3)

## change in slopes for ba vs. not ba
lmer.joblose4 <- update(lmer.joblose3, ~ . + ba:panelwave)
summary(lmer.joblose4)

## look for variability in slopes for bas
lmer.joblose5 <- lmer(rjoblose ~ panelwave + (1 + panelwave | idnum), data = sub, REML = F, subset= sub$ba==1)
summary(lmer.joblose5 )

## look for variability in slopes for not bas
lmer.joblose6 <- lmer(rjoblose ~ panelwave + (1 + panelwave | idnum), data = sub, REML = F, subset= sub$ba==0)
summary(lmer.joblose6)