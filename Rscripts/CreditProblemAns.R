## ---- label = "SETUP", echo = FALSE, results= 'hide', message = FALSE, warning = FALSE----
set.seed(123)
library(knitr)
knitr::opts_chunk$set(comment = NA, fig.show = 'as.is', fig.align = 'center', fig.height = 5, fig.width = 5, prompt = TRUE, highlight = TRUE, tidy = FALSE, warning = FALSE, message = FALSE, tidy.opts=list(blank = TRUE, width.cutoff= 75, cache = TRUE))
library(ISLR)


## ---------------------------------------------------------------------------------------
Credit$Utilization <- Credit$Balance / (Credit$Income*100)
DT::datatable(Credit[, -1], rownames = FALSE)


## ---------------------------------------------------------------------------------------
library(MASS)
mod.fs <- stepAIC(lm(Rating ~ 1, data = Credit), scope = .~Income + Limit + Cards + Age + Education + Gender + Student + Married + Ethnicity + Balance + Utilization, direction = "forward", test = "F")
mod.fs
mod.be <- stepAIC(lm(Rating ~ Income + Limit + Cards + Age + Education + Gender + Student + Married + Ethnicity + Balance + Utilization, data = Credit), direction = "backward", test = "F")
mod.be
# OR 
null <- lm(Rating ~ 1, data = Credit)
full <- lm(Rating ~ ., data = Credit)
mod.fs <- stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")
mod.be <- stepAIC(full, scope = list(lower = null, upper = full), direction = "backward", test = "F")
summary(mod.fs)
summary(mod.be) 
car::vif(mod.be)
car::vif(mod.fs)


## ---------------------------------------------------------------------------------------
mod <- lm(Rating ~ Limit + Cards + Married + Student + Education, data = Credit)
summary(mod)
par(mfrow = c(2, 2))
plot(mod)
par(mfrow = c(1, 1))
car::residualPlots(mod)
modN <- lm(Rating ~ poly(Limit, 2, raw = TRUE) + poly(Cards, 2, raw = TRUE) + Married + Student + Education, data = Credit)
summary(modN)
car::residualPlots(modN)
car::vif(modN)
summary(modN)


## ---------------------------------------------------------------------------------------
predict(mod, newdata = data.frame(Limit = 6000, Cards = 4, Married = "Yes", Student = "No", Education = 16))


## ---------------------------------------------------------------------------------------
predict(modN, newdata = data.frame(Limit = 6000, Cards = 4, Married = "Yes", Student = "No", Education = 16))
### Should be the same as:
coef(modN)[1] + coef(modN)[2]*6000 + coef(modN)[3]*6000^2 + coef(modN)[4]*4 + coef(modN)[5]*4^2 + coef(modN)[6]*1 + coef(modN)[7]*0 + coef(modN)[8]*16
predict(modN, newdata = data.frame(Limit = 12000, Cards = 2, Married = "Yes", Student = "No", Education = 8), response = "pred")


## ---------------------------------------------------------------------------------------
predict(mod, newdata = data.frame(Limit = 12000, Cards = 2, Married = "Yes", Student = "No", Education = 8))


## ---------------------------------------------------------------------------------------
predict(modN, newdata = data.frame(Limit = 12000, Cards = 2, Married = "Yes", Student = "No", Education = 8))


## ---------------------------------------------------------------------------------------
# Using predict function first:
predict(mod, newdata = data.frame(Limit = 6000, Cards = 4, Married = "Yes", Student = "No", Education = 16), interval = "conf", level = 0.95)


## ---------------------------------------------------------------------------------------
X <- model.matrix(mod)
XTXI <- solve(t(X)%*%X)
XTXI
betahat <- coef(mod)
betahat <- matrix(betahat, nrow = 6)
dim(betahat)
# For 3
Xh <- matrix(c(1, 6000, 4, 1, 0, 16), nrow = 1)
(Yhath <- Xh %*%betahat)
anova(mod)
MSE <- anova(mod)[6,3]
varcovB <- MSE*XTXI
(s2yhath <- Xh %*% varcovB %*% t(Xh))
syhath <- sqrt(s2yhath)
(crit_t <- qt(0.975, df.residual(mod)))
CI_EYh <- c(Yhath - crit_t*syhath, Yhath + crit_t*syhath)
CI_EYh
######
# For 4

# Using predict function first:
predict(mod, newdata = data.frame(Limit = 12000, Cards = 2, Married = "Yes", Student = "No", Education = 8), interval = "conf", level = 0.95)
######
Xh <- matrix(c(1, 12000, 2, 1, 0, 8), nrow = 1)
(Yhath <- Xh %*%betahat)
MSE <- anova(mod)[6,3]
varcovB <- MSE*XTXI
(s2yhath <- Xh %*% varcovB %*% t(Xh))
syhath <- sqrt(s2yhath)
(crit_t <- qt(0.975, df.residual(mod)))
CI_EYh <- c(Yhath - crit_t*syhath, Yhath + crit_t*syhath)
CI_EYh

