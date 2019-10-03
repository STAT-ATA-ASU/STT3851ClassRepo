# Transformations on predictors...

library(PASWR2)
library(tidyverse)

ggplot(data = SIMDATAXT, aes(x = x1, y = y)) + 
  geom_point() + 
  theme_bw()
#
ggplot(data = SIMDATAXT, aes(x = x1, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
# Not a bad approximation or not?
#
library(car)
mod1 <- lm(y ~ x1, data = SIMDATAXT)


residualPlots(mod1)
#
mod2 <- lm(y ~ I(x1^.5), data = SIMDATAXT)
residualPlots(mod2)
summary(mod2)
####
ggplot(data = SIMDATAXT, aes(x = x2, y = y)) + 
  geom_point() + 
  theme_bw()
#
ggplot(data = SIMDATAXT, aes(x = x2, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
# Not a bad approximation or not?
mod3 <- lm(y ~ x2, data = SIMDATAXT)
residualPlots(mod3)
#
mod4 <- lm(y ~ I(x2^2), data = SIMDATAXT)
residualPlots(mod4)
summary(mod4)
#
ggplot(data = SIMDATAXT, aes(x = x3, y = y)) + 
  geom_point() + 
  theme_bw()
###
ggplot(data = SIMDATAXT, aes(x = x3, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
# Not a bad approximation or not?---Clearly BAD!
mod5 <- lm(y ~ x3, data = SIMDATAXT)
residualPlots(mod5)
#
mod6 <- lm(y ~ I(1/x3), data = SIMDATAXT)
residualPlots(mod6)
#
ggplot(data = SIMDATAXT, aes(x = 1/x3, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 
#
summary(mod6)
##### Transformations on the RESPONSE

ggplot(data = SIMDATAST, aes(x = x1, y = y1)) + 
  geom_point() + 
  theme_bw()

# Consider boxCox()
modx1 <- lm(y1 ~ x1, data = SIMDATAST)
residualPlots(modx1)
boxCox(modx1)
boxCox(modx1, lambda = seq(from = -0.2, to = 0.2, by = 0.01))
# Use a log transformation
modxl <- lm(log(y1) ~ x1, data = SIMDATAST)
boxCox(modxl, lambda = seq(0.7, 1.3, .01))
#
ggplot(data = SIMDATAST, aes(x = x1, y = log(y1))) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
##
residualPlots(modxl)
#####
ggplot(data = SIMDATAST, aes(x = x2, y = y2)) + 
  geom_point() + 
  theme_bw()

modx2 <- lm(y2 ~ x2, data = SIMDATAST)
boxCox(modx2)
boxCox(modx2, lambda = seq(-1.3, -0.6, 0.05))
## use -1.0
ggplot(data = SIMDATAST, aes(x = x2, y = 1/y2)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
#
modx2f <- lm(I(1/y2) ~ x2, data = SIMDATAST)
residualPlots(modx2f)
summary(modx2f)
