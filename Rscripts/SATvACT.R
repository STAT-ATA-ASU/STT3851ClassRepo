library(PASWR2)
head(GRADES)
GRADES$act <- round(GRADES$sat/44,0)
head(GRADES)
with(data = GRADES, cor(sat, act))
ggplot(data = GRADES, aes(x = sat, y = gpa)) + 
  geom_point()
ggplot(data = GRADES, aes(x = act, y = gpa)) + 
  geom_point()
cor(GRADES)

mod1 <- lm(gpa ~ act, data = GRADES)
summary(mod1)

mod2 <- lm(gpa ~ sat, data = GRADES)
summary(mod2)

mod3 <- lm(gpa ~ act + sat, data = GRADES)
summary(mod3)

library(car)
vif(mod3)

X <- model.matrix(mod3)
XTX <- t(X)%*%X
XTX
XTXI <- solve(XTX)
XTXI

#####

# consider

y <- seq(from =1, to = 4, length = 10)
x1 <- round(seq(from = 400, to = 1600, length = 10), 0)
x2 <- round(x1/44, 0)

cor(y, x1)
cor(y, x2)
cor(x1, x2)
mod4 <- lm(y ~ x1 + x2)
X <- model.matrix(mod4)
XTX <- t(X)%*%X
XTX
solve(XTX)
