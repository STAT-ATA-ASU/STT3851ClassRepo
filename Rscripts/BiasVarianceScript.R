


LWD <- 0.1
Ntrains <- 1000   # Number of training sets to generate
n <- 40           # Number of observations to generate for each training set
dpt <- 5*9*5      # Number of x points to predict over
SD <- sqrt(.25)

par(mfrow= c(2, 2))

xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
plot(xs, ys, pch = 19, col = "blue", xlim = c(5, 9), ylim = c(-2, 2), cex = .25)
abline(lm(ys ~ 1))
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
plot(xs, ys, pch = 19, col = "blue", xlim = c(5, 9), ylim = c(-2, 2), cex = 0.25)
abline(lm(ys ~ 1))
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
plot(xs, ys, pch = 19, col = "blue", xlim = c(5, 9), ylim = c(-2, 2), cex = 0.25)
abline(lm(ys ~ 1))
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
plot(xs, ys, pch = 19, col = "blue", xlim = c(5, 9), ylim = c(-2, 2), cex = 0.25)
abline(lm(ys ~ 1))

par(mfrow = c(1, 1))

curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
dim(yhn)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  error <- rnorm(n, 0, SD)
  ys <- sin(xs) + error
  mod1 <- lm(ys ~ 1)
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  error <- rnorm(n, 0, SD)
  yst <- sin(xs) + error
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest0 <- mean(MSEtest)
avgMSEtrain0 <- mean(MSEtrain)


########


curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ xs)
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest1 <- mean(MSEtest)
avgMSEtrain1 <- mean(MSEtrain)

######

curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 2))
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest2 <- mean(MSEtest)
avgMSEtrain2 <- mean(MSEtrain)

#########

curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 3))
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest3 <- mean(MSEtest)
avgMSEtrain3 <- mean(MSEtrain)

################

curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 5))
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest5 <- mean(MSEtest)
avgMSEtrain5 <- mean(MSEtrain)

############

curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 10))
  nys <- predict(mod1)
  lines(xs, nys, col = "pink", lty = "dashed", lwd = LWD)
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - nys)^2)
  MSEtrain[i] <- mean((ys - nys)^2)
}
yhnbar <- apply(yhn, 2, mean)
curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
lines(nxs, yhnbar, col = "red", lwd = 2)
avgMSEtest10 <- mean(MSEtest)
avgMSEtrain10 <- mean(MSEtrain)

#####

MSE <- c(avgMSEtrain0, avgMSEtest0, avgMSEtrain1, avgMSEtest1, avgMSEtrain2, avgMSEtest2, avgMSEtrain3, avgMSEtest3, avgMSEtrain5, avgMSEtest5, avgMSEtrain10, avgMSEtest10)
Group <- c(rep(c("Train", "Test"), 6))
Flex <- c(rep(c(0, 1, 2, 3, 5, 10), each = 2))
DF <- data.frame(MSE, Group, Flex)
library(ggplot2)
ggplot(data = DF, aes(x = Flex, y = MSE, color = Group)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  geom_hline(yintercept = SD^2, linetype = "dashed")

######################

#### With GGPLOT2 now

Ntrains <- 250    # Number of training sets to generate
n <- 40           # Number of observations to generate for each training set
dpt <- 5*9*5      # Number of x points to predict over
SD <- 0.5
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p1 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p2 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p3 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p4 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

#####################

p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ 1)
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)