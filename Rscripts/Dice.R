#set.seed(3)
roll1 <- sample(1:6, 100000, replace = TRUE)
roll2 <- sample(1:6, 100000, replace = TRUE)
STR <- roll1 + roll2
A <- STR == 6
B <- roll1 == 3
PB <- mean(B)
PB
PA <- mean(A)
PAB <- mean(A & B)
PAB
(ans <- PAB/PB)

######

S <- expand.grid(roll1 = 1:6, roll2 = 1:6)
S$Sum <- apply(S, 1, sum)
S
table(S$Sum)
T1 <- MASS::fractions(table(S$Sum)/dim(S)[1])
T1
(x <- as.numeric(names(T1)))
(px <- T1)
(EX <- sum(x*px))
######
VX <- sum((x - EX)^2*px)
VX

##############

roll1 <- sample(1:6, 1000000, replace = TRUE)
roll2 <- sample(1:6, 1000000, replace = TRUE)
STR <- roll1 + roll2
mean(STR)
var(STR)


###################

rdf <- function(SIZE = 10000){
  roll1 <- sample(1:6, size = SIZE, replace = TRUE)
  roll2 <- sample(1:6, size = SIZE, replace = TRUE)
  STR <- roll1 + roll2
  A <- STR == 6
  B <- roll1 == 3
  PB <- mean(B)
  PA <- mean(A)
  PAB <- mean(A & B)
  ans <- PAB/PB
  ans
}
rdf()

ans <- replicate(1000, rdf())
hist(ans)
