source('myglm.R')
filepath <- "https://www.math.ntnu.no/emner/TMA4315/2018h/eliteserien2018"
tippeliga <- read.table(file = filepath, header = TRUE,
                        colClasses = c("character", "character", "numeric", "numeric"))
X <- matrix(0,nrow=384, ncol=18)
goals <- vector(length = 384)

X[,1] <- rep(1,384)
X[,2] <- rep(c(1,0),192)
teams <- tippeliga$home[order(unique(tippeliga$home))]
colnames(X) <- c("intercept","homeadvantage",teams)

for (i in 1:length(tippeliga[,1])) {
  home <- tippeliga$home[i]
  away <- tippeliga$away[i]
  # Home team
  X[2*i-1, home] <- 1
  X[2*i-1, away] <- -1
  # Away team
  X[2*i, home] <- -1
  X[2*i, away] <- 1
  # Goals
  goals[2*i-1] <- tippeliga$yh[i]
  goals[2*i] <- tippeliga$ya[i]
}
X <- X[,c(1:2,4:18)] # remove linear dependency
model2b <- myglm(goals ~ -1 + X, family = "poisson")
summary(model2b)

model2glm <- glm(goals ~ -1 + X, family = "poisson")
summary(model2glm)
