library(rlist)

filepath <- "https://www.math.ntnu.no/emner/TMA4315/2018h/eliteserien2018"
tippeliga <- read.table(file = filepath, header = TRUE,
                        colClasses = c("character", "character", "numeric", "numeric"))

teams <- tippeliga$home[order(unique(tippeliga$home))]

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
model2glm <- glm(goals ~ -1 + X, family = "poisson")

str=c(0,unname(model2glm$coefficients[3:17]))

# calculate both goal rates for all possible matches
goalrates <- data.frame(hometeam=rep(0,240),awayteam=rep(0,240),rh=rep(0,240),ra<-rep(0,240))
intercept <- model2glm$coefficients[1]
homeadv <- model2glm$coefficients[2]
for (i in 1:16){
  for (j in 1:15){
    if (j>=i){
      s <- j+1
    }else{
      s <- j
    }
    k <- 15*(i-1)+j
    goalrates[k,1] <- i
    goalrates[k,2] <- s
    goalrates[k,3] <- exp(homeadv + intercept+str[i]-str[s])
    goalrates[k,4] <- exp(intercept + str[s]-str[i])
  }
}

#simulate seasons
seasons <- list()
simulations <- 1000
season <- data.frame(team = teams, points = rep(0,16), goaldiff = rep(0,16))
pointtables <- rep(list(season),simulations)

for (i in 1:240) {
  print(c("Simulating Match", i, " for ", simulations, " seasons."))
  yh <- rpois(simulations, goalrates[i,3])
  ya <- rpois(simulations,goalrates[i,4])
  gdif <- yh - ya
  winners <- sign(gdif)
  for(j in 1:simulations) {
    homeTeamBool <- pointtables[[j]]$team == teams[goalrates[i,1]]
    awayTeamBool <- pointtables[[j]]$team == teams[goalrates[i,2]]
    pointsH <- pointtables[[j]][homeTeamBool,]$points
    pointsA <- pointtables[[j]][awayTeamBool,]$points
    gdifH <- pointtables[[j]][homeTeamBool,]$goaldiff
    gdifA <-pointtables[[j]][awayTeamBool,]$goaldiff 
    pointtables[[j]][homeTeamBool,]$goaldiff <- gdifH + gdif[j]
    pointtables[[j]][awayTeamBool,]$goaldiff <- gdifA - gdif[j]
    if (winners[j] == -1) {
      # Away wins
      pointtables[[j]][awayTeamBool,]$points <- pointsA + 3
    } else if (winners[j] == 0) {
      # Tie
      pointtables[[j]][homeTeamBool,]$points <- pointsH + 1
      pointtables[[j]][awayTeamBool,]$points <- pointsA + 1
    } else {
      # Home wins
      pointtables[[j]][homeTeamBool,]$points <- pointsH + 3
    }
  }
}
list.save(pointtables,file="seasons.RData")
