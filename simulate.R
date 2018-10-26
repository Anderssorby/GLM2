source('test_myglm.R')
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

for (i in 1:240) {
  cat("Simulating Match", i, "for", simulations, "seasons.\n")
  yh <- rpois(simulations, goalrates[i,3])
  ya <- rpois(simulations,goalrates[i,4])
  gdif <- yh - ya
  winners <- sign(gdif)
  for(j in 1:simulations) {
    pointtable <- data.frame(team = teams, points = rep(0,16), goaldiff = rep(0,16))
    homeTeamBool <- pointtable$team == teams[goalrates[i,1]]
    awayTeamBool <- pointtable$team == teams[goalrates[i,2]]
    pointsH <- pointtable[homeTeamBool,]$points
    pointsA <- pointtable[awayTeamBool,]$points
    gdifH <- pointtable[homeTeamBool,]$goaldiff
    gdifA <-pointtable[homeTeamBool,]$goaldiff 
    pointtable[homeTeamBool,]$goaldiff <- gdifH + gdif[j]
    pointtable[awayTeamBool,]$goaldiff <- gdifA - gdif[j]
    if (winners[j] == -1) {
      # Away wins
      pointtable[awayTeamBool,]$points <- pointsA + 3
    } else if (winners[j] == 0) {
      # Tie
      pointtable[homeTeamBool,]$points <- pointsH + 1
      pointtable[awayTeamBool,]$points <- pointsA + 1
    } else {
      # Home wins
      pointtable[homeTeamBool,]$points <- pointsH + 3
    }
    seasons<-c(seasons, pointtable)
  }
  
}
saveRDS(seasons,file="seasons.dat")