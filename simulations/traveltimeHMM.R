library(traveltimeHMM)
data(tripset)
head(tripset)
#>   tripID linkID timeBin logspeed traveltime    length                time
#> 1   2700  10469 Weekday 1.692292  13.000000  70.61488 2014-04-28 06:07:27
#> 2   2700  10444 Weekday 2.221321  18.927792 174.50487 2014-04-28 06:07:41
#> 3   2700  10460 Weekday 2.203074   8.589937  77.76295 2014-04-28 06:07:58
#> 4   2700  10462 Weekday 1.924290  14.619859 100.15015 2014-04-28 06:08:07
#> 5   2700  10512 Weekday 1.804293   5.071986  30.81574 2014-04-28 06:08:21
#> 6   2700   5890 Weekday 2.376925  31.585355 340.22893 2014-04-28 06:08:26

fit2 <- traveltimeHMM(data = tripset,nQ = 2,max.it = 20, model = "HMM")
single_trip2 <- subset(tripset, tripID==2700)
pred <- predict(object = fit2,
                tripdata = single_trip2,
                starttime = single_trip2$time[1],
                n = 1000)
hist(pred, freq = FALSE)
which(levels(fit2$factors) == paste(single_trip2$linkID[1],
                                   time_bins(single_trip2$time[1]),
                                  sep = "."), useNames = FALSE)
dependent_uniform<-function(n, rho=0.31) {
  S <-diag(n)
  if(n>1){
    for (i in 1:n) {
      for (j in 1:n) {
        S[i, j] <- rho^(abs(i-j))
      }
    }
    diag(S)<-1
    #print(S)
    U = c(pnorm(rmvnorm(1, sigma = S)))
  }else U = runif(1)
  U
}
