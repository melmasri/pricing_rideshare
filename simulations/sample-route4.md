sample route4
================
Mingze Li 300137754
2025-03-15

``` r
#source('traveltimeCLTfunctions.R')
library(traveltimeCLT)
library(data.table)
```

``` r
trips <- fread('data/trips.csv')
id <- sample(unique(trips$trip),1000)
train = trips[!trips$trip %in% id,]
test =  trips[trips$trip %in% id,]
timeBin_x_edge <- get_timeBin_x_edges(train)
```

``` r
sample <- pressure_test1(id, trips, r = 1001, timeBin_x_edges = timeBin_x_edge, lambda = 0, severity = 0)
pressure_test <- pressure_test1(id, trips, r = 1001, timeBin_x_edges = timeBin_x_edge, lambda = 0.4, severity = 0.9)
plot_CDF_compare(sample[[2]]$real_time, sample[[1]]$dependent_time, "frequency simulation")
```

![](sample-route4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot_CDF_compare(sample[[2]]$real_length,sample[[1]]$simulated_length,"global edge number simulation","total length","CDF of length",60000)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
real_price=price(sample[[2]]$real_time,sample[[2]]$real_length)[,1]
simulated_price<-data.table(simulated_price=price(sample[[1]]$dependent_time,sample[[1]]$simulated_length)[,1],pressured_price=price(pressure_test[[1]]$dependent_time,pressure_test[[1]]$simulated_length)[,1])
plot_CDF_compare(real_price,simulated_price$simulated_price," simulated price","price","CDF of price",100)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
names(train)[c(2,3,5,7,8)]=c("tripID","entry_time","duration_secs","distance_meters","linkID")
train$speed=exp(train$logspeed)
train$timeBin=time_bins_readable(train$entry_time)
fit <- traveltimeCLT(train)
```

    ## Warning in traveltimeCLT(train): 3 trips have less than 1 observation, and will
    ## not be used to estimate autocorrelations, or residual variance parameters

``` r
test = trips[trips$trip %in% id,]
names(test)[c(2,3,5,7,8)]=c("tripID","entry_time","1","distance_meters","linkID")
names(pressure_test[[3]])=c("tripID","linkID","entry_time","distance_meters")
p=predict(fit, test)
```

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

``` r
pressure_p=predict(fit,pressure_test[[3]])
```

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

    ## Warning in aux[(nlink - length(a) + 1):nlink] <- a: number of items to replace
    ## is not a multiple of replacement length

``` r
fit2 <- traveltimeCLT(train, model = 'population')
p2=predict(fit2, test)
pressure_p2=predict(fit2,pressure_test[[3]])
```

``` r
start_times <- test[, .(start_time = entry_time[1]), by = tripID]
pressure_start_time <- (pressure_test[[3]][, .(start_time = entry_time[1]), by = tripID][,2])
pressure_start_time <-pressure_start_time[["start_time"]]
R1=request_R(p,start_times$start_time-300,start_times$start_time,sample[[2]]$real_length,1,risk_free=0)
R2=request_R(p2,start_times$start_time-300,start_times$start_time,sample[[2]]$real_length,1,risk_free=0)
pressure_R1=request_R(pressure_p,pressure_start_time-300,pressure_start_time,pressure_test[[1]]$simulated_length,1,risk_free=0)
pressure_R2=request_R(pressure_p2,pressure_start_time-300,pressure_start_time,pressure_test[[1]]$simulated_length,1,risk_free=0)
all(R1==R2)
```

    ## [1] FALSE

``` r
all(p==p2)
```

    ## [1] FALSE

``` r
all(R1>0)
```

    ## [1] NA

``` r
#all(R1==R2,na.rm = T)
#all(p==p2,na.rm = T)
#which(is.na(p$variance)==T)
```

``` r
plot(sample[[2]]$real_length,R1,pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plot(real_price[1:1000],R1,pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
plot(sample[[2]]$real_time,R1,xlim = c(0, 6000),pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
plot(p$ETA,R1,xlim = c(0, 6000),pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
plot(R1,R2,pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
plot_CDF_compare(sample[[2]]$real_time,p$ETA,"CLT model expectation")
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
plot(density(na.omit(R1)))
```

![](sample-route4_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

``` r
plot(simulated_price$simulated_price,simulated_price$pressured_price,pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(pressure_test[[2]]$real_length,pressure_R1[1:1000],pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
plot(real_price[1:1000],pressure_R1[1:1000],pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
plot(pressure_test[[2]]$real_length,pressure_R1[1:1000],xlim = c(0, 6000),pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
plot(pressure_p$ETA,pressure_R1,xlim = c(0, 6000),pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

``` r
plot(pressure_R1,pressure_R2,pch = 16,cex = 0.6)
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->

``` r
plot_CDF_compare(sample[[2]]$real_time,pressure_p$ETA,x_max = 15000,"CLT model expectation")
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->

``` r
plot(density(na.omit(pressure_R1)))
```

![](sample-route4_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->
