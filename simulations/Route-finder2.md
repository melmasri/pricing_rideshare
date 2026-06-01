Route Finder2
================
Mingze Li 300137754
2025-02-14

``` r
library(traveltimeCLT)
library(data.table)
library(traveltimeHMM)
```

    ## 
    ## Attaching package: 'traveltimeHMM'

    ## The following objects are masked from 'package:traveltimeCLT':
    ## 
    ##     rules2timebins, time_bins, time_bins_functional,
    ##     time_bins_readable, to7daybins

``` r
library(doParallel)
```

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:traveltimeHMM':
    ## 
    ##     time_bins

    ## The following object is masked from 'package:traveltimeCLT':
    ## 
    ##     time_bins

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
trips = fread('data/net_stat.csv')
net <- unique(trips, by = c("linkID", "nextLinkID"))
net <- net[,.(linkID,nextLinkID,length)]
names(net)[3]<-"weight"
filted_net <- trips[trips$fictional==F,]
filted_net <- unique(filted_net, by = c("linkID", "nextLinkID"))
filted_net <- filted_net[,.(linkID,nextLinkID,length)]
names(filted_net)[3]<-"weight"
g1 <- graph_from_data_frame(filted_net, directed = TRUE)
g2 <- graph_from_data_frame(net, directed = T)
g1 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE)
g2 <- simplify(g2, remove.multiple = F, remove.loops = TRUE)
```

``` r
remove_duplicate_paths <- function(paths, k,type) {
  unique_paths <- list()
  for (path in paths) {
      if(type==1)path_nodes <-V(g1)$name[path]
      if(type==2)path_nodes <-V(g2)$name[path]
      is_duplicate <- any(sapply(unique_paths, function(p) identical(p, path_nodes)))
      if (!is_duplicate) {
        unique_paths <- append(unique_paths, list(path_nodes))
      }
      if (length(unique_paths) >= k) {
        break
      }
    }
    return(unique_paths)
}
calculate_path_length <- function(graph, pathset) {
  if (length(pathset) < 1) return(numeric(0))
  result<-c()
  for (paths in pathset) {
    edges <- E(graph, path = paths)
    result<-c(result,sum(edges$weight, na.rm = TRUE))
  }
result
}
calculate_expected_time<- function(graph, pathset,time="Global") {
  isTimeBin<-T
  if(!time %in% c("EveningNight", "EveningRush" , "Weekday"  ,    "MorningRush" , "Weekendday","Global"  )){isTimeBin<-F
    time <- as.POSIXct( time)
    start_time <- time
    time_Bin<-time_bins_readable(time)
    }else time_Bin<-time
    timelist <-c()
    arrivetime<-c()
  for (paths in pathset) {
      edges <- E(graph,path = paths)
      if(isTimeBin)time <- 0
      else time<-start_time
    for(edge in edges){
      if(!isTimeBin) time_Bin<-time_bins_readable(time)
      leave <- as.integer(as.vector(ends(graph, edge, names = TRUE)[1]))
      arrive <- as.integer(as.vector(ends(graph, edge, names = TRUE)[2]))
      edge_data <- trips[linkID == leave & nextLinkID == arrive & timeBin == time_Bin,]
      if (nrow(edge_data) == 0) {
        edge_data <- trips[linkID == leave & nextLinkID == arrive & timeBin == "Global",]
      }
        new_time <- exp(edge_data$one_way_mean)
      time <- time+new_time
    }
    if(isTimeBin)timelist<-c(timelist,time)
    else {
      arrivetime<-c(arrivetime,time)
      timelist<-c(timelist,as.numeric(difftime(time,start_time,  units = "secs")))
    }
  }
  return(list(expected_time=timelist,arrivetime=arrivetime))
}

findRoute <- function(start, end,time="Global", k = 1) {
  start <- as.character(start)
  end <- as.character(end)
  paths1 <- k_shortest_paths(g1, from = start, to = end, k = k, mode = "out")$vpaths
  paths2 <- k_shortest_paths(g2, from = start, to = end, k = k, mode = "out")$vpaths
  paths1<-remove_duplicate_paths(paths1,k,1)
  paths2<-remove_duplicate_paths(paths2,k,2)
  length1<-calculate_path_length(g1,paths1)
  length2<-calculate_path_length(g2,paths2)
  time1<-calculate_expected_time(g1,paths1,time)
  time2<-calculate_expected_time(g2,paths2,time)
  arrive_time1<-as.POSIXct(time1$arrivetime)
  arrive_time2<-as.POSIXct(time2$arrivetime)
  return(list(oneway = paths1,onway_legnth=length1,time1=time1$expected_time,expect_arrive_time1=arrive_time1, twoway = paths2,twoway_length=length2,time2=time2$expected_time,expect_arrive_time2=arrive_time2))
}
```

``` r
x=findRoute(1,6494,k=3)
x
```

    ## $oneway
    ## $oneway[[1]]
    ## [1] "1"    "6494"
    ## 
    ## $oneway[[2]]
    ## [1] "1"    "6541" "6494"
    ## 
    ## 
    ## $onway_legnth
    ## [1] 332.798 377.309
    ## 
    ## $time1
    ## [1] 12.15174 11.72471
    ## 
    ## $expect_arrive_time1
    ## POSIXct of length 0
    ## 
    ## $twoway
    ## $twoway[[1]]
    ## [1] "1"    "6494"
    ## 
    ## $twoway[[2]]
    ## [1] "1"    "6541" "6494"
    ## 
    ## $twoway[[3]]
    ## [1] "1"     "41672" "7920"  "13831" "36024" "7919"  "35300" "20559" "6494" 
    ## 
    ## 
    ## $twoway_length
    ## [1]  332.798  377.309 2571.266
    ## 
    ## $time2
    ## [1]  12.15174  11.72471 170.67213
    ## 
    ## $expect_arrive_time2
    ## POSIXct of length 0

``` r
findRoute(6494,1,"2025-01-02 19:08:01",k=6)
```

    ## $oneway
    ## $oneway[[1]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578" "43582"
    ## [28] "43580" "25013" "25016" "25017" "13440" "13442" "36024" "13831" "7920" 
    ## [37] "41672" "1"    
    ## 
    ## $oneway[[2]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578" "24812"
    ## [28] "43582" "43580" "25013" "25016" "25017" "13440" "13442" "36024" "13831"
    ## [37] "7920"  "41672" "1"    
    ## 
    ## $oneway[[3]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578" "43582"
    ## [28] "43580" "43581" "25013" "25016" "25017" "13440" "13442" "36024" "13831"
    ## [37] "7920"  "41672" "1"    
    ## 
    ## $oneway[[4]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "24874" "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578"
    ## [28] "43582" "43580" "25013" "25016" "25017" "13440" "13442" "36024" "13831"
    ## [37] "7920"  "41672" "1"    
    ## 
    ## $oneway[[5]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578" "43582"
    ## [28] "43580" "43581" "43579" "25013" "25016" "25017" "13440" "13442" "36024"
    ## [37] "13831" "7920"  "41672" "1"    
    ## 
    ## $oneway[[6]]
    ##  [1] "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "14610" "25011" "24535"
    ## [10] "24536" "24933" "24723" "24678" "24677" "24757" "24891" "24880" "24883"
    ## [19] "20614" "20593" "20592" "20610" "22720" "20556" "43575" "43578" "24812"
    ## [28] "43582" "43580" "43581" "25013" "25016" "25017" "13440" "13442" "36024"
    ## [37] "13831" "7920"  "41672" "1"    
    ## 
    ## 
    ## $onway_legnth
    ## [1] 29789.02 29832.06 29876.53 29895.27 29911.06 29919.57
    ## 
    ## $time1
    ## [1] 1219.167 1211.246 1212.580 1260.548 1212.128 1204.659
    ## 
    ## $expect_arrive_time1
    ## [1] "2025-01-02 19:28:20 PST" "2025-01-02 19:28:12 PST"
    ## [3] "2025-01-02 19:28:13 PST" "2025-01-02 19:29:01 PST"
    ## [5] "2025-01-02 19:28:13 PST" "2025-01-02 19:28:05 PST"
    ## 
    ## $twoway
    ## $twoway[[1]]
    ## [1] "6494" "1"   
    ## 
    ## $twoway[[2]]
    ## [1] "6494" "6541" "1"   
    ## 
    ## $twoway[[3]]
    ## [1] "6494"  "20559" "35300" "7919"  "36024" "13831" "7920"  "41672" "1"    
    ## 
    ## $twoway[[4]]
    ## [1] "6494"  "20559" "35300" "7919"  "36036" "13831" "7920"  "41672" "1"    
    ## 
    ## $twoway[[5]]
    ##  [1] "6494"  "20559" "35300" "7919"  "36024" "13831" "7920"  "41672" "41673"
    ## [10] "1"    
    ## 
    ## $twoway[[6]]
    ##  [1] "6494"  "20559" "35300" "7919"  "36036" "13831" "7920"  "41672" "41673"
    ## [10] "1"    
    ## 
    ## 
    ## $twoway_length
    ## [1]  744.426  788.937 2385.150 2465.433 2874.973 2955.256
    ## 
    ## $time2
    ## [1]  24.0000  25.0000 160.6211 241.0534 176.5696 257.0020
    ## 
    ## $expect_arrive_time2
    ## [1] "2025-01-02 19:08:25 PST" "2025-01-02 19:08:26 PST"
    ## [3] "2025-01-02 19:10:41 PST" "2025-01-02 19:12:02 PST"
    ## [5] "2025-01-02 19:10:57 PST" "2025-01-02 19:12:18 PST"

``` r
findRoute(15335, 361,"MorningRush")
```

    ## Warning in k_shortest_paths(g1, from = start, to = end, k = k, mode = "out"):
    ## At vendor/cigraph/src/paths/dijkstra.c:534 : Couldn't reach some vertices.

    ## $oneway
    ## list()
    ## 
    ## $onway_legnth
    ## numeric(0)
    ## 
    ## $time1
    ## NULL
    ## 
    ## $expect_arrive_time1
    ## POSIXct of length 0
    ## 
    ## $twoway
    ## $twoway[[1]]
    ##  [1] "15335" "15334" "15329" "15327" "26130" "3134"  "3128"  "3131"  "3130" 
    ## [10] "16212" "16196" "16201" "16207" "16197" "44238" "3609"  "27635" "44236"
    ## [19] "3000"  "39475" "19816" "19829" "19780" "39516" "33458" "45536" "28105"
    ## [28] "39469" "19834" "19840" "12704" "2173"  "2174"  "2176"  "2175"  "37135"
    ## [37] "37136" "37120" "37124" "37133" "37119" "37139" "7629"  "45320" "45317"
    ## [46] "45311" "361"  
    ## 
    ## 
    ## $twoway_length
    ## [1] 7202.261
    ## 
    ## $time2
    ## [1] 720.9929
    ## 
    ## $expect_arrive_time2
    ## POSIXct of length 0

``` r
findRoute(c(1,2,3,4,5,6), c(101,202,303,404,505,606))
```

    ## $oneway
    ## $oneway[[1]]
    ##   [1] "1"     "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "6496"  "43584"
    ##  [10] "43583" "43577" "3912"  "20594" "24873" "24894" "24827" "24825" "24826"
    ##  [19] "24830" "24829" "24831" "24823" "24821" "24814" "24817" "24834" "24818"
    ##  [28] "20553" "20550" "20547" "45466" "7922"  "22896" "22693" "22282" "22050"
    ##  [37] "21807" "21810" "21759" "22363" "22799" "43573" "20583" "20574" "20567"
    ##  [46] "20554" "23053" "42850" "22809" "22810" "42851" "21835" "21843" "21787"
    ##  [55] "20540" "29157" "45751" "45753" "45752" "6629"  "45028" "6630"  "35643"
    ##  [64] "34489" "34490" "35671" "35674" "35641" "35653" "35655" "35661" "35648"
    ##  [73] "35645" "35676" "35659" "39632" "6508"  "5709"  "5712"  "20695" "44618"
    ##  [82] "40720" "32026" "32027" "32029" "32030" "30764" "44651" "30762" "45331"
    ##  [91] "46224" "31036" "44167" "44220" "30021" "30026" "30597" "30519" "30524"
    ## [100] "30155" "44054" "38367" "42006" "42008" "40399" "40397" "12253" "30572"
    ## [109] "30571" "12229" "12230" "12231" "30279" "43259" "30424" "30046" "30104"
    ## [118] "2685"  "2669"  "28869" "45928" "42931" "43958" "42928" "42929" "45964"
    ## [127] "42400" "42398" "14688" "27578" "36267" "36264" "4724"  "4720"  "23358"
    ## [136] "46211" "23357" "501"   "92"    "101"  
    ## 
    ## 
    ## $onway_legnth
    ## [1] 39854.16
    ## 
    ## $time1
    ## [1] 2584.609
    ## 
    ## $expect_arrive_time1
    ## POSIXct of length 0
    ## 
    ## $twoway
    ## $twoway[[1]]
    ##   [1] "1"     "6494"  "6497"  "6495"  "6498"  "6499"  "6500"  "6496"  "43584"
    ##  [10] "43583" "43577" "3912"  "20594" "24873" "24894" "24827" "24825" "24826"
    ##  [19] "24830" "24829" "24831" "24823" "24821" "24814" "24817" "24834" "24818"
    ##  [28] "20553" "20550" "20547" "45466" "7922"  "22896" "22693" "22282" "22050"
    ##  [37] "21807" "21810" "21759" "22363" "22799" "43573" "20583" "20574" "20567"
    ##  [46] "20554" "23053" "42850" "22809" "22810" "42851" "21835" "21786" "20541"
    ##  [55] "20543" "44828" "45029" "45030" "20690" "28972" "35643" "34489" "34490"
    ##  [64] "35675" "35642" "22041" "35661" "35648" "35645" "35676" "35659" "39632"
    ##  [73] "6627"  "6628"  "28960" "5708"  "5713"  "39892" "39890" "39893" "31946"
    ##  [82] "31947" "31470" "31444" "44622" "34158" "44618" "40720" "32026" "32027"
    ##  [91] "32029" "32030" "30764" "44630" "45796" "29291" "29290" "30748" "30744"
    ## [100] "44096" "44098" "44099" "30753" "30024" "30011" "3444"  "44078" "35882"
    ## [109] "44065" "6635"  "39955" "3437"  "28869" "45928" "42931" "43958" "42928"
    ## [118] "42929" "45964" "42400" "42398" "14688" "27578" "36267" "36264" "4724" 
    ## [127] "4720"  "23358" "46211" "23357" "501"   "92"    "101"  
    ## 
    ## 
    ## $twoway_length
    ## [1] 38668.09
    ## 
    ## $time2
    ## [1] 2334.508
    ## 
    ## $expect_arrive_time2
    ## POSIXct of length 0

``` r
#findRoute(12, 23)
findRoute(12, 23,"2021/01/01 19:30")
```

    ## $oneway
    ## $oneway[[1]]
    ##  [1] "12"    "37639" "37645" "37640" "3"     "33691" "33702" "33713" "33700"
    ## [10] "33689" "33710" "33698" "24001" "25"    "23"   
    ## 
    ## 
    ## $onway_legnth
    ## [1] 2488.731
    ## 
    ## $time1
    ## [1] 193.104
    ## 
    ## $expect_arrive_time1
    ## [1] "2021-01-01 19:33:13 PST"
    ## 
    ## $twoway
    ## $twoway[[1]]
    ##  [1] "12"    "15"    "9"     "33699" "33694" "33690" "33709" "33715" "44389"
    ## [10] "24001" "25"    "23"   
    ## 
    ## 
    ## $twoway_length
    ## [1] 1469.33
    ## 
    ## $time2
    ## [1] 128.9128
    ## 
    ## $expect_arrive_time2
    ## [1] "2021-01-01 19:32:08 PST"
