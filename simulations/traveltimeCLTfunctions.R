library(mvtnorm)
library(traveltimeCLT)
library(data.table)
library(doParallel)
library(igraph)
library(tidygraph)
library(ggraph)
library(stringr)

sd_one_input_is_0<-function(x){
  x=na.omit(x)
  if(length(x)==1)return(0)
  else return(sd(x))
}

get_mode <- function(x) {
  x=na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dependent_uniform<-function(n, rho=0.31) {
  if(n==1)return(runif(1))
  S <-diag(n)
  for (i in 1:n) {
    for (j in 2:n) {
      S[i, j] <- rho^(abs(i-j))
    }
  }
  S = S +t(S)
  diag(S)<-1 
  St = 2 * sin(S * pi/6) # must be positive definite
  U = c(pnorm(rmvnorm(1, sigma = St)))
  U
}

first_order_uniform<-function(n, rho=0.31) {
  S <-diag(n)
  if(n>1){
    if(n>2)for (i in 2:(n-1)) {
      for (j in (i-1):(i+1)) {
        S[i, j] <- 2*rho^(abs(1))
      }
    }
    S[n,n-1]=2*rho
    S[1, 2]=rho
    S[2, 1]=rho
    diag(S)<-1
    eigen_values <- eigen(S, symmetric = TRUE)$values
    if(!all(eigen_values >= 0))
      S <- as.matrix(Matrix::nearPD(S, cor = TRUE)$mat)
    U = c(pnorm(rmvnorm(1, sigma = S)))
  }else U = runif(1)
  U
}

second_order_uniform<-function(n, rho=0.31) {
  S <-diag(n)
  if(n>2){
    for (i in 1:n) {
      if(i-2>0)S[i, (i-2)] <- 2*rho
      if(i+2<=n)S[i, (i+2)] <- 2*rho
    }
    S[1, 3]=rho
    S[3, 1]=rho
    diag(S)<-1
    eigen_values <- eigen(S, symmetric = TRUE)$values
    if(!all(eigen_values >= 0))
      S <- as.matrix(Matrix::nearPD(S, cor = TRUE)$mat)
    U = c(pnorm(rmvnorm(1, sigma = S)))
  }else U = runif(n)
  U
}

get_timeBin_x_edges <- function(trips=NULL,tripID=NULL,linkId=NULL,length=NULL,
                            timeBin=NULL,time=NULL,duration=NULL,log_duration=NULL){
  trip<-tripID
  frameAvailable <- !is.null(trips)
  if(frameAvailable){
    trips <- data.table(trips)
    tripParamsAvailable <- !is.null(trips$trip) & !is.null(trips$linkId)& !is.null(trips$length)
    timeParamsAvailable1 <- !is.null(trips$timeBin) & !is.null(trips$duration) &!is.null(trips$log_duration)
    timeParamsAvailable2 <- !is.null(trips$time)
  }
    else{
  tripParamsAvailable <- !is.null(trip) & !is.null(linkId)& !is.null(length)
  timeParamsAvailable1 <- !is.null(timeBin) & (!is.null(duration) |!is.null(log_duration))
  timeParamsAvailable2 <- !is.null(time)
    }
  if(!tripParamsAvailable)stop("Either 'trip' ,'length', or 'linkId' is not provided.")
  if(!timeParamsAvailable1&!timeParamsAvailable2)stop("'time' or 'timBin' and 'duration' is not provided.")
  if(!frameAvailable){
    if(!timeParamsAvailable2){
      if(!is.null(duration)){
        if (length(trip) != length(linkId) || length(trip) != length(duration)||
            length(trip) != length(timeBin) || length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','duration', 'length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log(duration),length=length)
      }else{
        if (length(trip) != length(linkId) || length(trip) != length(log_duration)||
            length(trip) != length(timeBin)|| length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','log_duration','length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log_duration,length=length)
      }
    }else{
      if (length(trip) != length(linkId) || length(trip) != length(time)|| length(trip) != length(length))
        stop("Parameter vectors for 'tripID', 'linkId', 'length', and 'time' are not equal in length!")
      trips<-data.table(trip=trip,linkId=linkId,time=time,length=length)
    }
  }
  if(!timeParamsAvailable1){
    trips$time <- as.POSIXct( trips$time)
    trips$timeBin<-time_bins_readable(trips$time)
    trips[, duration := as.numeric(difftime(shift(time, type = "lead"), time, units = "secs")), by = trip]
    trips[, log_duration := log(duration)]
    trips <- na.omit(trips)
  }
  timeBin_x_edges <- trips[,.(mean = mean(log_duration, na.rm = TRUE),
                             sd = sd_one_input_is_0(log_duration),
                             frequency = .N,
                             length = get_mode(length)),
                           by = .(linkId, timeBin)]
  timeBin_x_edges[, ID := 1:.N]
  timeBin_x_edges
}

get_timeBin_x_connections <- function(trips=NULL,tripID=NULL,linkId=NULL,length=NULL,
                                timeBin=NULL,time=NULL,duration=NULL,log_duration=NULL){
  trip<-tripID
  frameAvailable <- !is.null(trips)
  if(frameAvailable){
    trips <- data.table(trips)
    tripParamsAvailable <- !is.null(trips$trip) & !is.null(trips$linkId)& !is.null(trips$length)
    timeParamsAvailable1 <- !is.null(trips$timeBin) & !is.null(trips$duration) &!is.null(trips$log_duration)
    timeParamsAvailable2 <- !is.null(trips$time)
  }
  else{
    tripParamsAvailable <- !is.null(trip) & !is.null(linkId)& !is.null(length)
    timeParamsAvailable1 <- !is.null(timeBin) & (!is.null(duration) |!is.null(log_duration))
    timeParamsAvailable2 <- !is.null(time)
  }
  if(!tripParamsAvailable)stop("Either 'trip', 'length' or 'linkId' is not provided.")
  if(!frameAvailable){
    if(!timeParamsAvailable2){
      if(!is.null(duration)){
        if (length(trip) != length(linkId) || length(trip) != length(duration)||
            length(trip) != length(timeBin)||length(trip) != length(length)) 
          stop("Parameter vectors for 'tripID', 'length', 'linkId','duration', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log(duration),length=length)
      }else{
        if (length(trip) != length(linkId) || length(trip) != length(log_duration)||
            length(trip) != length(timeBin)||length(trip)!= length(length))
          stop("Parameter vectors for 'tripID','length', 'linkId','log_duration', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log_duration,length=length)
      }
    }else{
      if (length(trip) != length(linkId) || length(trip) != length(time) || length(trip) != length((length)))
        stop("Parameter vectors for 'tripID', 'length','linkId', and 'time' are not equal in length!")
      trips<-data.table(trip=trip,linkId=linkId,time=time,length=length)
    }
  }
  if(!timeParamsAvailable1){
    trips$time <- as.POSIXct( trips$time)
    trips$timeBin<-time_bins_readable(trips$time)
    trips[, duration := as.numeric(difftime(shift(time, type = "lead"), time, units = "secs")), by = trip]
    trips[, log_duration := log(duration)]
  }
  trips[, `:=`(nextLinkId, shift(linkId, type = "lead")), by = tripID]
  trips<-na.omit(trips)
  link_net_list<- trips[,c("log_duration","linkId","nextLinkId","timeBin","length")]
  names(link_net_list)<-c("log_duration","linkID","nextLinkID","timeBin","length")


  timeBin_stats <- link_net_list[, 
                                 .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                   one_way_sd = sd_one_input_is_0(log_duration),
                                   one_way_frequency = .N),
                                 by = .(linkID, nextLinkID, timeBin)]
  length_stats <- link_net_list[, .(length = get_mode(length)), by = .(linkID, nextLinkID)]
  timeBin_stats <- merge(timeBin_stats, length_stats, by = c("linkID", "nextLinkID"))
  global_stats <- link_net_list[,
                                .(  one_way_mean = mean(log_duration, na.rm = TRUE),
                                    one_way_sd = sd_one_input_is_0(log_duration),
                                    one_way_frequency = .N,
                                    length = get_mode(length)),, 
                                by = .(linkID, nextLinkID)]
  global_stats[, timeBin := "Global"]
  stats1 <- rbind(timeBin_stats, global_stats)
  existing_pairs <- unique(link_net_list[, .(linkID, nextLinkID)])
  reverse_pairs <- existing_pairs[, .(linkID = nextLinkID, nextLinkID = linkID)]
  missing_reverse <- reverse_pairs[!existing_pairs, on = .(linkID, nextLinkID)]
  setnames(missing_reverse, c("B", "A"))
  
  fictional_data <- link_net_list[missing_reverse,
                                  on = .(linkID = B),
                                  allow.cartesian = TRUE][nextLinkID != A]
  timeBin_fictional <- fictional_data[,
                                      .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                        one_way_sd = sd_one_input_is_0(log_duration),
                                        one_way_frequency = .N),
                                      by = .(linkID, A, timeBin)]
  timeBin_fictional[, nextLinkID := A][, A := NULL]
  global_fictional <- fictional_data[,
                                     .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                       one_way_sd = sd_one_input_is_0(log_duration),
                                       one_way_frequency = .N),
                                     by = .(linkID, A)]
  global_fictional[, timeBin := "Global"][, nextLinkID := A][, A := NULL]
  fictional_stats <- rbind(timeBin_fictional, global_fictional, fill = TRUE)
  length_fictional <- fictional_data[, .(length = get_mode(length)), by = .(linkID, A)]
  length_fictional[, nextLinkID := A][, A := NULL]
  fictional_stats <- merge(fictional_stats, length_fictional, by = c("linkID", "nextLinkID"))
  
  fictional_stats[, fictional := TRUE]
  stats1[, fictional := FALSE]
  
  stats1 <- rbind(stats1, fictional_stats, fill = TRUE)
  setcolorder(stats1, c("linkID", "nextLinkID", "timeBin", "one_way_mean", 
                        "one_way_sd", "one_way_frequency", "length", "fictional"))
  stats1
}

plot_metric_graph <- function(sampledtrips){
  sampled_connection=get_timeBin_x_connections(sampledtrips)
  edges <- unique(sampled_connection, by = c("linkID", "nextLinkID"))
  edges <- edges[fictional==F,]
  g <- graph_from_data_frame(edges, directed = TRUE)
  tidy_g <- as_tbl_graph(g)
  edge_alpha <- 1
  filtered_trips <- sampledtrips
  start_nodes <- as.character(filtered_trips[, .( linkId[1]), by = trip]$V1)
  end_nodes <- as.character(filtered_trips[, .( linkId[length(linkId)]), by = trip]$V1)
  junction_nodes <- V(g)[degree(g, mode = "out") > 1 | degree(g, mode = "in") > 1]$name
  node_label <- ifelse(V(g)$name %in% c(junction_nodes,end_nodes,start_nodes), V(g)$name, NA)
  paths <- list()
  for (i in 1:length(start_nodes)) {
    paths <- c(paths, all_simple_paths(g, from = start_nodes[i], to = end_nodes[i]))
  }
  shorten_segment <- function(segment) {
    l<-length(segment)
    if (l > 6) {
      new_length <- 5
      segment <- c(segment[1:new_length],segment[l])
    }
    return(segment)
  }
  segmented_paths <- lapply(paths, function(path) {
    junctions_in_path <- intersect(names(path), junction_nodes)
    if (length(junctions_in_path) == 0) {
      return(shorten_segment(path))
    }
    segments <- list()
    start_index <- 1
    for (junction in junctions_in_path) {
      end_index <- which(names(path) == junction)
      segment <- path[start_index:(end_index-1)]
      segments <- c(segments, list(shorten_segment(segment)))
      start_index <- end_index
    }
    last_segment <- path[start_index:length(path)]
    segments <- c(segments, list(shorten_segment(last_segment)))
    return(unlist(segments))
  })
  new_edges <- do.call(rbind, lapply(segmented_paths, function(path) {
    path_names <- names(path)
    from <- path_names[-length(path_names)]
    to <- path_names[-1] 
    data.frame(from = from, to = to)
  }))
  
  g <- graph_from_data_frame(new_edges, directed = TRUE)
  tidy_g <- as_tbl_graph(g)
  node_label <- ifelse(V(g)$name %in% c(junction_nodes,end_nodes,start_nodes), V(g)$name, NA)
  p1<-ggraph(tidy_g, layout = "stress") +
    geom_edge_link(
      aes(alpha = edge_alpha), 
      arrow = arrow(length = unit(1.5, "mm")), 
      edge_color = "black" 
    ) +
    geom_node_point(
      aes(
        color = ifelse(name %in% start_nodes, "Start", 
                       ifelse(name %in% end_nodes, "End", 
                              ifelse(name %in% junction_nodes, "Junction", "Normal"))),
        size = ifelse(name %in% start_nodes, "Start", 
                      ifelse(name %in% end_nodes, "End", 
                             ifelse(name %in% junction_nodes, "Junction", "Normal")))
      )
    )+
    geom_node_text(aes(label = node_label), size = 3, color = "darkblue", repel = TRUE, na.rm = TRUE) +
    scale_size_manual(values = c("Start" = 2, "End" = 2, "Junction" = 2, "Normal" = 0.01))+
    scale_color_manual(values = c("Start" = "green", "End" = "red", "Junction" = "orange", "Normal" = "lightblue")
    ) +
    theme_void() +  
    theme(legend.position = "none") 
  p1
}

get_metric_graph <- function(timeBin_x_connections){
  trips <- timeBin_x_connections
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
  return(list(one_way_map=g1,two_way_map=g2))
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

path_time<- function( pathset,timeBin_x_connections,time="Global",simulator="independent",rho=0.31) {
  simulator<-tolower(simulator)
  isTimeBin<-T
  if(!time %in% c("EveningNight", "EveningRush" , "Weekday"  ,    "MorningRush" , "Weekendday","Global"  )){isTimeBin<-F
  time <- as.POSIXct( time)
  start_time <- time
  time_Bin<-time_bins_readable(time)
  }else time_Bin<-time
  arrivetime<-c()
  result <- vector("list", length(pathset)) 
  if(length(pathset)==0)stop("the path set is empty!")
  for (path_idx  in 1:length(pathset)) {
    path <- pathset[[path_idx]]
    path<- attr(path,"names")
    l = length(path)-1
    if(isTimeBin){time <- 0
    simulate_time<-0}
    else{time<-start_time
    simulate_time<-start_time}
    if(simulator=="independent")U<-runif(l)
    else if(simulator=="dependent")U<-dependent_uniform(l,rho)
    else if(simulator=="first order")U<-first_order_uniform(l,rho)
    else if(simulator=="second order")U<-second_order_uniform(l,rho)
    else stop("The simuulator is not supported!")
    fictional <-c()
    frequency <-c()
    label_list <-c()
    timebinlist<-c()
    for(i in 1:l){
      if(!isTimeBin){ time_Bin<-time_bins_readable(time)
      simulate_time_Bin<-time_bins_readable(simulate_time)}
      leave <- as.integer(path[i])
      arrive <- as.integer(path[i+1])
      edge_data <- timeBin_x_connections[linkID == leave & nextLinkID == arrive & timeBin == time_Bin,]
      timebinlist[i]<-time_Bin
      if (nrow(edge_data) == 0) {
        edge_data <- timeBin_x_connections[linkID == leave & nextLinkID == arrive & timeBin == "Global",]
        timebinlist[i]<-"Global"
      }
      if (nrow(edge_data) == 0)stop(paste("Cannot find statistics from ",leave," to ",arrive," at ",time_Bin))
      duration <- exp(edge_data$one_way_mean)
      simulate_duration <- exp(edge_data$one_way_mean+edge_data$one_way_sd*qnorm(U[i]))
      time <- time+duration
      simulate_time <- simulate_time+simulate_duration
      label_list <-c(label_list,paste(leave,"->",arrive))
      fictional<-c(fictional,edge_data$fictional)
      frequency<-c(frequency,edge_data$one_way_frequency)
      names(fictional)<-label_list
      names(frequency)<-label_list
      names(timebinlist)<-label_list
    }
    if(isTimeBin){
      result[[path_idx]]<-list(expected_time=time,expected_arrivetime=NA,
                       simulate_time=simulate_time,sumulate_arrivetime=NA,
                       timebin=timebinlist,fictional=fictional,frequency=frequency)
    }else {
      duration<-as.numeric(difftime(time,start_time,  units = "secs"))
      simulate_duration<-as.numeric(difftime(simulate_time,start_time,  units = "secs"))
      result[[path_idx]]<-list(expected_time=duration,expected_arrivetime=time,
                       simulate_time=simulate_duration,sumulate_arrivetime=simulate_time,
                       timebin=timebinlist,fictional=fictional,frequency=frequency)
    }
  }
  return(result)
}

findRoute <- function(graphs,start, end,k = 1) {
  g1<-graphs$one_way_map
  g2<-graphs$two_way_map
  start <- as.character(start)
  end <- as.character(end)
  paths1 <- k_shortest_paths(g1, from = start, to = end, k = k, mode = "out")$vpaths
  paths2 <- k_shortest_paths(g2, from = start, to = end, k = k, mode = "out")$vpaths
  length1<-calculate_path_length(g1,paths1)
  length2<-calculate_path_length(g2,paths2)
  return(list(oneway = paths1,onway_legnth=length1, twoway = paths2,twoway_length=length2))
}

path_length<- function( pathset,timeBin_x_connections) {
  result<-list()
  for (path_idx  in 1:length(pathset)) {
    path <- pathset[[path_idx]]
    path<- attr(path,"names")
    l = length(path)-1
    len=0
    lenlist=c()
    label_list <-c()
    for(i in 1:l){
      leave <- as.integer(path[i])
      arrive <- as.integer(path[i+1])
      edge_data <- timeBin_x_connections[linkID == leave & nextLinkID == arrive]
      if (nrow(edge_data) == 0)stop(paste("Cannot find statistics from ",leave," to ",arrive))
      len <- len+get_mode(edge_data$length)
      label_list <-c(label_list,paste(leave,"->",arrive))
      lenlist<-c(lenlist,get_mode(edge_data$length))
    }
    names(lenlist)<-label_list
    result[[path_idx]]<-list(total_length=len,length_list=lenlist)

  }
  return(result)
}

plot_CDF_compare <- function(realtime,simulatetime,simulate_data_name="simulated_data",
                             x_lab="Total Travel Time (seconds)",title= "CDF of Travel Time",x_max=4000){
  travel_time <- data.frame(sampled_time=realtime,simulated_time=simulatetime)
  color_values <- c("sampled data" = "red", simulate_data_name = "black")
  names(color_values)[2] <- simulate_data_name
  plot1<-ggplot(travel_time) +
    stat_ecdf(aes(x = sampled_time,color="sampled data")) +
    stat_ecdf(aes(x = simulated_time,color=simulate_data_name)) +
    labs(title = title, x = x_lab , y = "Cumulative Probability")+
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1))+
    scale_color_manual(name = "Legend",
                       values = color_values)+
    theme(legend.position = c(0.95, 0.5),
          legend.justification = c(1, 1),
          legend.text.align = 0,
          legend.background = element_rect(color = "black", fill = "white"))
  plot1
}