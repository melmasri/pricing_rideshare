
set.seed(123456)
n = 100
p = 5
v = c(0.25,1,1.5,2)
x = exp(rnorm(n, 0 ,v[sample.int(length(v), n, replace =TRUE)]))
y =  exp(rnorm(n, 0 ,v[sample.int(length(v), n, replace =TRUE)]))
par(mar = c(5,5,1,1)+0.1)
plot(cumsum(x), 1:n, type = 'l', lwd = 3,
     xlab= 'time',
     ylab = 'space',
     cex.lab = 1.5,
     cex.axis = 1.5,
     xaxt='n',
     yaxt = 'n')
lines(cumsum(y),1:n, lwd =3, lty=3)


library(ggplot2)
data = data.frame(obs  = c(cumsum(x),
                      cumsum(y)),
    index = c(1:n, 1:n),
    trip = gl(2, n))

#pdf('filtration-2trips.pdf')
g = ggplot(data  = data, aes(y = obs, x = index))+
    scale_y_continuous( name = 'time', breaks = c(0,floor(max(unlist(data)))),
                       labels = c('0', 't')) +
        scale_x_continuous( name = 'space (n)', breaks = c(0,25,100),
                           labels = c('0',expression(e), '100'))+
  geom_line(aes(linetype = trip), size = 0.5)

t= theme(axis.text.y = element_text(colour = 'black', size = 20), 
          axis.title.y = element_text(size = 20, 
                                      hjust = 0.5, vjust = 0.2)) +
                  theme(axis.text.x = element_text(colour = 'black', size = 20), 
                        axis.title.x = element_text(size = 20, 
                                                    hjust = 0.5, vjust = 0.2),
                        legend.position="none") +
    theme_light() + theme(legend.position="none")

vl = geom_segment(aes(y = obs, x = index, xend =0, yend=obs),
    data = subset(data, index==25)) 
        
tx = geom_label(data = subset(data, index==25),
    aes(y = obs, x = c(-1,-1), label = list('tau[1](e)', 'tau[2](e)')),
    parse=TRUE, inherit.aes = FALSE, size =6)

h = g+ t +vl + tx
h
aspect_ratio = 0.45
#ggsave('filtration-2trips.pdf', h, height = 10*aspect_ratio , width = 10)
                                        #dev.off()    



#### Mixture model
set.seed(19) # some good seeds , 2, 19, 25, 37, 38, 45
nsegments = 100
nmixture = 2
p_fast = 0.8
segment_length = 100
pmixture = c(p_fast,1-p_fast)
sig = c(log(5/3.6), log(10/3.6))
mu = c(log(35/3.6), log(10/3.6))        # as in 35kph, and 10kph
route_x = sample(nmixture, nsegments, replace=TRUE, prob=pmixture)
mu_x = mu[route_x]
sig_x = sig[route_x]
route_y = sample(nmixture, nsegments, replace=TRUE, prob=pmixture)
mu_y = mu[route_y]
sig_y = sig[route_y]
x= segment_length/exp(rnorm(nsegments, mu_x, sig_x))
y= segment_length/exp(rnorm(nsegments, mu_y, sig_y))
par(mar = c(5,5,1,1)+0.1)
plot(cumsum(x), 1:nsegments, type = 'l', lwd = 3,
     xlab= 'time',
     ylab = 'space',
     cex.lab = 1.5,
     cex.axis = 1.5,
     xaxt='n',
     yaxt = 'n')
points(cumsum(x)[route_x==2], (1:nsegments)[route_x==2], col= 'red', lwd=4)
points(cumsum(y)[route_y==2], (1:nsegments)[route_y==2], col= 'red', lwd=4)
lines(cumsum(y),1:nsegments, lwd =3, lty=3)

library(ggplot2)
data = data.frame(obs  = c(cumsum(x),
                      cumsum(y)),
    index = c(1:nsegments, 1:nsegments),
    trip = gl(2, nsegments),
    traffic = c(route_x, route_y))
data

edgeind = 64

par(mar = c(5,5,1,1)+0.1)
                                        #pdf('filtration-2trips.pdf')
g = ggplot(data  = data, aes(y = index, x = obs))+
    scale_x_continuous( name = 'Travel time (minutes)', breaks = c(0,floor(max(unlist(data)))),
                       labels = c('0', round(max(data$obs)/60,2))) +
        scale_y_continuous( name = 'Distance (km)', breaks = c(0,edgeind,100),
                           labels = c('0',expression(e), '10'))+
    geom_line(aes(linetype = trip), size = 0.5) +
    geom_point(data  = data[data$traffic==2,],
               mapping = aes(y = index, x = obs), color ='red',fill=NA,  size=3, stroke =1,shape=21)

t = theme_light() + 
    theme(axis.text.y = element_text(colour = 'black'), 
          axis.title.y = element_text(hjust = 0.5, vjust = 0.2)) +
    theme(axis.text.x = element_text(colour = 'black'), 
          axis.title.x = element_text(hjust = 0.5, vjust = 0.2)) +
    theme(axis.text = element_text(size=15)) +
    theme(legend.position="none") +
    theme(axis.title = element_text(size = 18)) 

vl = geom_segment(aes(x = obs, y = index, yend =0, xend=obs),
    data = subset(data, index==edgeind)) 
        
tx = geom_label(data = subset(data, index==edgeind),
    aes(x = obs, y = c(-1,-1), label = list('tau[1](e)', 'tau[2](e)')),
    parse=TRUE, inherit.aes = FALSE, size =6)


h = g+ t +vl + tx
h

aspect_ratio = 0.50
ggsave('filtration-3trips.pdf', h, height = 8*aspect_ratio , width = 8)
dev.off()    

data






set.seed(100)
nsegments = 100
nmixture = 2
p_fast = 0.8
pmixture = c(p_fast,1-p_fast)
sig = c(log(5/3.6), log(10/3.6))
mu = c(log(35/3.6), log(10/3.6))
segment_length = 100                    # meters
## as in 35kph, and 10kph
sample_ride<-function(){
    route_y = sample(nmixture, nsegments, replace=TRUE, prob=pmixture)
    mu_y = mu[route_y]
    sig_y = sig[route_y]
    y = segment_length/exp(rnorm(nsegments, mu_y, sig_y))
    y  
}

ride_lengths = c(10) * 10
local_cumsum<-function(x){
    km_ = ride_lengths
    sapply(km_, function(y) sum(x[1:y])/y)
}

all_rides = replicate(1000, sample_ride())
dim(all_rides)

rides = apply(all_rides, 2, sum)
rides = apply(all_rides, 2, local_cumsum)

rides = data.table(t(rides))


dim(rides)
head(rides)
hist(rides[,4])




plot_rides_hist<-function(df, legend_label = ride_lengths){
    lengen_km = legend_label * segment_length/1000
    par(mar = c(5,5,1,1)+0.1)
    plot(density(df[[1]]),
         main = '',
         xlab = 'Average travel time per edge (seconds)',
         ylab = 'Empirical density',
         xlim = c(5,30),
                                        #ylim = c(0, 0.05),
         cex.lab=1.5,
         axes = FALSE
         )
    for(i in 2:ncol(df)){
        lines(density(df[[i]]), lty = i)
    }
    legend(x = 'topleft',
           lty = 1:ncol(df),
           legend =paste0(lengen_km, ' km'),
           cex = 1.5,
           box.lwd=0,
           bty = 'n',
           title = 'Route distance'
           
           )
    axis(side = 1, at = c(10,20,30), cex.axis = 1.4)
    axis(side = 2, at = c(0.0,0.3), cex.axis = 1.4)
    #box()
}

#pdf('toyexample_traveltime_density.pdf', width = 6, height = 4.5)
plot_rides_hist(rides)
dev.off()


price<-function(t) {

    1 + 2*t/60 + 0.5 * segment_length*nsegments/1000
    
}

plot(1:length(rides),price(rides), type= 'l')

data.table(rides, check.names=TRUE)
rides[, price(max(obs)) , by = trip]


rides
