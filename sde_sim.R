library(reshape2)
library(ggplot2)
source('utils.R')
library(data.table)
library(traveltimeCLT)


trips = data.table(trips)
trips

avg_seg_length = 170

n <- 1000
delta <- 1
mu <- 16.7/avg_seg_length
sigma <- 41/avg_seg_length
sigma <-20/avg_seg_length


population_sde <- function(n,
                           mu=mu,
                           sigma=sigma,
                           delta=delta) {
    B = rnorm(n , 0, sqrt(delta))
    t = numeric(n)
    for(i in 2:n) {
        t[i] = t[i-1] + mu *avg_seg_length + sigma*avg_seg_length * B[i]
    }
    t
}

stochastic_drift_sde <- function(n,
                           mu=mu,
                           sigma=sigma,
                           delta=delta) {
    B = rnorm(n , 0, sqrt(delta))
    t = numeric(n)
    for(i in 2:n) {
        lambda = exp(0.01*i)/(1+exp(0.01*i))
        mu_avrg = mu * (1-lambda) + (t[i-1]/(i-1)) * lambda
        t[i] = t[i-1] + mu_avrg *delta + sigma * B[i]
    }
    t
}

stochastic_drift_sde <- function(n,
                           mu=mu,
                           sigma=sigma,
                           delta=delta) {
    B = rnorm(n , 0, 1)
    t = numeric(n)
    for(i in 2:n) {
        t[i] = t[i-1] + (1+t[i-1]/(1 + i*delta)) *delta + sigma * B[i]
    }
    t
}


stochastic_sde <- function(n,
                           mu=mu,
                           sigma=sigma,
                           delta=delta) {
    m = floor(avg_seg_length  * n)
    B = rnorm(m , 0, 1)
    t = numeric(m)
    for(i in 2:m) {
        t[i] = max(t[i-1] + (t[i-1])/(i) + sigma * B[i], mu)
    }
    t
    
}

stochastic_mean_reversion <- function(n,
                           mu=mu,
                           sigma=sigma,
                           delta=delta) {
    m = floor(avg_seg_length  * n)
    B = rnorm(m , 0, 1)
    t = numeric(m)
    mu_function<-function(x,y, alpha=0.1) {
        alpha * (x - y)
    }
    
    for(i in 2:m) {
        #t[i] = max(t[i-1] + mu_function(mu, t[i-1]/i, 0.05)   + sigma * sqrt(t[i-1]) * B[i], mu)
        t[i] = t[i-1] + mu_function(mu, t[i-1]/i, 0.05)   + sigma * sqrt(t[i-1]) * B[i]
    }
    t
    
}


plot_lines <- function(df) {
    df_melted <- melt(df)
    head(df_melted)
    ggplot(df_melted, aes(x = Var1, y = value)) +
        geom_line(aes(color = Var2, group = Var2))
}

plot_trips<-function(tr, nsamples = 100) {
    tp = sample(tr[, unique(tripID)], nsamples)
    dt = tr[tripID %in% tp]
    a = dt[, .(N=1:.N, cumsum(duration_secs)), tripID]
     ggplot(a, aes(x = N, y = V2)) +
         geom_line(aes(color = tripID, group = tripID))
}

trips[, var(duration_secs), tripID][ ,mean(V1, na.rm=T)]
trips[, mean(duration_secs), tripID][ ,mean(V1, na.rm=T)]

trips[, .(mean(duration_secs), sd(duration_secs)) ,linkID][!is.na(V2)][]

sigma * avg_seg_length
true_trips = plot_trips(trips)
true_trips
n=100
t= population_sde(n, mu, sigma, delta)
s= stochastic_drift_sde(n, mu, sigma, delta)
s = stochastic_mean_reversion(n, mu, sigma, delta)
plot(1:n,t, type='l')
lines(1:n,s[1:n*avg_seg_length], type='l', col='red')

a = replicate(10, stochastic_sde(n, mu, sigma, delta))
b = replicate(10, population_sde(n, mu, sigma, delta))
c = replicate(10, stochastic_mean_reversion(n, mu, sigma, delta))

g_sde = plot_lines(a)
g_pop= plot_lines(b)
g_mean = plot_lines(c)

multiplot(g_pop,g_sde,g_mean, true_trips, cols=4)


df_melted <- melt(a)
df = data.table(df_melted)
df[, max(value), Var2][, c(sd(V1), mean(V1))]


df_melted <- melt(b)
df = data.table(df_melted)
df[, max(value), Var2][, c(sd(V1), mean(V1))]


sigma * avg_seg_length * sqrt(n)
mu * avg_seg_length * n
