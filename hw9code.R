################################################################################
library(tidyverse)
library(patchwork)
library(nleqslv)

################################################################################
# Precipitation in Madison County
################################################################################
dat.precip <- read_csv(file = "agacis.csv")

#####################################
# Clean Data
#####################################
dat.precip.long <- dat.precip |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))


llweibull <- function(par, data, neg=F){
  # a <- par[1]
  # sigma <- par[2]
  a <- exp(par[1]) # go from (-inf,inf) to (0,inf)
  sigma <- exp(par[2]) # go from (-inf,inf) to (0,inf)
  
  ll <- sum(log(dweibull(x=data, shape=a, scale=sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs <- optim(fn = llweibull,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)
weibull.likelihood <- -MLEs$value





#########    A      ##########
# MLE gamma 
###################
llgamma <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)), na.rm=T)
  
  return(ifelse(neg, -loglik, loglik))
}

mles.gamma <- optim(par = c(1,1),
               fn = llgamma,
               data=dat.precip.long$Precipitation,
               neg=T)

gamma.likelihood <- -mles.gamma$value

print(gamma.likelihood)


##########   B   #########
# MLE 
###################
lllognorm <- function(data, par, neg=F){
  mu <- par[1]
  sigma <- par[2]
  
  loglik <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

mles.log <- optim(par = c(1,1),
               fn = lllognorm,
               data=dat.precip.long$Precipitation,
               neg=T)

log.likelihood <- -mles.log$value

print(log.likelihood)


#########      C       ##########

exp(weibull.likelihood-gamma.likelihood)

#########      D       ##########

exp(weibull.likelihood-log.likelihood)

#########      C       ##########

exp(gamma.likelihood-log.likelihood)












