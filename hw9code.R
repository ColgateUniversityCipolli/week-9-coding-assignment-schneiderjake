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

(MLEs$par <- exp(MLEs$par))

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

mles.gamma$par


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
mles.log$par


log.likelihood <- -mles.log$value

print(log.likelihood)


#########      C       ##########

exp(weibull.likelihood-gamma.likelihood)

#########      D       ##########

exp(weibull.likelihood-log.likelihood)

#########      E        ##########

exp(gamma.likelihood-log.likelihood)



ggdat.weibull <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dweibull(x=x, shape=MLEs$par[1], scale=MLEs$par[2]))

ggdat.gamma <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dgamma(x=x, shape = mles.gamma$par[1], rate = mles.gamma$par[2]))

ggdat.log <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dlnorm(x=x, meanlog=mles.log$par[1], sdlog=mles.log$par[2]))

distribution <- ggplot() +
  geom_histogram(data=dat.precip.long,
                 aes(x=Precipitation, y=after_stat(density)),
                 breaks=seq(0, 15, 1),
                 color="lightgrey")+
  geom_line(data=ggdat.gamma,
            aes(x=x, y=pdf.mle, color="Gamma"))+
  geom_line(data=ggdat.weibull,
            aes(x=x, y=pdf.mle, color="Weibull"))+
  geom_line(data=ggdat.log,
            aes(x=x, y=pdf.mle, color="Log"))+
  geom_hline(yintercept = 0)+
  theme_bw()+
  xlab("Precipitation (Inches)")+
  ylab("Density")+
  labs(color="")


ggsave("distribution.pdf", plot = distribution, scale =1)





