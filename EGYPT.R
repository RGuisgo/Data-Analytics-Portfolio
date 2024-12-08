##################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#################                                 ###############
###################---------------------------###################

## Data importation
rm(list = ls())
setwd("C:/Users/User/Desktop/Documents/thesis/aims_rw-template-project/data/Egypt")
Egypt=read.csv("C:/Users/User/Desktop/Documents/thesis/aims_rw-template-project/data/Egypt/tas_1901_2016_EGY.csv",header=TRUE,stringsAsFactors = FALSE)


######   TEMPERAURE DATA

library(MASS)
library(gplots)
library(TSA)
require(INLA)
require(ggplot2)
require(mgcv)
require(dplyr)
require(DAAG)
require(reshape2)


ggplot(data=Egypt,mapping = aes(x=Year,y=Temperature))+geom_line(color="red")+labs(title = "Egypt ")


### AR1 Model

Month=c(Egypt$Year)
Temperature=c(Egypt$Temperature)
f=Temperature ~1+  f(Month, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

ar1 <- inla(f,data = data.frame(Temperature,Month), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))

summary(ar1)

ar1$cpo$cpo
ar1$cpo$failure

sum(ar1$cpo$cpo)
mean((ar1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))



### Model Hyperpaarameter Rho (autocorrelation strength)
ar1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=ar1$summary.random$Month$ID,y=ar1$summary.random$Month$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=ar1$summary.random$Month$`0.025quant`,ymax=ar1$summary.random$Month$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Egypt AR 1 Model ")+labs(y= "Effect", x = "Years",color=Month)

ggplot(mapping=aes(x=Month,y=ar1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=ar1$summary.fitted.values$`0.025quant`,ymax=ar1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt AR1 Model Temperature ")+labs(y= "Temperature", x = "Years")


### Estimated temperature

p=10 ### Number of years projection
i1=c(Egypt$Year,rep((Egypt$Year[length(Egypt$Year)]+1):(Egypt$Year[length(Egypt$Year)]+p),12))
Y1=c(Egypt$Temperature,rep(NA, (p*12)))
f1=Y1 ~1+  f(i1, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE)))
ar11 <- inla(f1,data = data.frame(Y=Y1,i=i1), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(ar11)
ggplot(mapping=aes(x=i1,y=ar11$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=ar11$summary.fitted.values$`0.025quant`,ymax=ar11$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt")+labs(y= "Temperature", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
  annotate('text', x = 2023, y = 24.5, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 24.5, label = 'fitted', color = 'blue') 

### Fixed Effects
ar11$summary.fixed




############## Random Walk of Order 1

ir=c(Egypt$Year)
Yr=c(Egypt$Temperature)
fr=Yr ~1+  f(ir, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE))) 

rw1 <- inla(fr,data = data.frame(Y=Yr,i=ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rw1)

rw1$cpo$cpo
rw1$cpo$failure

sum(rw1$cpo$cpo)
mean((rw1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))


### Model Hyperpaarameter Rho (autocorrelation strength)
rw1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=rw1$summary.random$ir$ID,y=rw1$summary.random$i$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=rw1$summary.random$i$`0.025quant`,ymax=rw1$summary.random$i$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 1 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=ir,y=rw1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rw1$summary.fitted.values$`0.025quant`,ymax=rw1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 1 Temperature ")+labs(y= "Temperature", x = "Years")

### Estimated temperature

p=10 ### Number of years projection
ir2=c(Egypt$Year,rep((Egypt$Year[length(Egypt$Year)]+1):(Egypt$Year[length(Egypt$Year)]+p),12))
Yr2=c(Egypt$Temperature,rep(NA, (p*12)))
fr2=Yr2 ~1+  f(ir2, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE)))
rw12 <- inla(fr2,data = data.frame(Y=Yr2,i=ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rw12)
ggplot(mapping=aes(x=ir2,y=rw12$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rw12$summary.fitted.values$`0.025quant`,ymax=rw12$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt ")+labs(y= "Temperature", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
  annotate('text', x = 2023, y = 24, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 24, label = 'fitted', color = 'blue') 

### Fixed Effects
rw12$summary.fixed


############# Random walk of order two

ir=c(Egypt$Year)
Yr=c(Egypt$Temperature)
fr=Yr ~1+  f(ir, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

rw2 <- inla(fr,data = data.frame(Y=Yr,i=ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rw2)

### Model Hyperpaarameter Rho (autocorrelation strength)
rw2$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=rw2$summary.random$ir$ID,y=rw2$summary.random$ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=rw2$summary.random$ir$`0.025quant`,ymax=rw2$summary.random$ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 2 ")+labs(y= "Effect", x = "Years")


ggplot(mapping=aes(x=ir,y=rw2$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rw2$summary.fitted.values$`0.025quant`,ymax=rw2$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 2 Temperature ")+labs(y= "Temperature", x = "Years")


### Estimated temperature

p=10 ### Number of years projection
ir2=c(Egypt$Year,rep((Egypt$Year[length(Egypt$Year)]+1):(Egypt$Year[length(Egypt$Year)]+p),12))
Yr2=c(Egypt$Temperature,rep(NA, (p*12)))
fr2=Yr2 ~1+  f(ir2, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE)))
rw22 <- inla(fr2,data = data.frame(Y=Yr2,i=ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rw22)
ggplot(mapping=aes(x=ir2,y=rw22$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rw22$summary.fitted.values$`0.025quant`,ymax=rw22$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 2 Forecasted Temperature ")+labs(y= "Temperature", x = "Years")


### Fixed Effects
rw22$summary.fixed




############# Rainfall


## Data importation
rm(list = ls())
setwd("G:/Documents/thesis/aims_rw-template-project/data/Egypt")
Egypt2=read.csv("G:/Documents/thesis/aims_rw-template-project/data/Egypt/pr_1901_2016_EGY.csv",header=TRUE,stringsAsFactors = FALSE)


ggplot(data=Egypt2,mapping = aes(x=Year,y=Rainfall))+geom_line(color="red")+labs(title="Egypt")


### AR1 Model

ir3=c(Egypt2$Year)
Yr3=c(Egypt2$Rainfall)
f=Yr3 ~1+  f(ir3, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

rain_ar1 <- inla(f,data = data.frame(Y=Yr3,i=ir3), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))

summary(rain_ar1)

rain_ar1$cpo$cpo
rain_ar1$cpo$failure

sum(rain_ar1$cpo$cpo)
mean((rain_ar1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))


### Model Hyperpaarameter Rho (autocorrelation strength)
rain_ar1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=rain_ar1$summary.random$ir3$ID,y=rain_ar1$summary.random$ir3$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=rain_ar1$summary.random$ir3$`0.025quant`,ymax=rain_ar1$summary.random$ir3$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Egypt Rainfall AR 1 Model ")+labs(y= "Effect", x = "Years")


ggplot(mapping=aes(x=ir3,y=rain_ar1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rain_ar1$summary.fitted.values$`0.025quant`,ymax=rain_ar1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Rainfall AR1 Model")+labs(y= "Rainfall", x = "Years")


### Estimated Rainfall

p=10 ### Number of years projection
i1=c(Egypt2$Year,rep((Egypt2$Year[length(Egypt2$Year)]+1):(Egypt2$Year[length(Egypt2$Year)]+p),12))
Y1=c(Egypt2$Rainfall,rep(NA, (p*12)))
f1=Y1 ~1+  f(i1, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE)))
rain_ar11 <- inla(f1,data = data.frame(Y=Y1,i=i1), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rain_ar11)
ggplot(mapping=aes(x=i1,y=rain_ar11$summary.fitted.values$mean))+geom_line(color="red")+annotate('text', x = 2023, y = 4.7, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 4.7, label = 'fitted', color = 'blue')+ 
  geom_ribbon(aes(ymin=rain_ar11$summary.fitted.values$`0.025quant`,ymax=rain_ar11$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt ")+labs(y= "Rainfall", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")


### Fixed Effects
rain_ar11$summary.fixed



############## Random Walk of Order 1

ir=c(Egypt2$Year)
Yr=c(Egypt2$Rainfall)
fr=Yr ~1+  f(ir, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE))) 

rain_rw1 <- inla(fr,data = data.frame(Y=Yr,i=ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rain_rw1)


rain_rw1$cpo$cpo
rain_rw1$cpo$failure

sum(rain_rw1$cpo$cpo)
-mean(log(rain_rw1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))


### Model Hyperpaarameter Rho (autocorrelation strength)
rain_rw1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=rain_rw1$summary.random$ir$ID,y=rain_rw1$summary.random$ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=rain_rw1$summary.random$ir$`0.025quant`,ymax=rain_rw1$summary.random$ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = " Egypt Rainfall Random Walk of Order 1 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=ir,y=rain_rw1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rain_rw1$summary.fitted.values$`0.025quant`,ymax=rain_rw1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Rainfall Random Walk of Order 1 Rainfall ")+labs(y= "Rainfall", x = "Years")

### Estimated Rainfall

p=10 ### Number of years projection
ir2=c(Egypt2$Year,rep((Egypt2$Year[length(Egypt2$Year)]+1):(Egypt2$Year[length(Egypt2$Year)]+p),12))
Yr2=c(Egypt2$Rainfall,rep(NA, (p*12)))
fr2=Yr2 ~1+  f(ir2, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE)))
rain_rw12 <- inla(fr2,data = data.frame(Y=Yr2,i=ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rain_rw12)
ggplot(mapping=aes(x=ir2,y=rain_rw12$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rain_rw12$summary.fitted.values$`0.025quant`,ymax=rain_rw12$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt  ")+labs(y= "Rainfall", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+annotate('text', x = 2023, y = 4.5, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 4.5, label = 'fitted', color = 'blue') 


### Fixed Effects
rain_rw12$summary.fixed


############# Random walk of order two

ir=c(Egypt2$Year)
Yr=c(Egypt2$Rainfall)
fr=Yr ~1+  f(ir, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

rain_rw2 <- inla(fr,data = data.frame(Y=Yr,i=ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rain_rw2)

### Model Hyperpaarameter Rho (autocorrelation strength)
rain_rw2$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=rain_rw2$summary.random$ir$ID,y=rain_rw2$summary.random$ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=rain_rw2$summary.random$ir$`0.025quant`,ymax=rain_rw2$summary.random$ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Egypt Rainfall Random Walk of Order 2 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=ir,y=rain_rw2$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rain_rw2$summary.fitted.values$`0.025quant`,ymax=rain_rw2$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 2 Rainfall ")+labs(y= "Rainfall", x = "Years")


### Estimated temperature

p=10 ### Number of years projection
ir2=c(Egypt2$Year,rep((Egypt2$Year[length(Egypt2$Year)]+1):(Egypt2$Year[length(Egypt2$Year)]+p),12))
Yr2=c(Egypt2$Rainfall,rep(NA, (p*12)))
fr2=Yr2 ~1+  f(ir2, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE)))
rain_rw22 <- inla(fr2,data = data.frame(Y=Yr2,i=ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(rain_rw22)
ggplot(mapping=aes(x=ir2,y=rain_rw22$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=rain_rw22$summary.fitted.values$`0.025quant`,ymax=rain_rw22$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Egypt Random Walk of Order 2 Forecasted Rainfall ")+labs(y= "Rainfall", x = "Years")


### Fixed Effects
rain_rw22$summary.fixed

