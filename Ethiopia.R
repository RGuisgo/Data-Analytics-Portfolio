##################---------------------------###################
#################                                 ###############
#                       Time Series Analysis                    #
#################                                 ###############
###################---------------------------###################

## Data importation
rm(list = ls())
setwd("G:/Documents/thesis/aims_rw-template-project/data/Ethiopia")
Ethiopia=read.csv("G:/Documents/thesis/aims_rw-template-project/data/Ethiopia/tas_1901_2016_ETH.csv",header=TRUE,stringsAsFactors = FALSE)


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


ggplot(data=Ethiopia,mapping = aes(x=Year,y=Temperature))+geom_line(color="red")+labs(title="Ethipoia")


### AR1 Model





Ethiopia_Month=c(Ethiopia$Year)
Ethiopia_Temperature=c(Ethiopia$Temperature)
Ethiopia_f=Ethiopia_Temperature ~1+  f(Ethiopia_Month, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

Ethiopia_ar1 <- inla(Ethiopia_f,data = data.frame(Ethiopia_Temperature,Ethiopia_Month), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))

summary(Ethiopia_ar1)

Ethiopia_ar1$cpo$cpo
Ethiopia_ar1$cpo$failure

sum(Ethiopia_ar1$cpo$cpo)
-mean(log(Ethiopia_ar1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))





### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia_ar1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia_ar1$summary.random$Ethiopia_Month$ID,y=Ethiopia_ar1$summary.random$Ethiopia_Month$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia_ar1$summary.random$Ethiopia_Month$`0.025quant`,ymax=Ethiopia_ar1$summary.random$Ethiopia_Month$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Ethiopia AR 1 Model Temperature ")+labs(y= "Effect", x = "Years")


ggplot(mapping=aes(x=Ethiopia_Month,y=Ethiopia_ar1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_ar1$summary.fitted.values$`0.025quant`,ymax=Ethiopia_ar1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia AR1 Model Temperature ")+labs(y= "Temperature", x = "Years")


### Estimated temperature

p=10 ### Number of years projection
Ethiopia_i1=c(Ethiopia$Year,rep((Ethiopia$Year[length(Ethiopia$Year)]+1):(Ethiopia$Year[length(Ethiopia$Year)]+p),12))
Ethiopia_Y1=c(Ethiopia$Temperature,rep(NA, (p*12)))
Ethiopia_f1=Ethiopia_Y1 ~1+  f(Ethiopia_i1, model = "ar1", hyper = list(prec = list(initial = 5, fixed = TRUE)))
Ethiopia_ar11 <- inla(Ethiopia_f1,data = data.frame(Y=Ethiopia_Y1,i=Ethiopia_i1), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia_ar11)
ggplot(mapping=aes(x=Ethiopia_i1,y=Ethiopia_ar11$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_ar11$summary.fitted.values$`0.025quant`,ymax=Ethiopia_ar11$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia  ")+labs(y= "Temperature", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
  annotate('text', x = 2023, y = 23.5, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 23.5, label = 'fitted', color = 'blue') 

### Fixed Effects
Ethiopia_ar11$summary.fixed




############## Random Walk of Order 1

Ethiopia_ir=c(Ethiopia$Year)
Ethiopia_Yr=c(Ethiopia$Temperature)
Ethiopia_fr=Ethiopia_Yr ~1+  f(Ethiopia_ir, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE))) 

Ethiopia_rw1 <- inla(Ethiopia_fr,data = data.frame(Y=Ethiopia_Yr,i=Ethiopia_ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia_rw1)

Ethiopia_rw1$cpo$cpo
Ethiopia_rw1$cpo$failure

sum(Ethiopia_rw1$cpo$cpo)
-mean(log(Ethiopia_rw1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))

### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia_rw1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia_rw1$summary.random$Ethiopia_ir$ID,y=Ethiopia_rw1$summary.random$Ethiopia_ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia_rw1$summary.random$Ethiopia_ir$`0.025quant`,ymax=Ethiopia_rw1$summary.random$Ethiopia_ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Ethiopia Temperature Random Walk of Order 1 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=Ethiopia_ir,y=Ethiopia_rw1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_rw1$summary.fitted.values$`0.025quant`,ymax=Ethiopia_rw1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 1 Temperature ")+labs(y= "Temperature", x = "Years")

### Estimated temperature

p=10 ### Number of years projection
Ethiopia_ir2=c(Ethiopia$Year,rep((Ethiopia$Year[length(Ethiopia$Year)]+1):(Ethiopia$Year[length(Ethiopia$Year)]+p),12))
Ethiopia_Yr2=c(Ethiopia$Temperature,rep(NA, (p*12)))
Ethiopia_fr2=Ethiopia_Yr2 ~1+  f(Ethiopia_ir2, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE)))
Ethiopia_rw12 <- inla(Ethiopia_fr2,data = data.frame(Y=Ethiopia_Yr2,i=Ethiopia_ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia_rw12)
ggplot(mapping=aes(x=Ethiopia_ir2,y=Ethiopia_rw12$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_rw12$summary.fitted.values$`0.025quant`,ymax=Ethiopia_rw12$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia ")+labs(y= "Temperature", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
annotate('text', x = 2023, y = 24.5, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 24.5, label = 'fitted', color = 'blue') 

### Fixed Effects
Ethiopia_rw12$summary.fixed


############# Random walk of order two

Ethiopia_ir=c(Ethiopia$Year)
Ethiopia_Yr=c(Ethiopia$Temperature)
Ethiopia_fr=Ethiopia_Yr ~1+  f(Ethiopia_ir, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE))) 

Ethiopia_rw2 <- inla(Ethiopia_fr,data = data.frame(Y=Ethiopia_Yr,i=Ethiopia_ir), family =  "gaussian",
            control.family = list(
              list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
            control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia_rw2)

### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia_rw2$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia_rw2$summary.random$Ethiopia_ir$ID,y=Ethiopia_rw2$summary.random$Ethiopia_ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia_rw2$summary.random$Ethiopia_ir$`0.025quant`,ymax=Ethiopia_rw2$summary.random$Ethiopia_ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 2 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=Ethiopia_ir,y=Ethiopia_rw2$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_rw2$summary.fitted.values$`0.025quant`,ymax=Ethiopia_rw2$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 2 Temperature ")+labs(y= "Temperature", x = "Years")

### Estimated temperature

p=10 ### Number of years projection
Ethiopia_ir2=c(Ethiopia$Year,rep((Ethiopia$Year[length(Ethiopia$Year)]+1):(Ethiopia$Year[length(Ethiopia$Year)]+p),12))
Ethiopia_Yr2=c(Ethiopia$Temperature,rep(NA, (p*12)))
Ethiopia_fr2=Ethiopia_Yr2 ~1+  f(Ethiopia_ir2, model = "rw2", hyper = list(prec = list(initial = 5, fixed = TRUE)))
Ethiopia_rw22 <- inla(Ethiopia_fr2,data = data.frame(Y=Ethiopia_Yr2,i=Ethiopia_ir2), family =  "gaussian",
             control.family = list(
               list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia_rw22)
ggplot(mapping=aes(x=Ethiopia_ir2,y=Ethiopia_rw22$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia_rw22$summary.fitted.values$`0.025quant`,ymax=Ethiopia_rw22$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 2 Forecasted Temperature ")+labs(y= "Temperature", x = "Years")


### Fixed Effects
Ethiopia_rw22$summary.fixed




############# Rainfall


## Data importation
rm(list = ls())
setwd("G:/Documents/thesis/aims_rw-template-project/data/Ethiopia")
Ethiopia2=read.csv("G:/Documents/thesis/aims_rw-template-project/data/Ethiopia/pr_1901_2016_ETH.csv",header=TRUE,stringsAsFactors = FALSE)


ggplot(data=Ethiopia2,mapping = aes(x=Year,y=Rainfall))+geom_line(color="red")+labs(title="Ethiopia")


### AR1 Model
Ethiopia2_ir3=c(Ethiopia2$Year)
Ethiopia2_Yr3=c(Ethiopia2$Rainfall)
Ethiopia2_f=Ethiopia2_Yr3 ~1+  f(Ethiopia2_ir3, model = "ar1", hyper = list(prec = list(initial =3.5, fixed = TRUE))) 

Ethiopia2_rain_ar1 <- inla(Ethiopia2_f,data = data.frame(Y=Ethiopia2_Yr3,i=Ethiopia2_ir3), family =  "gaussian",
                 control.family = list(
                   list(hyper = list(prec = list(initial = 0.9, fixed = TRUE)))),
                 control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))

summary(Ethiopia2_rain_ar1)


Ethiopia2_rain_ar1$cpo$cpo
Ethiopia2_rain_ar1$cpo$failure

sum(Ethiopia2_rain_ar1$cpo$cpo)
mean((Ethiopia2_rain_ar1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))

i=Ethiopia2$Year
Y=Ethiopia2$Rainfall
####### AR1 Model
M1="ar1" ### autoregressive model of order 1
M2="ar2" ### autoregressive model of order 2
### Note that there might be need to adjust the "initial value" of the hyperparameter when you change model type
f=Y ~1+  f(i, model = M1, hyper = list(prec = list(initial = 3, fixed = TRUE))) 

Ethiopia2_rain_ar1 <- inla(f,data = data.frame(Y=Y,i=i), family =  "gaussian",
                   control.family = list(
                     list(hyper = list(prec = list(initial = 3, fixed = TRUE)))),
                   control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE)
)
### Determine the  current Model Used
print(Ethiopia2_rain_ar1$model.random)


summary(Ethiopia2_rain_ar1)
### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia2_rain_ar1$summary.hyperpar$mean





# temp.ar1.pred <- inla(Temperature ~ f(Year, model = "ar1"), data = df,
#                       family = "gaussian", control.predictor = list(compute = TRUE ))

### Model Hyperpaarameter Rho (autocorrelation strength)
##Ethiopia2_rain_ar1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia2_rain_ar1$summary.random$i$ID,y=Ethiopia2_rain_ar1$summary.random$i$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia2_rain_ar1$summary.random$i$`0.025quant`,ymax=Ethiopia2_rain_ar1$summary.random$i$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Ethiopia Rainfall AR 1 Model ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=i,y=Ethiopia2_rain_ar1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_ar1$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_ar1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Rainfall AR1 Model")+labs(y= "Rainfall", x = "Years")


### Estimated temperature

p=10 ### Number of years projection
Ethiopia2_i1=c(Ethiopia2$Year,rep((Ethiopia2$Year[length(Ethiopia2$Year)]+1):(Ethiopia2$Year[length(Ethiopia2$Year)]+p),12))
Ethiopia2_Y1=c(Ethiopia2$Rainfall,rep(NA, (p*12)))
Ethiopia2_f1=Ethiopia2_Y1 ~1+  f(Ethiopia2_i1, model = "ar1", hyper = list(prec = list(initial = 3, fixed = TRUE)))
Ethiopia2_rain_ar11 <- inla(Ethiopia2_f1,data = data.frame(Y=Ethiopia2_Y1,i=Ethiopia2_i1), family =  "gaussian",
                  control.family = list(
                    list(hyper = list(prec = list(initial = 3, fixed = TRUE)))),
                  control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia2_rain_ar11)
ggplot(mapping=aes(x=Ethiopia2_i1,y=Ethiopia2_rain_ar11$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_ar11$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_ar11$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia  ")+labs(y= "Rainfall", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
annotate('text', x = 2023, y = 85, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 85, label = 'fitted', color = 'blue') 

### Fixed Effects
Ethiopia2_rain_ar11$summary.fixed



############## Random Walk of Order 1

Ethiopia2_ir=c(Ethiopia2$Year)
Ethiopia2_Yr=c(Ethiopia2$Rainfall)
Ethiopia2_fr=Ethiopia2_Yr ~1+  f(Ethiopia2_ir, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE))) 

Ethiopia2_rain_rw1 <- inla(Ethiopia2_fr,data = data.frame(Y=Ethiopia2_Yr,i=Ethiopia2_ir), family =  "gaussian",
                 control.family = list(
                   list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
                 control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia2_rain_rw1)

Ethiopia2_rain_rw1$cpo$cpo
Ethiopia2_rain_rw1$cpo$failure

sum(Ethiopia2_rain_rw1$cpo$cpo)
mean((Ethiopia2_rain_rw1$cpo$cpo))



new=inla.cpo(Rwanda2_rain_rw1)
summary(new)
-mean(log(Rwanda2_rain_rw1$cpo$cpo))

#### refitting model

new$cpo$cpo
new$cpo$failure

sum(new$cpo$cpo)
mean((new$cpo$cpo))


### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia2_rain_rw1$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia2_rain_rw1$summary.random$Ethiopia2_ir$ID,y=Ethiopia2_rain_rw1$summary.random$Ethiopia2_ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw1$summary.random$Ethiopia2_ir$`0.025quant`,ymax=Ethiopia2_rain_rw1$summary.random$Ethiopia2_ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = " Ethiopia Rainfall Random Walk of Order 1 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=Ethiopia2_ir,y=Ethiopia2_rain_rw1$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw1$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_rw1$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Rainfall Random Walk of Order 1 ")+labs(y= "Rainfall", x = "Years")

### Estimated Rainfall

p=10 ### Number of years projection
Ethiopia2_ir2=c(Ethiopia2$Year,rep((Ethiopia2$Year[length(Ethiopia2$Year)]+1):(Ethiopia2$Year[length(Ethiopia2$Year)]+p),12))
Ethiopia2_Yr2=c(Ethiopia2$Rainfall,rep(NA, (p*12)))
Ethiopia2_fr2=Ethiopia2_Yr2 ~1+  f(Ethiopia2_ir2, model = "rw1", hyper = list(prec = list(initial = 3.5, fixed = TRUE)))
Ethiopia2_rain_rw12 <- inla(Ethiopia2_fr2,data = data.frame(Y=Ethiopia2_Yr2,i=Ethiopia2_ir2), family =  "gaussian",
                  control.family = list(
                    list(hyper = list(prec = list(initial = 1.5, fixed = TRUE)))),
                  control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia2_rain_rw12)
ggplot(mapping=aes(x=Ethiopia2_ir2,y=Ethiopia2_rain_rw12$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw12$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_rw12$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia   ")+labs(y= "Rainfall", x = "Years")+geom_vline(xintercept =  2017, linetype = "dashed", color = "blue")+
  annotate('text', x = 2023, y = 80, label = 'predicted', color = 'red')+ annotate('text', x = 1960, y = 80, label = 'fitted', color = 'blue') 

### Fixed Effects
Ethiopia2_rain_rw12$summary.fixed


############# Random walk of order two

Ethiopia2_ir=c(Ethiopia2$Year)
Ethiopia2_Yr=c(Ethiopia2$Rainfall)
Ethiopia2_fr=Ethiopia2_Yr ~1+  f(Ethiopia2_ir, model = "rw2", hyper = list(prec = list(initial = 3.5, fixed = TRUE))) 

Ethiopia2_rain_rw2 <- inla(Ethiopia2_fr,data = data.frame(Y=Ethiopia2_Yr,i=Ethiopia2_ir), family =  "gaussian",
                 control.family = list(
                   list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
                 control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia2_rain_rw2)

### Model Hyperpaarameter Rho (autocorrelation strength)
Ethiopia2_rain_rw2$summary.hyperpar$mean


### Effects of Year
ggplot(mapping=aes(x=Ethiopia2_rain_rw2$summary.random$Ethiopia2_ir$ID,y=Ethiopia2_rain_rw2$summary.random$Ethiopia2_ir$mean))+geom_line(color="red",)+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw2$summary.random$Ethiopia2_ir$`0.025quant`,ymax=Ethiopia2_rain_rw2$summary.random$Ethiopia2_ir$`0.975quant`),alpha=0.2)+
  theme_bw()+labs(title = "Ethiopia Rainfall Random Walk of Order 2 ")+labs(y= "Effect", x = "Years")

ggplot(mapping=aes(x=Ethiopia2_ir,y=Ethiopia2_rain_rw2$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw2$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_rw2$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 2 Rainfall ")+labs(y= "Rainfall", x = "Years")


### Estimated Rainfall

p=10 ### Number of years projection
Ethiopia2_ir2=c(Ethiopia2$Year,rep((Ethiopia2$Year[length(Ethiopia2$Year)]+1):(Ethiopia2$Year[length(Ethiopia2$Year)]+p),12))
Ethiopia2_Yr2=c(Ethiopia2$Rainfall,rep(NA, (p*12)))
Ethiopia2_fr2=Ethiopia2_Yr2 ~1+  f(Ethiopia2_ir2, model = "rw2", hyper = list(prec = list(initial = 3.5, fixed = TRUE)))
Ethiopia2_rain_rw22 <- inla(Ethiopia2_fr2,data = data.frame(Y=Ethiopia2_Yr2,i=Ethiopia2_ir2), family =  "gaussian",
                  control.family = list(
                    list(hyper = list(prec = list(initial = 2.5, fixed = TRUE)))),
                  control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE))


summary(Ethiopia2_rain_rw22)
ggplot(mapping=aes(x=Ethiopia2_ir2,y=Ethiopia2_rain_rw22$summary.fitted.values$mean))+geom_line(color="red")+
  geom_ribbon(aes(ymin=Ethiopia2_rain_rw22$summary.fitted.values$`0.025quant`,ymax=Ethiopia2_rain_rw22$summary.fitted.values$`0.975quant`),alpha=0.3)+
  theme_bw()+labs(title = "Ethiopia Random Walk of Order 2 Forecasted Rainfall ")+labs(y= "Rainfall", x = "Years")


### Fixed Effects
Ethiopia2_rain_rw22$summary.fixed

