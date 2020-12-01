library(abind)
yoda<-read.csv("C:/Users/YODA ISMAEL/Desktop/Publication rapport/Matable_2019.csv",header= FALSE,sep=";",dec=".")
yoda
yoda<- as.vector(t(yoda))
yoda
yod<-yoda/1000000
yod
yoda.1 <- ts(yoda/1000000, start =c(1999,1), end =c(2019,12), frequency = 12)
yoda.1

plot(yoda.1)
# Packages nécessaires
library(lmtest)
library(tseries)
library(forecast)
library(urca)

### Rendons la serie additive
y<-log(yoda.1)
y
y<-c(y)
y
plot(y,type="l")

############# Decomposition de la serie additive ###########

require(graphics)
decomposition<-decompose(y,type="additive")
plot(decomposition)

###################### Modélisation direct ################

### Test de stationnarité de la série
adf.test(y) #P-value=0.99. La série n'est pas stationnaire

####### Choix du nombre de retards pour le test
summary(ur.df(y,type = c("trend"),6))
###### Le nombre de retards a considérer est 6 car y'a 6
# retards significatifs

# Modal without trend and constant
summary(ur.df(y,type = c("none"),6)) 

# Modal with drift 
summary(ur.df(y,type = c("drift"),6)) ### constante du modèle nn significatif


## ACF et PACF de la série non stationnaire
par(mfrow=c(1,2))
a<-seq(0,48,12)
a
plot(acf(y,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=0 D=0")
plot(pacf(y,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=0 D=0")

#graphique
plot(y)


#La sortie ACF présente une décroissance lente vers 0,ce qui 
#traduit un problème de non-stationnarité. On effectue donc une différenciation (I???B) .



## Stationnariation de la série par différenciation
y_dif1=diff(y,lag=1,differences=1) # Série différenciée
par(mfrow=c(1,2))
plot(acf(y_dif1,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=1 D=0")
plot(pacf(y_dif1,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=1 D=0")

par(mfrow=c(1,1))
y_dif1<-c(y_dif1)
plot(y_dif1,type="l",xlab="Months",ylab="Difference",main="d=1 D=0")
## La sortie ACF de la série ainsi différenciée présente encore une décroissance lente
## vers 0 pour les multiples de 12. On effectue cette fois 
## la différenciation (I???B12).

# Différenciation saisonnière
y_dif_1_12=diff(y_dif1,lag=12,differences=1)
par(mfrow=c(1,2))
plot(acf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=1 D=1")
plot(pacf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1),main="d=1 D=1")
## Figure représentatif
par(mfrow=c(1,1))
y_dif_1_12
y_dif_1_12<-c(y_dif_1_12)
plot(y_dif_1_12,type="l",xlab="Months",ylab="Seasonal Difference",main="d=1 D=1")

## Identification des potentielles modèles

model1=Arima(y,order=c(0,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model1)
coeftest(model1) #rmse:0.05419025 mape:0.9880089

model2=Arima(y,order=c(0,1,2),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model2)
coeftest(model2) #rmse:0.05410239 mape:0.9871713

model3=Arima(y,order=c(1,1,0),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model3)
coeftest(model3) #rmse:0.0604973 mape:0.9871713

model4=Arima(y,order=c(1,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model4)
coeftest(model4) #rmse:0.05410654 mape:0.9871787 

model5=Arima(y,order=c(1,1,2),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model5)
coeftest(model5) #rmse:0.05412324 mape:0.9872222


model6=Arima(y,order=c(2,1,0),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model6)
coeftest(model6) #rmse:0.05791952 mape:1.035107

model7=Arima(y,order=c(2,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model7) #rmse: 0.05423335 mape:0.9856721
coeftest(model7)

model8=Arima(y,order=c(2,1,2),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model8) #rmse: 0.05416253 mape:0.9858248
coeftest(model8)

model9=Arima(y,order=c(3,1,0),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model9) #rmse: 0.05631183 mape:1.026864
coeftest(model9)

model10=Arima(y,order=c(3,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model10) # rmse:0.05422417 mape:0.9870186
coeftest(model10)

model11=Arima(y,order=c(3,1,2),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model11) # rmse:0.05427881 mape:0.9888762
coeftest(model11)

#### Modeles hors selections
model12=Arima(y,order=c(1,1,1),list(order=c(1,1,2),period=12),include.mean=FALSE,method="CSS-ML")
summary(model12) # rmse:0.05356029 mape:0.9762887
coeftest(model12)

model13=Arima(y,order=c(1,1,0),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model13) # rmse:0.06306115 mape:1.159043
coeftest(model13)

model14=Arima(y,order=c(2,1,0),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model14) # rmse:0.05791952 mape:1.035107
coeftest(model14)

model15=Arima(y,order=c(3,1,0),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model15) # rmse:0.05663817 mape:1.037743
coeftest(model15)

rmse<-c(0.05419025,0.05410239,0.0604973,0.05410654,0.05412324,0.05791952,
        0.05423335,0.05416253,0.05631183,0.05422417,0.05427881)


length(rmse)
min(rmse) #model2

mape<-c(0.9880089,0.9871713,0.9871713,0.9871787,0.9872222,1.035107,
        0.9856721,0.9858248,1.026864,0.9870186,0.9888762)


length(mape)
min(mape)#model7

Akaikes_criterions<-AIC(model1,model2,model3,model4,model5,model6,
                        model7,model8,model9,model10,model11)
Akaikes_criterions$AIC
min(Akaikes_criterions)#model1



Bic_criterions<-BIC(model1,model2,model3,model4,model5,model6,
                    model7,model8,model9,model10,model11,model12,
                    model13,model14,model15)
Bic_criterions$BIC
min(Bic_criterions)#model1

# Validation du modèle

# Test d'autocorrélation

Box.test(model1$residuals) # P-value=0.88
                           # Les résidus ne sont pas autocorrélés

tsdiag(model1)
# Test de normalité

shapiro.test(model2$residuals) # P-value=0.0001363. Loi non normal


## Prévisions

#### Essaie d'autres modèles
model1=Arima(y,order=c(0,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model1)
coeftest(model1) #rmse:0.05483757 mape:0.9976381
                 # rmse à battre : 0.05410239


#################################################################

par(mfrow=c(1,1))
pred_model1=forecast(model1,h=12,level=95)
pred_model1
pred=exp(pred_model1$mean)
pred
pred_l=exp(pred_model1$lower)
pred_l
pred_u=exp(pred_model1$upper)
pred_u

## Représentation graphique
a=seq(1,252,1)
yod=c(yod)
plot(a,yod,type="l",xlab="Months",ylab="Energy Consumption,Gwh",xlim=c(1,264),ylim=c(1,200))
lines(seq(253,264,len=12),pred,type="l",lty=2,col=2)
legend("topleft",lty=c(4,2),col=c(1,2,3),
       legend = c("Actual","Forecast"))

lines(seq(253,264,len=12),pred_l,type="l",lty=2,col=3)
lines(seq(253,264,len=12),pred_u,type="l",lty=2,col=3)
legend("topleft",lty=c(4,2),col=c(1,2,3),
       legend = c("Actual","Forecast","Upper and Lower"))


#### Evaluation de la qualité prédictive du modèle en prédisant les
#### données de 2019 avec le meme modèle

#On tronque la série de l'année 2019, qu'on cherche ensuite à 
#prévoir à partir de l'historique 1999-2018.

x_tronc=window(yoda.1,end=c(2018,12))
x_tronc=c(x_tronc)
x_tronc
length(x_tronc)
y_tronc=log(x_tronc)
length(y_tronc)
x_a_prevoir=window(yoda.1,start=c(2019,1)) # La série qu'il faut prévoir
x_a_prevoir
x_a_prevoir=c(x_a_prevoir)


# On vérifie que le modèle 2 sur la série tronquée 
# est toujours validé.

model1_tronc=Arima(y_tronc,order=c(0,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
summary(model1_tronc)
coeftest(model1_tronc)




Box.test(model1_tronc$residuals)# Non autocorrelée

shapiro.test(model1_tronc$residuals) # Non normale

# Prévision
pred_model1tronc=forecast(model1_tronc,h=12,level=95)
pred_tronc=exp(pred_model1tronc$mean)
pred_tronc
pred_l_tronc=exp(pred_model1tronc$lower) 
pred_u_tronc=exp(pred_model1tronc$upper)
a=seq(241,252,1)
plot(a,x_a_prevoir,type="l",xlab="Months",ylab="Energy Consumption,Gwh",col=1,ylim=c(100,200))
lines(a,pred_tronc,type="l",col=2,lty=2)
lines(pred_l_tronc,type="l",col=3,lty=2)
lines(pred_u_tronc,type="l",col=3,lty=2)
legend("topleft",legend=c("Real data","Forecasts data"),col=c(1,2),lty=c(1,2),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))


b=seq(1,252,1)
plot(b,yod,type="l",xlab="Months",ylab="Energy Consumption,Gwh",col=1,ylim=c(1,200),xlim=c(1,252))
lines(seq(241,252,len=12),pred_tronc,type="l",col=2,lty=2)
legend("topleft",legend=c("Actual","Forecasts"),col=c(1,2),lty=c(1,2))

lines(seq(241,252,len=12),pred_l_tronc,type="l",col=3,lty=2)
lines(seq(241,252,len=12),pred_u_tronc,type="l",col=3,lty=2)
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))


pred_tronc # Valeurs prédites
x_a_prevoir # Vraies valeurs

#On calcule les RMSE et MAPE

rmse=sqrt(mean((x_a_prevoir-pred_tronc)^2))
rmse

mape=mean(abs(1-pred_tronc/x_a_prevoir))*100
mape
