rm(list = ls())
library(fpp)
library(forecast)
library(haven)
library(readxl)
library(dygraphs)


## veriler aylık seriye sahiptir
data <- read_excel('C:/Users/Seher/Desktop/Zaman Serileri/Ödev/data.xlsx')
View(data)

## Zaman serisi olarak tanımlanır
dataTs<- ts(data$Seri)

## Zaman grafiğinin tespit edilmesi 
ts.plot(dataTs,gpars=list(xlab="Zaman", ylab="x18"))

summary(data$Seri)
class(data$Seri)
data$seri = round(data$Seri)

## Seri trende sahiptir (ilk 4 gecikme sınır dışında)
Acf(dataTs,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(dataTs,lag.max = 42, ylim=c(-1,1), lwd=3)

## Bir fark ile trendden arındırdık
## periyot = 12
## seri her 12 ayda bir en yüksek değerini alıyor
diff1 <- diff(dataTs,differences = 1)
diffSeason <- diff(diff1, 12)

Acf(diffSeason, lag=42)

## farklı yöntem
Acf(diff(diff(dataTs),12),lag.max = 42)
Pacf(diff(diff(dataTs),12),lag.max = 42)

## Additive Model
dataTs_Trend<-tslm(dataTs~trend)
periyot<- dataTs-dataTs_Trend[["fitted.values"]]

Acf(periyot,lag.max = 42,  ylim=c(-1,1), lwd=3)

data_ts<- ma(dataTs, order = 12, centre = TRUE)

Mevsim<- dataTs-data_ts
donemort<-t(matrix(data=Mevsim, nrow = 9, ncol=8))
colMeans(donemort, na.rm = T)
sum(colMeans(donemort, na.rm = T))
mean(colMeans(donemort, na.rm = T))


## Endeks
endeks<- colMeans(donemort, na.rm = T)-mean(colMeans(donemort, na.rm = T))
indeks<-  matrix(data = endeks, nrow = 72)
trenthata<- dataTs-indeks
class(trenthata)
colnames(trenthata) <- make.names(1:ncol(trenthata))

trent<-tslm(trenthata~trend)


tahmin<- indeks+trent[["fitted.values"]]

hata<- dataTs-indeks-trent[["fitted.values"]]


## Model
plot(window(dataTs), 
     xlab="Zaman", ylab="")
lines(window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Tahmin))))
#hatalar akgurultu mu?
Acf(hata,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)



## Hatalar akgürültü değil model anlamlı değil       
Box.test(hata, lag=42, type="Ljung-Box")      


## multiplicative model

#mevsimsel bileseni bulunmasi (Zt/MHO) (hata terimi de mevcut)
Mevsim1 <- dataTs/data_ts

#her bir periyot icin ortalama degerlerinin hesabi
donemort1<-t(matrix(data=Mevsim1, nrow = 9, ncol=8))

colMeans(donemort1, na.rm = T)

#toplam
sum(colMeans(donemort1, na.rm = T))

#ortalamalarin ortalamasi
mean(colMeans(donemort1, na.rm = T))

#mevsimsel endeks degerlerinin bulunusu
endeks1<- colMeans(donemort1, na.rm = T)/mean(colMeans(donemort1, na.rm = T))
#mean(endeks1)=1?

#endeks degerlerini seri boyunca yazdirma islemi
indeks1<-  matrix(data = endeks1, nrow = 72)


#trent serisi (hata da mevcut) (orijinal seri/mevsimsel endeks serisi)
trenthata1<- dataTs/indeks1
colnames(trenthata1) <- make.names(1:ncol(trenthata1))

#hatadan arindirma islemi
trent1<- tslm(trenthata1~trend)
tahmin1<- indeks1*trent1[["fitted.values"]] #tahmin=endeks*orijinal serinin trent bileseni

#hata serisi
hata1<- dataTs-tahmin1


plot(window(dataTs), 
     xlab="Zaman", ylab="")
lines(window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Tahmin))))

#hatalar akgurultu mu?
Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

Box.test(hata1, lag=42, type="Ljung-Box")      


### Regresyon ###
## Toplamsal Model
dataSeri <- data$Seri
t<-1: 1: 72   #t terimini olusturalim

sin1<-sin(2*3.1416*t/9)
cos1<-cos(2*3.1416*t/9)


dataset<-as.data.frame(cbind(dataSeri, t, sin1, cos1))

names(dataset)<- c("y", "t", "sin1", "cos1")
attach(dataset)

regresyon.model<-lm(y~t+sin1+cos1)
summary(regresyon.model)

## Çarpımsal model
s1<-t*sin(2*3.1416*t/9)
c1<-t*cos(2*3.1416*t/9)


dataset2<-as.data.frame(cbind(dataSeri, t, s1, c1))

names(dataset2)<- c("y", "t", "s1", "c1")
attach(dataset2)

regresyon.model3<-lm(y~t+s1+c1)
summary(regresyon.model3)
dwtest(y~t+s1+c1)



## Winter's Yöntemi
dataTs2 <- ts(data$Seri, frequency = 12)

## Toplamsal Yöntem ##
WintersAdditive <- ets(dataTs2, model = "AAA")

summary(WintersAdditive)

tahmin1<- WintersAdditive[["fitted"]]

plot(window(dataTs2), 
     xlab="Zaman", ylab="")
lines(window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Tahmin))))

hata1<- WintersAdditive[["residuals"]]

Box.test (hata1, lag = 42, type = "Ljung")

Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

checkresiduals(WintersAdditive, lag = 42)

ongoru <- forecast(WintersAdditive,h=12)

ongoru[["mean"]]

### Çarpımsal Yöntem ###
WintersMultip<- ets(abs(dataTs2), model = "MAM")

summary(WintersMultip)

tahmin2<- WintersMultip[["fitted"]]

plot(window(dataTs2), 
     xlab="Zaman", ylab="")
lines(window(tahmin2) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Tahmin))))

hata2<- WintersMultip[["residuals"]]

Box.test (hata2, lag = 42, type = "Ljung")

Acf(hata2,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata2,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

checkresiduals(WintersMultip, lag = 42)

ongoru <- forecast(WintersMultip,h=12)

ongoru[["mean"]]


## Box Jeinkes ## 
data <- read_excel('C:/Users/Seher/Desktop/Zaman Serileri/Ödev/data.xlsx')
dataTs <- ts(data[,2], start = 2013,  frequency =12)


ts.plot(dataTs,main="İthalat", xlab="Zaman", ylab="")

Acf(dataTs,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(dataTs,lag.max = 42, ylim=c(-1,1), lwd=3)

## Bir fark ile trendden arındırdık
## periyot = 12
## seri her 12 ayda bir en yüksek değerini alıyor
diff1 <- diff(dataTs,differences = 1)
diffSeason <- diff(diff1, 12)

Acf(diffSeason, lag=42)

## farklı yöntem
Acf(diff(diff(dataTs),12),lag.max = 42)
Pacf(diff(diff(dataTs),12),lag.max = 42)

## AR modeli 

dataArima1 <- Arima(dataTs, order = c(2,1,0), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(dataArima1)
summary(dataArima1)

dataArima2 <- Arima(dataTs, order = c(2,1,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(dataArima2)
summary(dataArima2)

dataArima3 <- Arima(dataTs, order = c(1,1,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(dataArima3)
summary(dataArima3)

dataArima6 <- Arima(dataTs, order = c(1,1,0), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(dataArima6)
summary(dataArima6)


## q deneyelim

dataArima4 <- Arima(dataTs, order = c(0,1,2), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(dataArima4)
summary(dataArima4)

dataArima5 <- Arima(dataTs, order = c(1,1,2), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(dataArima5)
summary(dataArima5)

dataArima7 <- Arima(dataTs, order = c(1,1,1), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(dataArima7)
summary(dataArima7)

dataArima10 <- Arima(dataTs, order = c(1,1,2), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(dataArima10)
summary(dataArima10)

dnm<- auto.arima(dataTs, trace=TRUE)
summary(dnm)
coeftest(dnm)

tahmin<- dataArima2[["fitted"]]
hata<- dataArima2[["residuals"]]

plot( window(dataTs), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(X7_b_uygulama)),
                   expression(paste(Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))


ongoru<- forecast(auto.arima(dataTs) , h=12)


Box.test(hata, lag = 42, type = "Ljung")

checkresiduals(hata)

Acf(hata,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

ongoru

