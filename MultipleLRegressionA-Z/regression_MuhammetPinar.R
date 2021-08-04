
veri<-file_10_21622061_Muhammet_Pinar
names(veri)
bakterisayisi<-veri$y
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
attach(veri)
qqnorm(y)
qqline(y)
install.packages("psych")
library(psych)
describe(veri)
funModeling::freq(x4)
install.packages("fBasics")
library(fBasics)
jarqueberaTest(y)
shapiro.test(y)
boxplot(y)
###Normal dağılım göstermez h0 red     Boxplot grafiğinde aykırı değerlerimizi görüyoruz
boxplot(veri)
boxplot.stats(y)
plot(density(y))
lny<-log(y)
jarqueberaTest(lny)
shapiro.test(lny)
boxplot(lny)   ######ln dönüşümü yaparak normalliği sağladık
lny
veri<-cbind(lny,x1,x2,x3,x4)
headTail(veri)
boxplot(veri)
veri<-as.data.frame(veri)
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
pairs.panels(veri)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
summary(model)
summary(model)   
confint(model,level = .95)
head(x4)
PerformanceAnalytics::chart.Correlation(veri,histogram = TRUE)
headTail(veri)
inf<-ls.diag(model)
inf
influence.measures(model)


k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")




veri<-veri[-c(3,25,39,41,60,63,83,6,17,23,56),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)




k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")









veri<-veri[-c(82),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)



k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")
















veri<-veri[-c(2,21,27,50,63,68,86),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)




k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")





veri<-veri[-c(6,13,45,23,51,61,63,71,73),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)


k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")



veri<-veri[-c(3,7,9,22,45,15),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)


k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")



veri<-veri[-c(36,41,53,58),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)


k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")

veri<-veri[-c(29,32,51,52),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)


k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")


veri<-veri[-c(6,18,34,38),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)



k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")



veri<-veri[-c(8,35,49,52,53),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)

k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")

veri<-veri[-c(21),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)

k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")




veri<-veri[-c(15),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)

k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")



veri<-veri[-c(25,31),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)

k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")



veri<-veri[-c(12,43),]
bakterisayisi<-veri$lny
sicaklik<-veri$x1
ph<-veri$x2
nem<-veri$x3
kap<-veri$x4
kap<-as.factor(kap)
model<-lm(bakterisayisi~sicaklik+ph+nem+kap)
inf<-ls.diag(model)
inf
influence.measures(model)

k=5
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
library(zoo) #index fonksiyonu için paket
cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 4/(length(bakterisayisi)-k-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(bakterisayisi)>50) 4/length(bakterisayisi) else 5/(length(bakterisayisi)-k-1),names(cooksd),""), col="red")
hat<-inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/length(bakterisayisi) , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/length(bakterisayisi),index(hat),""), col="red")
stud<-inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")
std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")




summary(model)

write.table(veri)








##Odevin 2. kismi

predict(model)  # y sapkalar

#degisen varyanslilik
library(lmtest)
bptest(model)
model
#ho:Hatalar sabit varyanslidir.

summary(lm(abs(residuals(model)) ~ fitted(model)))
#degisen varyanslılık yoktur.
par(mfrow=c(2,2)) # “Grafik çizimi ile ilgili kod”
plot(predict(model), abs(inf$stud.res), ylab="Studentized Residuals", xlab="Predicted Value")



dwtest(model)


PerformanceAnalytics::chart.Correlation(veri,histogram = TRUE)


install.packages("olsrr")
library(olsrr)
ols_vif_tol(model)


install.packages("perturb")
library(perturb)
colldiag(model.matrix(model),add.intercept=FALSE)

install.packages("fastDummies")
library(fastDummies)
dummy<-dummy_cols(kap)# Nitel değişken için göstermelik değişkenlerin oluşturulması
x41<-dummy$.data_1
x42<-dummy$.data_2
x43<-dummy$.data_3# Standartlaştırma işlemleri(nitel düzeylerden biri kılavuz değişken olacağı için alınmaz, burada 1. düzey alınmıştır ancak değiştirilebilir) 
ort1<-mean(sicaklik)
kt1<-sum((sicaklik-ort1)^2)
skx1<-(sicaklik-ort1)/(kt1^0.5)
ort2<-mean(ph)
kt2<-sum((ph-ort2)^2)
skx2<-(ph-ort2)/(kt2^0.5)
ort3<-mean(nem)
kt3<-sum((nem-ort3)^2)
skx3<-(nem-ort3)/(kt3^0.5)
ort42<-mean(x42)
kt42<-sum((x42-ort42)^2)
skx42<-(x42-ort42)/(kt42^0.5)
ort43<-mean(x43)
kt43<-sum((x43-ort43)^2)
skx43<-(x43-ort43)/(kt43^0.5)
x<-cbind(skx1,skx2,skx3,skx42,skx43)
sm<-eigen (t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)

V<-sm$vectors
t(V)%*%V
V %*% diag(sm$values) %*% t(V)



sonuc<-lm(bakterisayisi~sicaklik+ph+nem+kap)
summary(sonuc)
confint(sonuc, level = .95) 
anova(sonuc)
confint(sonuc, level=.95)


predict(model,interval = "confidence")


modelon<-data.frame(sicaklik=8,ph=2,nem=3,kap=1)
modelon$kap<-as.factor(modelon$kap)
predict(model,newdata = modelon,interval = "confidence")


confint(model,level = .99)





library(stats)
lm.null <- lm(bakterisayisi ~ 1)
forward <- step(lm.null,bakterisayisi~sicaklik+nem+ph+kap, direction = "forward")
forward
summary(forward) 

backward<-step(model,direction="backward")
summary(backward)

library(MASS)
step.model <- stepAIC(model, direction = "both", trace = FALSE)
summary(step.model) 



library(MASS)

ridge <- lm.ridge(bakterisayisi~sicaklik+ph+nem+kap ,lambda = seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
            ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam == 1]


