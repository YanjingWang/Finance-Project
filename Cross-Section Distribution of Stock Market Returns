install.packages("quantmod")
install.packages("plyr")
install.packages("xts")
install.packages("zoo")
install.packages("TTR")

library(TTR)
library(zoo)
library(xts)
library(quantmod)
library(plyr)

data<-as.vector(read.csv("/Users/suyue/Desktop/fe800/data/financial/2-21-FinancialData.csv",sep = ","))
#==========================

data=data+1

log.data<-log(data[,2:ncol(data)],base=exp(1))

#==========================

table<-matrix(0,ncol = 7,nrow = ncol(log.data))

for(i in 1:ncol(log.data)){
  table[i,]<-as.numeric(quantile(log.data[,i], 
                                 probs = c(0.05, 0.25,
                                           0.45,0.5,0.55, 
                                           0.75, 0.95),na.rm=TRUE))
  }


disp.col<-table[,7]-table[,1]
skew.col<-(table[,7]-table[,4])-(table[,4]-table[,1])
left.col<-table[,3]-table[,2]-(table[,2]-table[,1])
right.col<-table[,7]-table[,6]-(table[,6]-table[,5])


mean.col<-c()
data[is.na(data)]<-0
for(i in 1:ncol(data)){
  mean.col[i]<-mean(data[,i])
}
outcome<-cbind(mean.col,disp.col,skew.col,left.col,right.col)
mov.ave<-SMA(skew.col)

#==============GDP============
install.packages("Quandl")
library(Quandl)
Quandl.api_key("-gkmQ_iEFWGzMBHcbn99")


mydata = Quandl("FRED/GDP")#quartly GDP

gdp = as.vector(Quandl("FRED/GDP", type="raw"))
gdp$Date<-as.Date(gdp$Date)
gdp<-subset(gdp,Date>="1973-01-01",select = c(Date,Value))
gdp<- gdp[seq(dim(gdp)[1],1),]

GDPreturns = gdp[1:nrow(gdp),]
for (j in 2:nrow(gdp)) {
  GDPreturns[j,2] = (as.numeric(gdp[j,2])-
                       as.numeric(gdp[j-1,2]))/as.numeric(gdp[j-1,2])*100
}
GDPreturns[1,2]=0
plot(GDPreturns,type="l",col="red",xlab="year",ylab="GDP Growth")



plot(skew.col,type="l",col="blue")
#============2.2=============

#moving average
cor.table<-cbind(GDPreturns$Value[10:nrow(GDPreturns)],mov.ave[10:length(mov.ave)])

cor(cor.table, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

cor.file.75<-as.data.frame(read.csv("/Users/suyue/Desktop/fe800/data/correlation.csv"))
cor.file.75$Date<-as.Date(cor.file.75$Date,format='%m/%d/%Y')
cor.file.75$mov.ave<-as.numeric(cor.file.75$mov.ave)
cor.file.86<-subset(cor.file.75,Date >"1985-10-01",select = c(Value,USRECQP,mov.ave))
cor.file.84.08<-subset(cor.file.75,Date>="1984-10-01"&Date <"2008-10-01",select = c(Value,USRECQP,mov.ave))

cor.file.08<-subset(cor.file.75,Date >"2007-10-01",select = c(Value,USRECQP,mov.ave))

list<-c(round(cor(cor.file.75$Value,cor.file.75$mov.ave),2),
        round(cor(cor.file.75$USRECQP,cor.file.75$mov.ave),2),
        round(cor(cor.file.86$Value,cor.file.86$mov.ave),2),
        round(cor(cor.file.86$USRECQP,cor.file.86$mov.ave),2),
        round(cor(cor.file.08$Value,cor.file.08$mov.ave),2),
        round(cor(cor.file.08$USRECQP,cor.file.08$mov.ave),2))
matrix(list,ncol = 3,nrow = 2,dimnames = list(c("GDP Growth","Expansion Indicator"),c("1973–2017","1986–2017","2008-2017")))

cor(GDPreturns$Value,skew.col)

#===========Moody's baa========
spread_aaa <- Quandl("FRBP/SPR_BAA_AAA_MN")
install.packages("jrvFinance")
library(jrvFinance)
Aaa<-as.vector(read.csv("/Users/suyue/Desktop/fe800/data/financial/2-21-FinancialData.csv",sep = ","))
Baa<-as.vector(read.csv("/Users/suyue/Desktop/fe800/data/financial/2-21-FinancialData.csv",sep = ","))

#===============logit model============

#standardized regressors
install.packages("standardize")
library(standardize)

scaled.reg<-scale(outcome)
nber<-as.vector(read.csv("/Users/suyue/Desktop/fe800/data/USRECQP.csv",sep = ","))
nber$DATE<-as.Date(nber$DATE)

nber<-subset(nber,DATE>="1973-01-01",select = c(DATE,USRECQP))
for(i in 1:nrow(nber)){
  if(nber$USRECQP[i]==0){
    nber$USRECQP[i]=1
  }
  else{
    nber$USRECQP[i]=0
  }  
}
#correlation
#write csv
#a<-cbind(GDPreturns,nber,mov.ave)
#write.csv(a,file="correlation.csv")






#Pseudo.R2========
Pseudo.R2=function(object){
  stopifnot(object$family$family == "binomial")
  object0 = update(object, ~ 1)
  wt <- object$prior.weights # length(wt)
  y = object$y # weighted
  ones = round(y*wt)
  zeros = wt-ones
  fv <- object$fitted.values   # length(fv)
  if (is.null(object$na.action)) fv0 <- object0$fitted.values else
    fv0 <- object0$fitted.values[-object$na.action] # object may have missing values
  resp <- cbind(ones, zeros)
  Y <- apply(resp, 1, function(x) {c(rep(1, x[1]), rep(0, x[2]))} )
  if (is.list(Y)) Y <- unlist(Y) else Y <- c(Y)
  # length(Y); sum(Y)
  fv.exp <- c(apply(cbind(fv, wt), 1, function(x) rep(x[1], x[2])))
  if (is.list(fv.exp)) fv.exp <- unlist(fv.exp) else fv.exp <- c(fv.exp)
  # length(fv.exp)
  fv0.exp <- c(apply(cbind(fv0, wt), 1, function(x) rep(x[1], x[2])))
  if (is.list(fv0.exp)) fv0.exp <- unlist(fv0.exp) else fv0.exp <- c(fv0.exp)
  (ll = sum(log(dbinom(x=Y,size=1,prob=fv.exp))))
  (ll0 = sum(log(dbinom(x=Y,size=1,prob=fv0.exp))))
  
  n <- length(Y)
  G2 <- -2 * (ll0 - ll)
  McFadden.R2 <- 1 - ll/ll0
  CoxSnell.R2 <- 1 - exp((2 * (ll0 - ll))/n) # Cox & Snell / Maximum likelihood pseudo r-squared
  r2ML.max <- 1 - exp(ll0 * 2/n)
  Nagelkerke.R2 <- CoxSnell.R2/r2ML.max  # Nagelkerke / Cragg & Uhler's pseudo r-squared
  
  out <- c(llh = ll, llhNull = ll0, G2 = G2, McFadden = McFadden.R2,
           r2ML = CoxSnell.R2, r2CU = Nagelkerke.R2)
  out
}
#=====coefficient==========

coeff<-as.data.frame(cbind(outcome,nber$USRECQP))

coeff.mean<-glm(V6 ~ mean.col, data = coeff,family = binomial())
coeff.disp<-glm(V6 ~ disp.col, data = coeff,family = binomial())
coeff.skew<-glm(V6 ~ skew.col, data = coeff,family = binomial())
coeff.left<-glm(V6 ~ left.col, data = coeff,family = binomial())
coeff.right<-glm(V6 ~ right.col, data = coeff,family = binomial())
summary(coeff.mean)
summary(coeff.disp)
summary(coeff.skew)
coefficients(coeff.skew)
summary(coeff.left)
summary(coeff.right)
Pseudo.R2(coeff.mean)[4]
Pseudo.R2(coeff.disp)[4]
Pseudo.R2(coeff.skew)
Pseudo.R2(coeff.left)[4]
Pseudo.R2(coeff.right)[4]




#nber$USRECQP <- factor(nber$USRECQP)
coeff.table<-glm(V6 ~ mean.col+disp.col+skew.col+left.col+right.col, data = coeff,family = binomial())
summary(coeff.table)
coeff.table$family
Pseudo.R2(coeff.table)[4]


#==========================In-sample Predictive Regressions==================

#===========GDP growth indicator======

install.packages("tseries")
library(tseries)
gdp$Date<-as.Date(gdp$Date)
GDPreturns.ts<-GDPreturns
GDPreturns.ts<-xts(GDPreturns.ts[,-1], order.by=as.Date(GDPreturns.ts[,1], "%m/%d/%Y"))

adf.test(GDPreturns.ts,alternative = "stationary")
nrow(GDPreturns.ts)
new.GDPreturns<-GDPreturns[52:180,]
yth=new.GDPreturns[9:nrow(new.GDPreturns),]
y4=new.GDPreturns[4:(nrow(new.GDPreturns)-5),]
y3=new.GDPreturns[3:(nrow(new.GDPreturns)-6),]
y2=new.GDPreturns[2:(nrow(new.GDPreturns)-7),]
y1=new.GDPreturns[1:(nrow(new.GDPreturns)-8),]


new.scaled.reg<-scaled.reg[52:180,]
M1t=new.scaled.reg[5:(nrow(new.GDPreturns)-4),1]
M2t=new.scaled.reg[5:(nrow(new.GDPreturns)-4),2]
M3t=new.scaled.reg[5:(nrow(new.GDPreturns)-4),3]
M4t=new.scaled.reg[5:(nrow(new.GDPreturns)-4),4]
M5t=new.scaled.reg[5:(nrow(new.GDPreturns)-4),5]

M1t_1=new.scaled.reg[4:(nrow(new.GDPreturns)-5),1]
M2t_1=new.scaled.reg[4:(nrow(new.GDPreturns)-5),2]
M3t_1=new.scaled.reg[4:(nrow(new.GDPreturns)-5),3]
M4t_1=new.scaled.reg[4:(nrow(new.GDPreturns)-5),4]
M5t_1=new.scaled.reg[4:(nrow(new.GDPreturns)-5),5]

fedf<-as.vector(read.csv('/Users/suyue/Desktop/fe800/data/economic_predictors/FedF.csv',sep=','))
ztf<-fedf$Value[5:(nrow(fedf)-4)]
ztf<-scale(ztf)
ztf_1<-fedf$Value[4:(nrow(fedf)-5)]
ztf_1<-scale(ztf_1)

term.sp<-as.vector(read.csv('/Users/suyue/Desktop/fe800/data/economic_predictors/termspread.csv',sep=','))

ztsp<-term.sp$Value[5:(nrow(term.sp)-4)]


ztsp<-scale(ztsp)
ztsp_1<-fedf$Value[4:(nrow(fedf)-5)]
ztsp_1<-scale(ztsp_1)

temp<-cbind(new.GDPreturns$Value[5:(nrow(new.GDPreturns)-4)],yth$Value,y4$Value,y3$Value,y2$Value,y1$Value,M1t,M2t,M3t,M4t,M5t,M1t_1,M2t_1,M3t_1,M4t_1,M5t_1,ztf,ztf_1,ztsp,ztsp_1)
colnames(temp)<-c("yt","Yh","y4",'y3','y2','y1','M1t','M2t','M3t','M4t','M5t','M1t1','M2t1','M3t1','M4t1','M5t1','FedF','FedF_1',"TermSpread","TermSpread_1")
temp<-as.data.frame(temp)
coeff.gdp<-temp[10:nrow(temp),]


#====lm regression============



fitbench<-lm(Yh ~ y4+y3+y2+y1, data=temp)
ab<-summary(fitbench)
ab$r.squared

fit.m1 <- lm(Yh ~ y4+y3+y2+y1+ M1t + M1t1, data=temp)
m1<-summary(fit.m1)
m1$r.squared
coefficients(fit.m1)

fit.m2<-lm(Yh ~ y4+y3+y2+y1+M2t + M2t1, data=coeff.gdp)
m2<-summary(fit.m2)
coefficients(fit.m2)
m2$r.squared

fit.m3<-lm(Yh ~ y4+y3+y2+y1+M3t+M3t1, data=coeff.gdp)
m3<-summary(fit.m3)
coefficients(fit.m3)
m3$r.squared

fit.m4<-lm(Yh ~ y4+y3+y2+y1+M4t + M4t1, data=coeff.gdp)
m4<-summary(fit.m4)
coefficients(fit.m4)
m4$r.squared

fit.m5<-lm(Yh ~ y4+y3+y2+y1+M5t + M5t1, data=coeff.gdp)
m5<-summary(fit.m5)
coefficients(fit.m5)


fit.mt<-lm(Yh ~ y4+y3+y2+y1+ M1t + M1t1+ M2t + M2t1+ M3t + M3t1+ M4t + M4t1+M5t + M5t1, data=coeff.gdp)
summary(fit.mt)
coefficients(fit.mt)

fit.fed<-lm(Yh ~ y4+y3+y2+y1+ztf + ztf_1, data=temp)
mfed<-summary(fit.fed)
coefficients(fit.fed)
mfed$r.squared


fit.sp<-lm(Yh ~ y4+y3+y2+y1+ztsp + ztsp_1, data=temp)
msp<-summary(fit.sp)
coefficients(fit.sp)
msp$r.squared

fit.eco<-lm(Yh ~ y4+y3+y2+y1+ztsp + ztsp_1+ztf+ztf_1, data=temp)
meco<-summary(fit.eco)
meco$r.squared
coefficients(fit.eco)

fit.eco<-lm(Yh ~ y4+y3+y2+y1+M3t + M3t1+ztsp + ztsp_1+ztf+ztf_1, data=temp)
meco<-summary(fit.eco)
meco$r.squared
coefficients(fit.eco)

#new method:
install.packages("dLagM")
library(dLagM)
model.ardl = ardlDlm(x = temp$M3t,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)
MASE(model.ardl)

model.ardl = ardlDlm(x = temp$M1t,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)

model.ardl = ardlDlm(x = temp$M2t,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)

model.ardl = ardlDlm(x = temp$M4t,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)
model.ardl = ardlDlm(x = temp$M5t,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)

model.ardl = ardlDlm(x = temp$FedF,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)

model.ardl = ardlDlm(x = temp$TermSpread,
                     y = temp$yt, p = 1 , q = 4 , show.summary = TRUE)

model.dlm = ardlDlm(formula = yt ~ M1t+M2t+M3t+M4t+M5t ,
                data = data.frame(temp), p = 1 , q = 4, show.summary = TRUE)

model.dlm = ardlDlm(formula = yt ~ FedF+TermSpread ,
                    data = data.frame(temp), p = 1 , q = 4, show.summary = TRUE)

model.dlm = ardlDlm(formula = yt ~M3t+ FedF+TermSpread ,
                    data = data.frame(temp), p = 1 , q = 4, show.summary = TRUE)

#ardlDlmForecast(model = model.ardl , x = coeff.gdp$M3t,
#                h = 4 , interval = FALSE)

coefficients(model.ardl)

#===========Macro Varables============
macro<-as.vector(read.csv("/Users/suyue/Desktop/fe800/data/economic_predictors/Macro.csv"))
macro.v<-macro[156:284,]
adf.test(macro.v[,2])
adf.test(macro.v[,3])

#Comsumption
coeff.mac<-cbind(macro.v$PCEC[9:nrow(macro.v)],macro.v$PCEC[5:(nrow(macro.v)-4)],macro.v$PCEC[4:(nrow(macro.v)-5)],macro.v$PCEC[3:(nrow(macro.v)-6)],macro.v$PCEC[2:(nrow(macro.v)-7)],macro.v$PCEC[1:(nrow(macro.v)-8)],M3t,M3t_1,ztf,ztf_1,ztsp,ztsp_1)
colnames(coeff.mac)<-c("PCEC","yt","y4","y3","y2","y1","M3t","M3t_1","ztf","ztf_1","ztsp","ztsp_1")
coeff.mac<-as.data.frame(coeff.mac)

adc<-lm(PCEC~y4+y1+y2+y3,data=coeff.mac)
adc<-summary(adc)
coefficients(adc)
adc$r.squared

adc<-lm(PCEC~y4+y1+y2+y3+M3t+M3t_1,data=coeff.mac)
adc<-summary(adc)
coefficients(adc)
adc$r.squared

adc<-lm(PCEC~y4+y1+y2+y3+M3t+M3t_1+ztf+ztf_1+ztsp+ztsp_1,data=coeff.mac)
adc<-summary(adc)
adc$r.squared

model.dlm = ardlDlm(y = coeff.mac$yt , x=coeff.mac$M3t ,
                    data = data.frame(coeff.mac), p = 1 , q = 4, show.summary = TRUE)


model.dlm = ardlDlm(yt ~M3t + ztf+ztsp ,
                    data = data.frame(coeff.mac), p = 1 , q = 4, show.summary = TRUE)


coeff.mac$PCEC

#Fixed investment
coeff.invest<-cbind(macro.v$FPI[9:nrow(macro.v)],macro.v$FPI[5:(nrow(macro.v)-4)],macro.v$FPI[4:(nrow(macro.v)-5)],macro.v$FPI[3:(nrow(macro.v)-6)],macro.v$FPI[2:(nrow(macro.v)-7)],macro.v$FPI[1:(nrow(macro.v)-8)],M3t,M3t_1,ztf,ztf_1,ztsp,ztsp_1)
colnames(coeff.invest)<-c("FPI","yt","y4","y3","y2","y1","M3t","M3t_1","ztf","ztf_1","ztsp","ztsp_1")
coeff.invest<-as.data.frame(coeff.invest)

adc<-lm(FPI~y4+y1+y2+y3,data=coeff.invest)
adc<-summary(adc)
coefficients(adc)
adc$r.squared

adc<-lm(FPI~y4+y1+y2+y3+M3t+M3t_1,data=coeff.invest)
adc<-summary(adc)
coefficients(adc)
adc$r.squared




adc<-lm(FPI~y4+y1+y2+y3+M3t+M3t_1+ztf+ztf_1+ztsp+ztsp_1,data=coeff.invest)
adc<-summary(adc)
adc$r.squared

model.dlm = ardlDlm(y = coeff.invest$yt , x=coeff.mac$M3t ,
                    data = data.frame(coeff.invest), p = 1 , q = 4, show.summary = TRUE)


model.dlm = ardlDlm(yt ~M3t + ztf+ztsp ,
                    data = data.frame(coeff.invest), p = 1 , q = 4, show.summary = TRUE)

#====gdp&variables=====
a<-cbind((GDPreturns$Value)[52:180],scaled.reg[52:180,])
a<-as.data.frame(a)
fit1<-lm(V1~ mean.col,data=a)
fit2<-lm(V1~ disp.col,data=a)
fit3<-lm(V1~ skew.col,data=a)

fit4<-lm(V1~ left.col,data=a)
fit5<-lm(V1~ right.col,data=a)

g1<-summary(fit1)
g2=summary(fit2)
g3=summary(fit3)
g4=summary(fit4)
g5=summary(fit5)

g1$r.squared
g2$r.squared
g3$r.squared
g4$r.squared
g5$r.squared

g1
g2
g3
g4
g5


coefficients(fit1)
coefficients(fit2)
coefficients(fit3)
coefficients(fit4)
coefficients(fit5)


plot(scaled.reg[1:180,3],(GDPreturns$Value),type = "p",xlim=c(-2,4),ylim=c(0,6),ylab = "GDPGrowth", xlab = "Financial Skewness")
abline(lm(GDPreturns$Value~scaled.reg[1:180,3]), col="red")

#==========moving everage============

ab<-rollmean(skew.col,4)
ab<-c(0,0,0,ab)
c=ab*6.5+1.5
d=new.GDPreturns[,2]

date<-as.Date(new.GDPreturns$Date,format="%m/%d/%Y")
mov.plot<-as.data.frame(cbind(as.character(date),c[52:180]))
mov.plot$V1<-as.Date(mov.plot$V1)
mov.plot<-xts(mov.plot[,-1], order.by=as.Date(mov.plot[,1], "%m/%d/%Y"))


ab<-rollmean(skew.col,4)

gdpgrowth<-xts(new.GDPreturns[,-1], order.by=as.Date(new.GDPreturns[,1], "%m/%d/%Y"))
plot(gdpgrowth,type="l", col='blue',ylim=c(-2,3),main=' ')
lines(mov.plot,type="l",col="red",lwd=2)
legend("right", c("GDP growth","Financial skewness"), lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"),pch = 1)
