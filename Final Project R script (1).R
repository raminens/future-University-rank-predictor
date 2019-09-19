install.packages("car")
library(car)
install.packages("e1071")
library(e1071)
install.packages("GGally")
library(GGally)
install.packages("corrplot")
library(corrplot)
install.packages("ztable")
library(ztable)
install.packages("shiny")
library(shiny)
install.packages("shinydashboard")
library(shinydashboard)
install.packages("ggplot2")
library(ggplot2)
install.packages("VIF")
library(VIF)
install.packages("qqPlot")
library(qqPlot)

getwd()
setwd("C:/Users/kalya/OneDrive/Desktop")

PerEval = read.csv("2017 Universities ranking.csv", header=T)
data1 = cor(PerEval[,c(3:17)])
data1
data2 = PerEval[,c(3:17)]
data2
## Density and Boxplots

for (k in names(data2)){
  png(file=paste(k,"_dens_scatter_box" ,".png", sep=""), width=5000, height=5000)
  x <- as.numeric (data2[, k])
  Skewness <- round(skewness(x), 2)  # calc skewness
  dens <- density(x, na.rm=T)  # density func
  par(mfrow=c(1, 2))  # setup plot-area in 3 columns
  
  # Density plot
  plot(dens, type="l", col="red", ylab="Frequency", xlab = k, main = paste(k, ": Density Plot"), sub=paste("Skewness: ", Skewness))
  polygon(dens, col="red")
  # boxplot
  boxplot(x, main=paste(k, ": Box Plot"), sub=paste("Outliers: ", paste(boxplot.stats(x)$out, collapse=" ")))
  dev.off() 
  
}

## Correlation Matrix
par("mar")
par(mar=c(1,1,1,1))
corrplot(data1, method = "number", number.cex=0.75)


#Principal component analysis#
pcal <-princomp(data2, scores=TRUE, cor=TRUE)
summary(pcal)
loadings(pcal)
plot(pcal)
biplot(pcal)
pcal$scores[1:10,]

#factor analysis#
install.packages('psych')
install.packages('GPArotation')
library("psych")
library("GPArotation")
fact=factanal(data2,factor = 3, rotation= "none", scores = "regression")
parallel <- fa.parallel(data2, fm = 'minres', fa = 'fa')
threefactor <- fa(data2,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)
print(threefactor$loadings,cutoff = 0.3)
fourfactor <- fa(data2,nfactors = 4,rotate = "oblimin",fm="minres")
print(fourfactor$loadings,cutoff = 0.3)
fa.diagram(fourfactor)

## Multiple regression

Perf <- lm(World_Rank ~ INT + Research1 + Citations + Industry_Income_Rating  , data= PerEval)
summary(Perf)

Perf <- lm(World_Rank ~   Teaching_Rating + INT + Research1 + Citations + Industry_Income_Rating  + Total_Score + Num_Students + Student.Staff_Ratio + X._Inter_Students + Teaching.1  +Fem1  , data= PerEval)
summary(Perf)
install.packages("VIF")
library(VIF)
vif(Perf)
alias(Perf)
plot(Perf)



##Normality
hist(residuals(Perf),col="darkgray")
qqPlot(Perf, main="QQ Plot")


##Linearity 
plot(fitted(Perf),residuals(Perf))
abline(0,1, col="blue", lwd=1)
##Independence
durbinWatsonTest(Perf)




##Equality
plot(residuals(Perf), PerEval$Teaching_Rating)
plot(fitted(Perf),residuals(Perf))
abline(0,1, col="blue", lwd=2)



model.null = lm(World_Rank ~ 1,
                data=PerEval)
summary(model.null)
#final models
model.full = lm(World_Rank ~ Teaching_Rating + INT + Research1 + Citations + Industry_Income_Rating  + Total_Score + Num_Students + Student.Staff_Ratio + X._Inter_Students + Teaching.1  +Fem1  , data= PerEval)
summary(model.full)


#STEP analysis
step(model.null,
     scope = list(upper=model.full),
     direction="both",
     data=PerEval)

#final models
model.full = lm(World_Rank ~  Teaching_Rating + INT + Research1 + Citations + Industry_Income_Rating + Total_Score + Num_Students + Student.Staff_Ratio + X._Inter_Students + Teaching.1  +Fem1 , data= PerEval)
summary(model.full)

#Cross-ValidaTION#
install.packages("boot")
library("boot")
set.seed(101)
sample <- sample.int(n = nrow(PerEval), size = floor(.50*nrow(PerEval)), replace = F)
train <- PerEval[sample, ]
test  <- PerEval[-sample, ]
test


#Confidence intervals#
(ci <- confint(model.full))
(exp(coef(model.full)))
exp(cbind(OR = coef(model.full), ci))

#coefficients of individual variables#
(ctable = coef(summary(model.full)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 
(ctable <- cbind(ctable, "p value" = p))


install.packages("psych")
library("psych")
fit <- principal(data2, nfactors=5, rotate="varimax")
fit <- factanal(data2, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
#Prediction for the world rankings#
fitted.rev <- predict(model.full)
fitted.rev
predicted.rev <- predict(model.full, newdata = PerEval, interval='prediction')
predicted.rev
#Prediction for 50 university rankings#
data3 = PerEval[(150:200),c(3:17)]
pred <- predict(model.full, data3)
pred

#time series analysis#
data4 = PerEval[(191:200),c(3)]

myts <- ts(data4,  frequency=10) 
myts
fit<- decompose(myts)
plot(fit)

