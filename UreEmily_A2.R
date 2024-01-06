#simple linear regression
#load data

Auto <- read.csv("Auto.csv")

Auto <- read.csv("Auto.csv", header=T,na.strings="?") 

Auto=na.omit(Auto) 

fix(Auto)

#Use the lm () function to perform a simple linear regression with mpg as the dependent variable and
#weight as the predictor. Use the summary () function to print the regression results

lm.fit=lm(mpg~weight,data=Auto)

summary(lm.fit)



#What are the associated 95%
#confidence and prediction intervals? 

predict(lm.fit,data.frame(weight=2000), interval="confidence")

predict(lm.fit,data.frame(weight=2000), interval="prediction")

#Please make a scatter plot between the dependent variable (mpg) and the predictor (weight). Please
#display the least squares regression line in red color. Take a screenshot of your output.
attach(Auto)

length(weight) == length(mpg)

length(mpg)
length(weight)



plot(weight,mpg)

abline(lm.fit)

abline(lm.fit,lwd=3)

abline(lm.fit,lwd=3,col="red")


#Please produce four diagnostic plots of the least squares regression fit. Comment on each plot and
#conclude whether each plot indicates/shows some problems.


plot(lm.fit)


#Multiple Linear Regression

#First load the data. Use the lm () function to perform a multiple linear regression with Grad.Rate as the
#dependent variable and other 10 variables including Private, Apps, Accept, Enroll, Top10perc, Top
#25perc, PhD, Terminal, S.F.Ratio, Expend as the predictors (independent variables). Use the summary( )
#function to print the results. Take a screenshot of your output.

College <- read.csv("College.csv")

College <- read.csv("College.csv", header=T,na.strings="?") 

College=na.omit(College) 

fix(College)
View(College)


lm.fit2=lm(Grad.Rate~ Private+Apps+Accept+Enroll+Top10perc+Top25perc+PhD+Terminal+S.F.Ratio+Expend, data=College)
summary(lm.fit2)



#First use the * symbol to fit the linear regression model with interaction effects
#dependent variable is Grad.Rate; the two independent variables are Private and Top10perc;
# the interaction term is the product of Private and Top10perc


lm.fit3=lm(Grad.Rate~ Private*Top10perc, data=College)
summary(lm.fit3)


lm.fit4=lm(Grad.Rate~ Private:Top10perc+Private+Top10perc, data=College)
summary(lm.fit4)



#test VIF

lm.fit5=lm(Grad.Rate~ Private+Apps+Accept+Enroll+Top10perc+Top25perc+PhD+Terminal+S.F.Ratio+Expend, data=College)


library(car)

vif(lm.fit5)

#Backward Selection Method (refer textbook p.79) to decide the optimal model with all the remaining variables having p-values below 0.05. 
#Take a screenshot of the regression results for the final optimal model 

#(i.e. all the remaining variables have p values below 0.05).

lm.fit5=lm(Grad.Rate~ Private+Apps+Accept+Enroll+Top10perc+Top25perc+PhD+Terminal+S.F.Ratio+Expend, data=College)
summary(lm.fit5)


lm.fit6=lm(Grad.Rate~ Private+Apps+Accept+Enroll+Top10perc+Top25perc+PhD+Terminal+S.F.Ratio-Expend,data=College)
summary(lm.fit6)

lm.fit7=lm(Grad.Rate~ Private+Apps+Accept+Enroll+Top10perc+Top25perc+PhD+Terminal-Expend-S.F.Ratio,data=College)
summary(lm.fit7)

lm.fit8=update(lm.fit7, ~.-Terminal)
summary(lm.fit8)

lm.fit9=update(lm.fit8, ~.-Accept)
summary(lm.fit9)

lm.fit10=update(lm.fit9, ~.-Top10perc)
summary(lm.fit10)