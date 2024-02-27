rm(list= ls())
#install.packages("gridExtra")
# Script Name:
#   11_Model Selection.R
##
# Purpose: 
#   In this script, we will walk through the lm() procedure for MLR
#   including fitting the model and confidence/prediction intervals.

####  

#



#######################################################

##Bring in the cement data

setwd("C:/Users/agrya/Google Drive/Stat 4214/2023 5214G Psych/R")
Data=read.csv("11_cement.csv")


library(ggplot2)

library(gridExtra)


##Creating a grid plot of scatterplats for each predictor
p1=ggplot(Data, aes(y=y,x=x1))+geom_point()+theme_bw()+
  labs(y="Heat Evolved",x="Tricalcium Aluminate (%)")
p2=ggplot(Data, aes(y=y,x=x2))+geom_point()+theme_bw()+
  labs(y="Heat Evolved",x="Tricalcium Silicate (%)")
p3=ggplot(Data, aes(y=y,x=x3))+geom_point()+theme_bw()+
  labs(y="Heat Evolved",x="Tetracalcium Alumino Ferrite (%)")
p4=ggplot(Data, aes(y=y,x=x4))+geom_point()+theme_bw()+
  labs(y="Heat Evolved",x="Dicalcium Silicate (%)")

grid.arrange(p1,p2,p3,p4,ncol=2)


## Best Subset Selection

Fullmodel=lm( y~x1+x2+x3+x4,data=Data)
library(olsrr)

allpossible=ols_step_all_possible(Fullmodel)
names.all=c("n","predictors","rsquare","adjr","cp","aic") #Extracting the metrics of interest
ind=which(names(allpossible)%in%names.all) # selecting the columns 
knitr::kable(allpossible[ind]) # creating the table
plot(allpossible)
allpossible


## Forward Selection 


ols_step_forward_p(Fullmodel,penter=0.25)
ols_step_forward_aic(Fullmodel)

## Backward Elimination
ols_step_backward_p(Fullmodel,prem=0.10)
ols_step_backward_aic(Fullmodel)

## Stepwise Selection

ols_step_both_p(Fullmodel,pent=0.25,prem=0.25)
ols_step_both_aic(Fullmodel)

## Forward/Stepwise Selected Model 
Model=lm(y~x1+x2+x4,Data)
summary(Model)
vif(Model)

##Backward Model
Model2=lm(y~x1+x2,Data)
summary(Model2)
vif(Model2)



