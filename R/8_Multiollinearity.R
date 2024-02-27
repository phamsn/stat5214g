rm(list= ls())

# Script Name:
#   10_Collinearity.R
##
# Purpose: 
#   In this script, we will walk through multicollinearity diagnostics
####  



setwd("C:/Users/agrya/Google Drive/Stat 4214/2023 5214G Psych/R")
  
# Hospital Example 


Data=read.csv("10_Hospital Data.csv")


# Collinearity Assessment

#correlation
cor(Data[1:5],)

## Scatterplot Matrix

library(psych)


pairs.panels(Data[1:5], 
             method = "pearson", # correlation method
             hist.col = "mediumorchid",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses

library(GGally)

ggpairs(Data,1:5,
        upper = list(continuous = "smooth"),
        lower = list(continuous = "cor"),
        diag = list(continuous = "blankDiag")
)

## VIF

# First you have to fit your model

modfit=lm(Man.Hours ~ Load + Xray +Bed.Days + Pop + Length.of.Stay, data = Data)

##Calcuates just the VIF
library(car)
vif(modfit)

#Package that calcuates the VIF and the tolerance
library(olsrr)
ols_vif_tol(modfit)



# Remedy - Subset Model

##Start building a model (more on this in later lectures)

ols_step_both_aic(modfit)
Stepwise=lm(Man.Hours ~ Xray + Bed.Days + Length.of.Stay, data = Data)
summary(Stepwise)

ols_vif_tol(Stepwise)


# What if we eliminate Xray?




Model2=lm(Man.Hours ~ Bed.Days + Length.of.Stay, data = Data)
summary(Model2)

ols_vif_tol(Model2)



# What if we eliminate bed days



Model3=lm(Man.Hours ~  Length.of.Stay + Xray, data = Data)
summary(Model3)

ols_vif_tol(Model3)

