library(tidyverse)
library(ggplot2)
library(haven)
library(ltm)
# library(readr)
library(car)
#library(dplyr)
################################################################################
# DATA SET-UP
########################
setwd("C:/Users/phams/OneDrive/Documents/stat5214g")
# Paths to data files
data_path = "data/sleep_study"
data_p_path = paste(sep = "/", data_path, "T5 Parent Raw and Scored_5.12.2020.sav")
data_a_path = paste(sep = "/", data_path, "T5 Adolescent Raw and Scored_6.11.2020.sav")
data_csv_path = paste(sep = "/", data_path, "t5_data.csv")
data_item_parent_path = paste(sep = "/", data_path, "data_item_parent.csv")
data_item_adolescent_path = paste(sep = "/", data_path, "data_item_adolescent.csv")
data_sav_path = paste(sep = "/", data_path, "T5 Sleep Study Dataset.sav")

########################
# Read in path variable above for dataset that you want to pull in
  t5_data = read_csv(data_csv_path)

########################
# Create dataframe with only variables of interest
data_all = t5_data[,c(
  "ID",
  "Group",
  #"MedT5D",
  #Parent ADHD (BAARS-IV)
  "BAAToM",
  # "BAAINM",
  # "BAAHIM",
  #Parent DASS
  "DASdeS",
  "DASanS",
  "DASstS",
  "DASinS2",
  "DAStoS2",
  #Parent DERS
  "DERpToS",
  # "DERpStrS5",
  # "DERpNonS5",
  # "DERpImpS5",
  # "DERpGoaS5",
  # "DERpClaS5",
  #Parent PBS
  "PBSposS",
  "PBSdisS",
  "PBSmatS",
  "PBSteaS",
  #Adolescent ER
  "ERCToM",
  "ERCNEGM",
  "ERCREGM",
  #Adolescent DERS
  "DERaToS"
  # "DERaStrS",
  # "DERaNonS",
  # "DERaImpS",
  # "DERaGoaS",
  # "DERaClaS"
)]

# ADHD only dataset
#
data_all$Group = factor(data_all$Group, levels = c(0,1), labels = c("non-ADHD", "ADHD"))
data_adhd = data_all[data_all$Group == "ADHD", ]

# Comparison dataset
#
data_nonadhd = data_all[data_all$Group == "non-ADHD", ]
################################################################################
################################################################################
#
# Negative lability subscale
mdl_neg = lm(ERCNEGM ~ BAAToM + DAStoS2 + DERpToS + Group , data=data_all)
mdl_neg_adhd = lm(ERCNEGM ~ BAAToM + DAStoS2 + DERpToS , data=data_adhd)
mdl_neg_nonadhd = lm(ERCNEGM ~ BAAToM + DAStoS2 + DERpToS , data=data_nonadhd)

# Emotion regulation subscale
mdl_er = lm(ERCREGM ~ BAAToM + DAStoS2 + DERpToS +  Group, data=data_all)
mdl_er_adhd = lm(ERCREGM ~ BAAToM + DAStoS2 + DERpToS , data=data_adhd)
mdl_er_nonadhd = lm(ERCREGM ~ BAAToM + DAStoS2 + DERpToS , data=data_nonadhd)

# Emotion dysregulation (DERS) subscale
mdl_der = lm(DERaToS ~ BAAToM + DAStoS2 + DERpToS +  Group, data=data_all)
mdl_der_adhd = lm(DERaToS ~ BAAToM + DAStoS2 + DERpToS , data=data_adhd)
mdl_der_nonadhd = lm(DERaToS ~ BAAToM + DAStoS2 + DERpToS , data=data_nonadhd)

# Studentized values
  # Studentized values for each ### Neg/Lab
    neg_stud = rstudent(mdl_neg)
    neg_adhd_stud = rstudent(mdl_neg_adhd)
    neg_nonadhd_stud = rstudent(mdl_neg_nonadhd)
      
  # Studentized values for each ### EmoReg
    er_stud = rstudent(mdl_er)
    er_adhd_stud = rstudent(mdl_er_adhd)
    er_nonadhd_stud = rstudent(mdl_er_nonadhd)
    
  # Studentized values for each ### EmoDysreg
    der_stud = rstudent(mdl_der)
    der_adhd_stud = rstudent(mdl_der_adhd)
    der_nonadhd_stud = rstudent(mdl_der_nonadhd)
    
# Fitted residuals
  # Fitted resid. values for each ### Neg/Lab
    neg_fitted = fitted(mdl_neg)
    neg_adhd_fitted = fitted(mdl_neg_adhd)
    neg_nonadhd_fitted = fitted(mdl_neg_nonadhd)
    
  # Fitted resid. values for each ### EmoReg
    er_fitted = fitted(mdl_er)
    er_adhd_fitted = fitted(mdl_er_adhd)
    er_nonadhd_fitted = fitted(mdl_er_nonadhd)
    
  # Fitted resid. values for each ### EmoDysreg
    der_fitted = fitted(mdl_der)
    der_adhd_fitted = fitted(mdl_der_adhd)
    der_nonadhd_fitted = fitted(mdl_der_nonadhd)

# Summaries of ERC scale models separately
# Neg/Lab
###################### 
# Model fit R-sq is best for combined group on Neg/Lab scale
# Correlations were all going in correct direction (accord to theory) for every predictor (regardless of model)
######################
summary(mdl_neg)
summary(mdl_neg_adhd)
summary(mdl_neg_nonadhd)

# Emo. Reg.
###################### 
# Model fit R-sq is best for combined group on EmoReg scale
# All correlations were going in opposite direction on every predictor (regardless of model)
######################
summary(mdl_er)
summary(mdl_er_adhd)
summary(mdl_er_nonadhd)

# Emo. Dysreg.
###################### 
# Model fit R-sq is best for combined group on EmoReg scale
# All correlations were going in opposite direction on every predictor (regardless of model)
######################
summary(mdl_der)
summary(mdl_der_adhd)
summary(mdl_der_nonadhd)


#####################
# Plots
#####################
plot_col = c("navy", "darkorange")[data_all$Group]
### Neg/Lab - 
  
  # All
    par(mfrow=c(2,2))
    plot(mdl_neg)
    
  # adhd
    par(mfrow=c(2,2))
    plot(mdl_neg_adhd)
    
  # non-adhd
    par(mfrow=c(2,2))
    plot(mdl_neg_nonadhd)
    
### EmoReg - 
    
  # All
    par(mfrow=c(2,2))
    plot(mdl_er)
    
  # adhd
    par(mfrow=c(2,2))
    plot(mdl_er_adhd)
    
  # non-adhd
    par(mfrow=c(2,2))
    plot(mdl_er_nonadhd)
  
###############################################
# Studentized residuals
##  Neg/Lab scale
    
    plot_neg = plot(neg_fitted, neg_stud,
                       main="Studentized Residuals vs. Predicted", 
                       xlab="Predicted Adolescent Neg/Lab", 
                       ylab="Studentized Residuals",
                       ylim=(c(-max(neg_stud), max(neg_stud)))
    );
    abline(h = 0,col = 'violet')
    abline(h = 2.5,col = 'blueviolet')
    abline(h = -2.5,col = 'blueviolet')


plot_neg_bc = plot(ER_neg_fitted, stud_ER_neg,
                       main="Studentized Residuals vs. Predicted (All, Emotion Lability)", 
                       xlab="Predicted Adolescent ER", 
                       ylab="Studentized Residuals",
                       ylim=(c(-max(stud_ER_neg), max(stud_ER_neg)))
);
abline(h = 0,col = 'violet')
abline(h = 2.5,col = 'blueviolet')
abline(h = -2.5,col = 'blueviolet')

par(mfrow=c(2,2))
plot(mdl_ER_neg)

# Q-Q plot
qqnorm(neg_stud,pch = 16);abline(0,1, col="red")

# VIF
vif_ER_neg = vif(mdl_ER_neg)
vif_ER_neg

# Shapiro-Wilk Test
shapiro.test(stud_neg)


###################################################
# BOXCOX
###################################################
bc = boxcox(mdl_neg)
# Exact lambda
lambda <- bc$x[which.max(bc$y)]
lambda

###################
# TRANSFORMED MODEL
data_all$neg_bc = (data_all$ERCNEGM^lambda-1)/lambda
mdl_neg_bc = lm(neg_bc ~ BAAToM + DAStoS2 + DERpToS +  Group, data=data_all)
summary(mdl_neg_bc)

###################
# New plots post boxcox
plot(mdl_neg_bc)

  # studentized resid. & fitted values
  stud_neg_bc = rstudent(mdl_neg_bc)  #studentized resid.
  fitted_neg_bc = fitted(mdl_neg_bc)  #fitted values

  # plot
  plot_neg_bc = plot(fitted_neg_bc, stud_neg_bc,
                     
                                main="Studentized Residuals vs. Predicted", 
                                xlab="Predicted Adolescent Neg. Lability", 
                                ylab="Studentized Residuals",
                                ylim=(c(-max(stud_neg_bc), max(stud_neg_bc)))
  );
  abline(h = 0,col = 'violet')
  abline(h = 2.5,col = 'blueviolet')
  abline(h = -2.5,col = 'blueviolet')

  which(abs(stud_neg_bc)>2.5)

  # 4 plots
  par(mfrow=c(2,2))
  plot(mdl_new_neg)
  
  # Q-Q plot
  qqnorm(stud_neg_bc,pch = 16);abline(0,1, col="red")

# Shapiro-wilk

shapiro.test(rstudent(mdl_neg))
shapiro.test(rstudent(mdl_neg_bc))


#########################
# CHECK OUTLIERS
#########################
# cutoff
neg_bc_outlier = hatvalues(mdl_neg_bc);
p = 3; n = 266;
cutoff = (2*p)/n

# Plot of leverage points, separated by ADHD status
plot(neg_bc_outlier, 
     main = "Leverage Points", 
     ylab = 'Leverage Values',
     xlab = 'Observation Index',
     col = plot_col,
     pch = 16);
abline(h = cutoff, col = 'blueviolet');
legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)


which(h>cutoff) 
which(mileage_outlier==max(mileage_outlier))

c <- cooks.distance(mdl_neg_bc)
plot(c, main = "HIP Plots", 
     ylab = 'Cooks Distance',
     xlab = 'Observation Index',
     col = plot_col,
     pch = 16);
abline(h = 1, col = 'blueviolet');
legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)
which(c > cutoff) # Prints the observations that are greater than the cutoff.
#########################
# REMOVE OUTLIERS
#########################
no_outliers = data_all[-c(153, 252, 293, 84),]

mdl_no_outliers = lm(neg_bc ~ BAAToM + DAStoS2 + DERpToS +  Group, data=no_outliers)
summary(mdl_no_outliers)

# studentized resid. & fitted values
stud_no_outliers = rstudent(mdl_no_outliers)  #studentized resid.
fitted_no_outliers = fitted(mdl_no_outliers)  #fitted values

# plot
plot_no_outliers = plot(fitted_no_outliers, stud_no_outliers,
                   main="Studentized Residuals vs. Predicted", 
                   xlab="Predicted Adolescent Neg. Lability", 
                   ylab="Studentized Residuals",
                   ylim=(c(-max(stud_no_outliers), max(stud_no_outliers)))
);
abline(h = 0,col = 'violet')
abline(h = 2.5,col = 'blueviolet')
abline(h = -2.5,col = 'blueviolet')

stud_no_outliers[stud_no_outliers>2.5]

#########################
# INTERACTION
#########################

# fit each X w/ grouping variable
mdl_int = lm(neg_bc ~ BAAToM + DAStoS2 + DERpToS + BAAToM:Group + DAStoS2:Group + DERpToS:Group, data=no_outliers)
summary(mdl_int)
#
mdl_int_2 = lm(neg_bc ~ BAAToM + Group +BAAToM:Group, data=no_outliers)
summary(mdl_int_2)

mdl_int_3 = lm(neg_bc ~ DAStoS2 + Group +DAStoS2:Group, data=no_outliers)
summary(mdl_int_3)

mdl_int_4 = lm(neg_bc ~ DERpToS + Group +DERpToS:Group, data=no_outliers)
summary(mdl_int_4)
#
plot(mdl_int)

xrange = seq(0, 60, 0.1)
yfit_adhd = mdl_no_outliers$coeff[1] + mdl_no_outliers$coeff[2]*xrange + mdl_no_outliers$coeff[3]*xrange + mdl_no_outliers$coeff[4]*xrange
yfit_nonadhd = yfit_adhd + mdl_no_outliers$coeff[5]

plot(no_outliers$BAAToM,
     no_outliers$neg_bc,
     ylab = 'Adolescent Neg. Emotionality/Lability',
     xlab = 'Caregiver Variables',
     col = plot_col,
     pch = 16,
     xlim = (c(0,60)),
     ylim = (c(0,3)),
     );
points(no_outliers$DAStoS2,
       no_outliers$neg_bc,
       col = plot_col,
       pch = 18, );
points(no_outliers$DERpToS,
       no_outliers$neg_bc,
       col = plot_col,
       pch = 15, );
lines(xrange, yfit_adhd, col = "navy");
lines(xrange, yfit_nonadhd, col = "darkorange");
legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)


##################
# FIT
##################
all_possible = ols_step_all_possible(mdl_neg_bc)
a = all_possible[["result"]]
plot(all_possible)

adhd_all = ols_step_all_possible(mdl_neg_adhd)
b = adhd_all[["result"]]


nonadhd_all = ols_step_all_possible(mdl_neg_nonadhd)
c = adhd_all[["result"]]

#################
# SCATTERPLOTS
#################
# Parent ADHD
plot(data_all$BAAToM,data_all$ERCNEGM, 
     main="Parent ADHD x Adolescent Neg/Lab", 
     xlab="Parent ADHD ", 
     ylab="Adolescent Neg/Lab", 
     bg="navy",
     cex=1.25,
     col = plot_col,
     pch=19); legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)

# Parent total stress, anxiety, depression
plot(data_all$DAStoS2,data_all$ERCNEGM, 
     main="Parent Psychopathology x Adolescent Neg/Lab", 
     xlab="Parent Psychopathology ", 
     ylab="Adolescent Neg/Lab", 
     col=plot_col, 
     bg="navy",
     cex=1.25, 
     pch=19); legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)

# Parent ED
plot(data_all$DERpToS,data_all$ERCNEGM, 
     main="Parent ED x Adolescent Neg/Lab", 
     xlab="Parent ED ", 
     ylab="Adolescent Neg/Lab", 
     col=plot_col, 
     bg="navy",
     cex=1.25, 
     pch=19); legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)

# Adolescent ADHD Status
plot(data_all$Group,data_all$ERCNEGM, 
     main="Adolescent x Adolescent Neg/Lab", 
     xlab=" Adolescent ADHD Status", 
     ylab="Adolescent Neg/Lab", 
     col=plot_col, 
     bg="navy",
     cex=1.25, 
     pch=19); legend(x="topright", legend=c("non-adhd", "adhd"), fill = plot_col)
####################
# FINAL MODEL
####################
# FINAL MODEL
mdl_final = lm(neg_bc ~ BAAToM + DERpToS +  Group, data=data_all)
summary(mdl_final)

###################
# New plots post boxcox

par(mfrow=c(2,2))

# studentized resid. & fitted values
stud_final = rstudent(mdl_final)  #studentized resid.
fitted_final = fitted(mdl_final)  #fitted values

# plot
plot_final = plot(fitted_final, stud_final,
                   
                   main="Studentized Residuals vs. Predicted", 
                   xlab="Predicted Adolescent Neg. Lability", 
                   ylab="Studentized Residuals",
                   ylim=(c(-max(stud_final), max(stud_final)))
);
abline(h = 0,col = 'violet')
abline(h = 2.5,col = 'blueviolet')
abline(h = -2.5,col = 'blueviolet')

which(abs(stud_neg_bc)>2.5)

# Q-Q plot
qqnorm(stud_final,pch = 16);abline(0,1, col="red")

# Shapiro-wilk

shapiro.test(rstudent(mdl_neg))
shapiro.test(rstudent(mdl_neg_bc))