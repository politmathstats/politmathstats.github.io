### Regression analysis. Lab 2
### Multiple regression. Multicollinearity
### March 10, 2023

install.packages("haven")
install.packages("psych")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("car")
install.packages("memisc")

library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(memisc)

## open reg_lab2.dta
lab2 <- read_dta(file.choose())
head(lab2)

## Description

# Source: Kalenborn C., Lessman C., 2013 
# Available at: https://yadi.sk/i/nlEQUoWKiqY0UA

# STATEMENT
# The data is used for the training purposes only. For the further use, please cite the original paper. 

## VARIABLES
# cpi - Corruption Perception Index. The scale ranges from 0 to 10, where 10 stands for the highest level of corruption
# dem - Vanhanen’s democratization index. The scale ranges from 0 to 100, where 100 stands for the highest level of democracy
# fp - freedom of press: Freedom House. The scale ranges from 0 to 100, where 100 stands for the highest level of freedom of press
# loggdppc - natural logarithm of GDP per capita: World Bank. 
# stab - political stability. The index is constructed on the basis of "Political Stability" and "Absence of Violence/Terrorism" indicators 
# from the Worldwide Governance Indicators. The scale ranges from -2.5 to 2.5, where 2.5 stands for the highest level of political stability  
# britcol - a dummy variable, 1 - former British colony, 0 - otherwise. 

lab2 <- na.omit(lab2)
describe(lab2)

## Visualize the relationship between democracy and corruption
ggplot(lab2, aes(dem, cpi))+geom_point()+geom_smooth(method = "lm")

## Run a pairwise linear regression model
m1 <- lm(cpi ~ dem, data = lab2)
summary(m1) 
# Comment on the relationship between democracy and corruption
# Is the predictor coefficient significant? 
# What does the R-squared measure indicate? Is it significant?

## Run a multiple linear regression model
m2 <- lm(cpi ~ dem + fp + loggdppc + stab + britcol, data = lab2)
summary(m2)
# How has the relationship between democracy and corruption changed? Why?
# Comment on the choice of control variables 

## perfect multicollinearity
lab2$notbritcol = ifelse(lab2$britcol == 1, 0, 1)
m3 <- lm(cpi ~ dem + fp + loggdppc + stab + britcol + notbritcol, data = lab2)
summary(m3) 

## the coefficient estimate for nonbritcol cannot be calculated. Why is this so?
X_nointercept <- dplyr::select(lab2, -c(country, cpi))
intercept_vector <- rep(1, dim(lab2)[1])
cbind(intercept_vector, X_nointercept)
X <- as.matrix(cbind(intercept_vector, X_nointercept))
X

inverse_matrix <- solve(t(X) %*% X) 

## Detecting the multicollinearity problem
# identify the approximate values of Pearson's correlation coefficients
# draw a more detailed graph
X_new <- dplyr::select(lab2, -c(country, cpi, britcol, notbritcol))
ggpairs(X_new)
# correlation heatmap
ggcorr(X_new, label = T, label_round = 3)
corrmatrix <- cor(X_new)
round(corrmatrix, 3)

# Calculating VIF. Auxiliary models
# Examine R-squared and their significance
m1_aux <- lm(dem ~ fp + loggdppc + stab + britcol, data=lab2)
m2_aux <- lm(fp ~ dem + loggdppc + stab + britcol, data=lab2)
m3_aux <- lm(loggdppc ~ fp + dem + stab + britcol, data=lab2)
m4_aux <- lm(stab ~ dem + fp + loggdppc + britcol, data=lab2)
m5_aux <- lm(britcol ~ dem + fp + loggdppc + stab, data=lab2)

mtable(m1_aux, m2_aux, m3_aux, m4_aux, m5_aux)

## calculate VIF 
vif <- vif(m2)
vif
1/(1 - summary(lm(dem ~ fp + loggdppc + stab + britcol, data=lab2))$r.sq)
# Interpret the VIF values
# VIF values higher than 10 indicate severe multicollinearity problems

## or tolerance: the bigger the better
tolerance <- 1/vif
tolerance

## How VIF and R-squared are related
rsquared <- seq(0, 1, by = 0.01)
vif_values <- 1/(1 - rsquared)
data <- data.frame(rsquared, vif_values)
ggplot(data, aes(rsquared, vif_values)) + geom_line() +  scale_x_continuous(breaks=seq(0,1,0.1))
