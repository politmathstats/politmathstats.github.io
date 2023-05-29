### Regression analysis. Lab 3
### Multiple regression. Heteroskedasticity
### March 24, 2023

install.packages("haven")
install.packages("ggplot2")
install.packages("sandwich")
install.packages("lmtest")
install.packages("skedastic")

library(haven)
library(ggplot2)
library(sandwich)
library(lmtest)
library(skedastic)

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

m1 <- lm(cpi ~ dem + fp + loggdppc + stab + britcol, data = lab2)
summary(m1)

# visualize the relationship between the key predictors and Y 
ggplot(lab2, aes(dem, cpi)) + geom_point()
ggplot(lab2, aes(fp, cpi)) + geom_point() 

ggplot(lab2, aes(m1$fitted.values, cpi)) + geom_point()

# visualize the relationship between the squared residuals and the predictor variables
ggplot(lab2, aes(dem, m1$residuals^2)) + geom_point()
ggplot(lab2, aes(fp, m1$residuals^2)) + geom_point()

ggplot(lab2, aes(m1$fitted.values, m1$residuals^2)) + geom_point()

# formal tests: 
bptest(m1) # Breusch-Pagan test
white(m1, interactions = T) # White test

ggplot(lab2, aes(stab, m1$residuals^2)) + geom_point()
gqtest(m1, order.by = ~stab, data = lab2, fraction = 0.2, alternative = "two.sided") # Goldfeld-Quandt test: we suggest that variance of residuals depends on control of corruption 

# adjusted standard errors (heteroskedasticity-consistent)
vcovHC(m1)
coeftest(m1, vcov = vcovHC, type = "HC3") 
