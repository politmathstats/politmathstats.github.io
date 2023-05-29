### Regression analysis. Lab 4
### Multiple regression. Measures of Influence
### April 7, 2023

install.packages("haven")
install.packages("car")
install.packages("dplyr")
install.packages("broom")
install.packages("olsrr")
install.packages("memisc")

library(haven)
library(car)
library(dplyr)
library(broom)
library(olsrr)
library(memisc)

# open reg_lab2.dta
data <- read_dta(file.choose())
head(data)
data <- na.omit(data)

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

m1 <- lm(cpi ~ dem + fp + loggdppc + stab + britcol, data)
summary(m1)

car::outlierTest(m1) # studentized residuals (outliers)
ols_plot_resid_stud(m1)
car::influenceIndexPlot(m1) # diagnostic plots (Cook's distance, hat values, studentized residuals, p-value Bonferroni)
car::influencePlot(m1) # hat-values VS Studentized residuals (visualize Cook's measure)

influence_data <- augment(m1) %>% mutate(index = 1:n()) # create a separate dataset with influence measures + id variable
influence_data <- cbind(influence_data, dfbetas(m1)) # create DFBETA measure variables
head(influence_data)
colnames(influence_data)[14:19] <- c("dfbeta_Intercept","dfbeta_dem","dfbeta_fp", "dfbeta_loggdppc", "dfbeta_stab", "dfbeta_britcol") # rename DFBETA variables

# cut-off value: absolute value of DFBETA > 2/sqrt(N)
# detect DFBETAS for intercept and predictors and reestimate models without potentially influential observations
inf_dem <- influence_data %>% filter(abs(dfbeta_dem) > 2/sqrt(dim(data)[1]))
m1_upd_dem <- update(m1, subset = c(-inf_dem$index))
mtable(m1, m1_upd_dem)
inf_fp <- influence_data %>% filter(abs(dfbeta_fp) > 2/sqrt(dim(data)[1]))
m1_upd_fp <- update(m1, subset = c(-inf_fp$index))
mtable(m1, m1_upd_fp)

# Cook's distance measure
# cut-off value: > 4/(N-k-1)
ols_plot_cooksd_bar(m1)
influential <- influence_data %>% filter(.cooksd > 4/(dim(data)[1] - m1$rank))
influential$index
m1_upd <- update(m1, subset = c(-influential$index))
mtable(m1, m1_upd)

top_cook <- influence_data %>% top_n(3, .cooksd)
m1_upd2 <- update(m1, subset = c(-top_cook$index))
mtable(m1, m1_upd2)