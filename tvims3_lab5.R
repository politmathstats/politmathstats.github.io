### Regression analysis. Lab 5
### Multiple regression. Measures of Influence
### April 28, 2023

# Source: Kalenborn C., Lessman C., 2013 
# Available at: https://yadi.sk/i/nlEQUoWKiqY0UA

# STATEMENT
# The data is used for the training purposes only. For the further use, please cite the original paper. 
############################################################################################################

install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("margins")
install.packages("psych")
install.packages("memisc")

library(haven) 
library(dplyr)
library(ggplot2)
library(margins)
library(psych)
library(memisc)

lab5 <- read_dta(file.choose())
head(lab5)

# Description

# cpi - Corruption Perception Index. The scale ranges from 0 to 10, where 10 stands for the highest level of corruption
# dem - Vanhanen?s democratization index. The scale ranges from 0 to 100, where 100 stands for the highest level of democracy
# fp - freedom of press: Freedom House. The scale ranges from 0 to 100, where 100 stands for the highest level of freedom of press
# loggdppc - natural logarithm of GDP per capita: World Bank. 
# stab - political stability. The index is constructed on the basis of "Political Stability" and "Absence of Violence/Terrorism" indicators 
# from the Worldwide Governance Indicators. The scale ranges from -2.5 to 2.5, where 2.5 stands for the highest level of political stability  
# britcol - a dummy variable, 1 - former British colony, 0 - otherwise. 

lab5 <- na.omit(lab5) 

lab5$fp_groups <- ifelse(lab5$fp>70, 1, 0)
head(lab5, 10)
lab5$fp_groups_label <- dplyr::recode(lab5$fp_groups, "1" = "free", "0" = "not free")

ggplot(data = lab5, aes(x = dem, y = cpi, color = fp_groups_label)) +
  geom_point(size=2) + labs(x = "Level of democracy", 
                            y = "Level of corruption",
                            title = "The effect of corruption on democracy") + 
  geom_smooth(method=lm)+ scale_colour_manual(values = c("coral3", "darkorchid3"))

free <- subset(lab5, fp_groups == 1)
m1_1 <- lm(cpi ~ dem, data = free)
summary(m1_1)

not_free <- subset(lab5, fp_groups == 0)
m1_2 <- lm(cpi ~ dem, data = not_free)
summary(m1_2)

mtable(m1_1, m1_2)

m2 <- lm(cpi ~ dem*fp_groups, data = lab5)
summary(m2)
mtable(m1_1, m1_2, m2)

m3_1 <- lm(cpi ~ dem*fp, data = lab5)
summary(m3_1)

m3 <- lm(cpi ~ dem*fp + loggdppc + stab + britcol, data = lab5)
summary(m3)

## marginal effects
s <- margins(m3, at = list(fp = seq(min(lab5$fp), max(lab5$fp), by = 1)))

ggplot(s, aes(fp, dydx_dem)) + geom_line() +
  geom_line(aes(y = dydx_dem+1.96*sqrt(Var_dydx_dem)), linetype = 2) +
  geom_line(aes(y = dydx_dem-1.96*sqrt(Var_dydx_dem)), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Conditional effect") +
  xlab("Freedom of press (fp)") + ylab("Marginal effect of democracy")
