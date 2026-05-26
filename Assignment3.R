#Assignment 3 Political Science Research Methods

install.packages('ggplot2')
install.packages('dplyr')
install.packages('texreg')

library(ggplot2)
library(dplyr)
library(texreg)

setwd('~/Desktop/PSRM')
getwd()

df <- read.csv('assignment3.csv', header = T)

#Q1

#scatterplot
m <- lm(life_exp ~ urbpop, data = df)
summary(m)

ggplot(df, aes(urbpop, life_exp)) + geom_point() + geom_smooth(method = 'lm') +
  xlab('% Urban Population') + ylab('Life Expectancy') +
  theme_bw()


#Q2
df$gdppc <- df$gdp/df$po
df_subset <- subset(df, gdppc > 5e4)

#scatterplot of subset -> need to try to fix the scale of the x-axis
ggplot(df_subset, aes(gdppc, life_exp)) +
  geom_point(color = '#15598c') +
  theme_bw() +
  xlab ('GDP Per Capita') +
  ylab ('Life Expentancy') + 
  ggtitle ('GDP Per Capita V. Life Expectancy in Countries GDP Per Capita > 50,000')

#weird numbers
m <- lm(life_exp ~ gdppc, data = df_subset)
summary(m)

sum(df$gdppc < 10000)

#same with full data -> really gotta figure out how to fix the x-axis that is so ugly the US and China(?) are just too annoying
ggplot(df, aes(gdppc, life_exp)) +
  geom_point(color = '#3b0226') +
  theme_bw() +
  xlab ('GDP Per Capita') +
  ylab ('Life Expentancy') + 
  ggtitle ('GDP Per Capita V. Life Expectancy')

#weird numbers
m <- lm(life_exp ~ gdppc, data = df)
summary(m)

#Q3
df$lngdppc <- log(df$gdppc)

ggplot(df, aes(lngdppc, life_exp)) +
  geom_point(color = '#183c3d') +
  theme_bw() +
  xlab ('(Ln) GDP Per Capita') +
  ylab ('Life Expentancy') + 
  ggtitle ('(Ln) GDP Per Capita V. Life Expectancy')

m <- lm(life_exp ~ lngdppc, data = df)
summary(m)

#Q4

m = lm(infmort ~ smoke, data = df)
summary(m)

ggplot(df, aes(smoke, infmort, color = continent)) +
  geom_point(size = 1.5) +
  xlab ('% Smokers (Over 15 yrs)') +
  ylab ('% Infant Mortality') +
  ggtitle('Smoking V. Infant Mortality')

m = lm(infmort ~ smoke + continent, data = df)
summary(m)

summary(df$infmort)


