# Assignment 2 PLSC Research Methods
# Note that for graphs/colors/titles/etc I used a lot of Googling to make them pretty


install.packages('ggplot2')
install.packages('dplyr')
install.packages('texreg')

library(ggplot2)
library(dplyr)
library(texreg)

setwd('~/Desktop/PSRM')
getwd()

gss <- read.csv('gss.csv', header = T)
blw <- read.csv('brightlinewatch.csv', header = T)

#make a histogram
ggplot(df, aes(variable)) +
  geom_histogram()


#make a bar chart
ggplot(df, aes(variable)) +
  geom_bar()


# Question 1 Code components

#age historgram (works)
ggplot(gss, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = '#a17aeb') +
  theme_bw() +
  xlab ('Age') +
  ylab ('Count') + 
  ggtitle ('Ages of Respondents')

summary(gss$age)

#marital bar (works)
ggplot(gss, aes(x = marital)) +
  geom_bar(fill = '#a17aeb') +
  theme_bw() +
  xlab ('Marital Status') +
  ylab ('Count') + 
  ggtitle ('Respondent Marital Status')

# Question 2 Code components --> take that this is mostly not going to be coding based

summary(gss$tvhours[gss$party_nominal=='D'])
summary(gss$tvhours[gss$party_nominal=='R'])

# Partisan subsets
gss_rSub <- subset(gss, party_nominal == 'R')
gss_dSub <- subset(gss, party_nominal == 'D')


# seperating parties and then plotting on top of eachother (WORKS) but is completely unnecessary 
ggplot()+
  geom_histogram(data = gss_dSub, aes(x = tvhours, fill = 'Democrat'), binwidth = 1, alpha = 0.5)+
  geom_histogram(data = gss_rSub, aes(x = tvhours, fill = 'Republican'), binwidth = 1, alpha = 0.5) +
  theme_bw()+
  xlab ('TV Hours Watched') +
  ylab ('Count') + 
  ggtitle ('Partisan TV Watching') +
  scale_fill_manual( values = c('Democrat' = '#78a6f0', 'Republican' ='#eb413b' )) +
  labs(fill = 'Legend')

# Question 3 Code components

mean(gss$tvhours[gss$party_nominal == 'D']) - mean(gss$tvhours[gss$party_nominal == 'R'])

t.test(tvhours ~ party_nominal, data = gss)



# part 2
# Question 4 (regression question)

#scatterplot
ggplot(blw, aes(age, rating_USA)) +
  geom_point(color = '#2e613a') +
  theme_bw() +
  xlab ('Age') +
  ylab ('Rating of US Democracy') + 
  ggtitle ('Age V. Rating of US Democracy')

cor(blw$age, blw$rating_USA)

m <- lm(rating_USA ~ age, data = blw)
summary(m)


#what a 100 year old would think, not sure how to do this
mean(gss$tvhours[gss$party_nominal == 'D']) - mean(gss$tvhours[gss$party_nominal == 'R'])

#prediction (googled) to check my plugging 100 into the expression
predict(m, newdata = data.frame(age = 100))

#t test
t.test(tvhours ~ party_nominal, data = gss)

#part5
blw$Dem <- ifelse(blw$partisan == 'Democrat', 1,0)

m <- lm(rating_USA ~ Dem, data = blw)
summary(m)

#random sample
blw_sample <- blw[sample(nrow(blw), 1000), ]
m <- lm(rating_USA ~ Dem, data = blw_sample)
summary(m)


#6
blw_subset <- subset(blw, presvote20post == 'Joe Biden' | presvote20post == 'Donald Trump')
prop.table(table(blw_subset$biden_winner, blw_subset$presvote20post), 2)
chisq.test(blw_subset$biden_winner, blw_subset$presvote20post)
