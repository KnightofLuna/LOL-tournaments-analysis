library(HH)
library(glm2)
library(lme4)
library(lmtest)

DataForWR = read.csv('./data/LOL-LCK-2017.csv')

# Split the data into training and testing sets
DataForWR$game = as.factor(DataForWR$game)
DataForWR$fb = as.factor(DataForWR$fb)
DataForWR$fd = as.factor(DataForWR$fd)
DataForWR$ft = as.factor(DataForWR$ft)
str(DataForWR)

xyplot(result~goldat15|team, 
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.lmline(x, y)
       },
       data = DataForWR)

# random intercepts
glm1.1 = glmer(result~goldat15+(1|team), family = binomial, data = DataForWR)
summary(glm1.1)

# random slopes 
glm1.2 = glmer(result~goldat15 +(0+goldat15|team), family = binomial, data = DataForWR)
summary(glm1.2)

# random intercepts and slopes
glm1.3 = glmer(result~goldat15 +(goldat15|team), family = binomial, data = DataForWR)
summary(glm1.3)

# independent random intercepts and slopes
glm1.4 = glmer(result~goldat15 +(goldat15||team), family = binomial, data = DataForWR)
# boundary (singular) fit: see ?isSingular

lrtest(glm1.1, glm1.3)
"""
Likelihood ratio test

Model 1: result ~ goldat15 + (1 | team)
Model 2: result ~ goldat15 + (goldat15 | team)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   3 -482.42                     
2   5 -482.27  2 0.2955     0.8627
"""
coef(glm1.1)
