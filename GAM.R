setwd('/Users/kariya/Documents/STAT6014/Ass2')

library(mgcv)

DataForWR = read.csv('./data/LOL-LCK-2017.csv')

# Split the data into training and testing sets
DataForWR$game = as.factor(DataForWR$game)
DataForWR$game = relevel(DataForWR$game, ref = '3')
DataForWR$team = relevel(DataForWR$team, ref = 'Kongdoo Monster')
DataForWR$fb = as.factor(DataForWR$fb)
DataForWR$fd = as.factor(DataForWR$fd)
DataForWR$ft = as.factor(DataForWR$ft)

gam0 = gam(result~side + team + fb + fd + ft + fttime + cspm + goldat15, family = binomial, data = DataForWR)

gam1 = gam(result~side + team + fb + fd + ft + s(fttime) + s(cspm) + s(goldat15), family = binomial, data = DataForWR)
summary(gam1)

plot(gam1, pages = 1, se = TRUE, shade = TRUE)

gam2 = gam(result~side + team + fb + fd + ft + s(cspm) + goldat15, family = binomial, data = DataForWR)
anova(gam1, gam2, test = "Chisq") # no significantly difference

summary(gam2)
AIC(gam2)
  