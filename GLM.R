library(glm2)
library(glmnet)
library(glmulti)

DataForWR = read.csv('./data/LOL-LCK-2017.csv')

# Data description
str(DataForWR)
summary(DataForWR)

# pairs plot
# Customize upper panel
lowerPanel = function(x, y){
  points(x,y, pch = 20, cex = 0.1, col = c("red", "blue")[as.factor(DataForWR$result)])
}

# Customize diagonal panel
panelHist = function(x) { 
  usr = par("usr"); on.exit(par(usr)) 
  par(usr = c(usr[1:2], 0, 1.5)) 
  h = hist(x, plot = FALSE) 
  breaks = h$breaks; nB = length(breaks) 
  y = h$counts; y = y/max(y) 
  rect(breaks[-nB], 0, breaks[-1], y, col = '#FF9999') 
} 

# Create the plots
pairs(DataForWR[,-14],
      diag.panel = panelHist,
      lower.panel = lowerPanel,
      upper.panel = NULL
      )

par(xpd=TRUE)
legend('topright', col = c("red", "blue"), pch = 20, legend = c("lose", "win"))

# Split the data into training and testing sets
unique(DataForWR$team)
DataForWR$team = relevel(DataForWR$team, ref = 'Kongdoo Monster')
DataForWR$game = as.factor(DataForWR$game)
DataForWR$game = relevel(DataForWR$game, ref = '3')
DataForWR$fb = as.factor(DataForWR$fb)
DataForWR$fd = as.factor(DataForWR$fd)
DataForWR$ft = as.factor(DataForWR$ft)
str(DataForWR)

# logistic regression model
lr.full = glm2(result~., family = binomial(link = 'logit'), data = DataForWR)
summary(lr.full)

# refine the model

# find the best model in another way
fitall = glmulti(result~., family = binomial(link = 'logit'), data = DataForWR, level = 1, method = 'h', crit = 'aicc')
weightable(fitall) 
lr.reduce = glm2(result ~ side + team + fb + fd + ft + fttime + cspm + goldat15, family = binomial, data = DataForWR)
summary(lr.reduce)

# intercation term
lr.inter = glm2(result ~ side + team + fb + fd + ft + fttime + cspm + goldat15 + side:team, family = binomial, data = DataForWR)
summary(lr.inter)

