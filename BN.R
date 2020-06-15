setwd('/Users/kariya/Documents/STAT6014/Ass2')

library(pROC)
library(bnlearn)

DataForWR = read.csv('LOL-LCK-2017.csv')

# Split the data into training and testing sets
DataForWR$game = as.factor(DataForWR$game)
DataForWR$fb = as.factor(DataForWR$fb)
DataForWR$fd = as.factor(DataForWR$fd)
DataForWR$ft = as.factor(DataForWR$ft)
DataForWR$result = as.factor(DataForWR$result)

n = nrow(DataForWR)
set.seed(6014)
testRows = sample(c(1:n), floor(n*0.2), replace = F)

# Hybrid Bayesian Newtwork
trainData = DataForWR[-testRows,]
testData = DataForWR[testRows,]

mmhcdag = mmhc(trainData, whitelist = NULL, blacklist = NULL)
plot(mmhcdag)
modelstring(mmhcdag)

mmmhcfit = bn.fit(mmhcdag, trainData, method = 'mle')
for (i in c(6,8,10:13)){
  bn.fit.qqplot(mmmhcfit[[i]])
}

pred = predict(mmmhcfit, data = testData, node = 'result', method = 'bayes-lw')
sum(pred == testData$result) / nrow(testData) # 0.4715909

# Bayesian network

roccurve.hc = roc(testData_d$result, pred.hc.prob) #  0.7244
plot.roc(roccurve.hc, print.thres = "best", col = 'red')
# plot.roc(roccurve.lr.reduce,  print.thres = "best", add = TRUE, col = 'blue')
# lfull = paste0("Full (",round(auc(roccurve.lr.full),4),")")
# lreduce = paste0("Reduced (",round(auc(roccurve.lr.reduce),4),")")
# legend("bottomright", legend=c(lfull, lreduce), col=c("red", "blue"), lwd=2)
#title('Comparison of ROC Curves of Logistic Regression Models', line = 3)


optimal_break = function(num_break, 
                         discretize_method = 'interval', 
                         test_row, 
                         plot_ROC = FALSE, output_model = FALSE){
  # Discretize the continuous variables by interval discretization
  d = discretize(DataForWR[,c(6,8,10:13)], discretize_method, num_break)
  
  # Split the disretized data into training set and tesing set with the same seed as before
  tempTrainData = cbind(DataForWR[-test_row,-c(6,8,10:13)], d[-test_row,])
  tempTestData = cbind(DataForWR[test_row,-c(6,8,10:13)], d[test_row,])
  tempTrainData$result = as.factor(tempTrainData$result)
  tempTestData$result = as.factor(tempTestData$result)
  
  # fit a Bayesian Network
  temphcdag = hc(tempTrainData)
  tempBIC = score(temphcdag, data = tempTrainData)
  tempLoglik = score(temphcdag, data = tempTrainData, type = 'loglik')
  temphcfit = bn.fit(temphcdag, tempTrainData, method = "mle")
  
  # predict
  temppred.label = predict(temphcfit, tempTestData, node = 'result', prob = TRUE)
  temppred.prob = attr(temppred.label, which = 'prob')[2,]
  tempacc = sum(temppred.label == tempTestData$result) / nrow(tempTestData)
  
  # ROC curve
  temproccurve = roc(tempTestData$result, temppred.prob)
  tempauc = auc(temproccurve)
  if (plot_ROC){
    plot.roc(temproccurve, print.auc = TRUE, print.thres = "best", print.thres.best.method = "closest.topleft")
    }
  if (output_model){
    return(list(DAG = temphcdag, fit = temphcfit))
  }
  else {
    return(list(BIC = tempBIC, loglik = tempLoglik, test_acc = tempacc, auc = tempauc))
  }
}

bicList = c(); logitList = c(); accList = c(); aucList =c()
for (b in c(2:10)){
  tempResult = optimal_break(num_break = b, discretize_method = 'interval', test_row = testRows)
  bicList = c(bicList, tempResult$BIC)
  logitList = c(logitList, tempResult$loglik)
  accList = c(accList, tempResult$test_acc)
  aucList = c(aucList, tempResult$auc)
}

par(mfrow = c(2,2))
plot(x = c(2:10), y = bicList, type = 'b', ylab = '', xlab = 'Break')
title('BIC')
plot(x = c(2:10), y = logitList, type = 'b', ylab = '', xlab = 'Break')
title('Loglik')
plot(x = c(2:10), y = accList, type = 'b', ylab = '', xlab = 'Break')
title('Test Accuracy')
plot(x = c(2:10), y = aucList, type = 'b', ylab = '', xlab = 'Break')
title('AUC')
par(mfrow = c(1,1))

i = 1 # break = 2
bicList[i]; logitList[i]; accList[i]; aucList[i]
'''
[1] -6957.698
[1] -6780.665
[1] 0.7386364
[1] 0.7710171
'''

# Recall the best BN
list1 = optimal_break(num_break = 2, test_row = testRows, plot_ROC = T, output_model = T)
plot(list1$DAG)
modelstring(list1$DAG)

for(var in colnames(trainData)){
  print(list1$fit[[var]])
  bn.fit.barchart(list1$fit[[var]])
}

# plot discretized variables
d = discretize(DataForWR[,c(6,8,10:14)], 'interval', 3)
par(mfrow=c(3,3))
for(col in colnames(d)){
  icol = which(colnames(DataForWR) == col)
  icold = which(colnames(d) == col)
  plot(DataForWR[,icol],col=d[,icold], xlab = '', ylab = '')
  legend("topright",legend = unique(d[,icold]), col = c(1:3), pch = 1, cex = 0.8)
  title(main = paste0(col))
}
par(mfrow=c(1,1))

cpquery(list1$fit, event = (result == '0'), evidence = (team == 'KT Rolster' & ft == '1' & fd == '1'), n = 10000)
