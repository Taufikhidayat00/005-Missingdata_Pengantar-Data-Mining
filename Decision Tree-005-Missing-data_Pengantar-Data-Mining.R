library(rpart)
library(rpart.plot)
library(party)
dataku <- read.delim("clipboard")
View(dataku)
dataku$city <- as.factor(dataku$city)
dataku$kelayakan <- as.factor(dataku$kelayakan)
summary(dataku)
str(dataku)
dat <- sample (2, nrow(dataku), replace = TRUE, prob = c(0.7, 0.3) )
trainData <- dataku[dat==1, ]
testData <- dataku[dat==2, ]
myFormula <- city ~ sqft_lot+ sqft_above+sqft_living+price+floors+city
data_ctree <- ctree (myFormula, data = trainData)
table(predict(data_ctree), trainData$sqft_basement)
print(data_ctree)
plot(data_ctree)
prediksi <- predict(data_ctree, trainData)
cm <- table(trainData[, 5], prediksi)
cm
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy
summary(prediksi)

