library(hydroGOF)
library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(graphics)
library(caret)
library (randomForest)
library (tree)
library (ISLR)

#immporting data
train<- read.csv("C:/Users/Maliha/Downloads/R Project/Data Set/Group Project on R-Data Set-4.csv",
                 header = TRUE)
test<- read.csv("C:/Users/Maliha/Downloads/R Project/Data Set/Group Project on R-Data Set-3.csv",
                header = TRUE)
any(is.na(train))
sum(is.na(train))
na.cols = which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[na.cols], is.na)), decreasing = TRUE)
summary(train)
#cause these columns have more than 40% missing
train$Alley <- NULL
train$Fence <- NULL
train$PoolQC <- NULL
train$FireplaceQu <- NULL
train$MiscFeature <- NULL
sapply(train,function(x) sum(is.na(x)))
train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage, na.rm = TRUE)
train$MasVnrArea[is.na(train$MasVnrArea)] = mean(train$MasVnrArea, na.rm = TRUE)
train$GarageYrBlt[is.na(train$GarageYrBlt)] = mean(train$GarageYrBlt, na.rm = TRUE)
Mode2 <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var2 in 1:ncol(train)) {
  if (class(train[,var2])=="numeric") {
    train[is.na(train[,var2]),var2] <- mean(train[,var2], na.rm = TRUE)
  } else if (class(train[,var2]) %in% c("character", "factor")) {
    train[is.na(train[,var2]),var2] <- Mode2(train[,var2], na.rm = TRUE)
  }
}

##TEST##
any(is.na(test))
sum(is.na(test))
na.cols1 = which(colSums(is.na(test)) > 0)
sort(colSums(sapply(test[na.cols], is.na)), decreasing = TRUE)
summary(test)
#cause these columns have more than 40% missing
test$Alley <- NULL
test$Fence <- NULL
test$PoolQC <- NULL
test$FireplaceQu <- NULL
test$MiscFeature <- NULL
sapply(test,function(x) sum(is.na(x)))
test$LotFrontage[is.na(test$LotFrontage)] = mean(test$LotFrontage, na.rm = TRUE)
test$MasVnrArea[is.na(test$MasVnrArea)] = mean(test$MasVnrArea, na.rm = TRUE)
test$GarageYrBlt[is.na(test$GarageYrBlt)] = mean(test$GarageYrBlt, na.rm = TRUE)
Mode3 <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var3 in 1:ncol(test)) {
  if (class(test[,var3])=="numeric") {
    test[is.na(test[,var3]),var3] <- mean(test[,var3], na.rm = TRUE)
  } else if (class(test[,var3]) %in% c("character", "factor")) {
    test[is.na(test[,var3]),var3] <- Mode3(test[,var3], na.rm = TRUE)
  }
}
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = Mode2(test$BsmtFinSF1, na.rm = TRUE)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = Mode2(test$BsmtFinSF2, na.rm = TRUE)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = Mode2(test$BsmtUnfSF, na.rm = TRUE)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = Mode2(test$TotalBsmtSF, na.rm = TRUE)
test$BsmtFullBath[is.na(test$BsmtFullBath)] = Mode2(test$BsmtFullBath, na.rm = TRUE)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = Mode2(test$BsmtHalfBath, na.rm = TRUE)
test$GarageCars[is.na(test$GarageCars)] = Mode2(test$GarageCars, na.rm = TRUE)
test$GarageArea[is.na(test$GarageArea)] = Mode2(test$GarageArea, na.rm = TRUE)

plotCategorical = function(cols, dataframe) {
  for (col in cols) {
    # Remove NA's & sort categories by tally
    order.cols = names(sort(table(df[,col]), decreasing = TRUE))
    # qplot is ggplot's equivalent of base R's high-level plotting function `plot`
    num.plot = qplot(dataframe[,col]) +
      # change bar color 
      geom_bar(fill = 'cornflowerblue') +
      # add the value labels to each bar
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      # minimal theme
      theme_minimal() +
      # set scales for each plot to go from 0 to max of categorical feature
      scale_y_continuous(limits = c(0,max(table(dataframe[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      # rotate x-axis label text 30 degrees and set font size to 12
      theme(axis.text.x = element_text(angle = 30, size=12))
    # Show plot and suppress warning messages from plot function
    suppressWarnings(print(num.plot))
  }
}
init <- dim(train)[1]
# histogram of sale price
ggplot(train, aes(x = SalePrice)) + 
  geom_histogram(fill = "#81AC9B", col = "black", bins = 55) + 
  scale_x_continuous(name = "Sale Price", labels = dollar, limits = c(0, 800000)) + 
  ggsave(filename = "HistSaleprice.png", width = 5, height = 5)

# remove outliers
outlierMinMax <- quantile(train$SalePrice, c(.01, .99))
train <- train[ train$SalePrice > outlierMinMax[1] & train$SalePrice < outlierMinMax[2],  ]
# end number of obs
end <- dim(train)[1]
any(is.na(df))



nums <- sapply(train, is.numeric)
aa<-train[ , nums]
as.data.frame(aa)
aa=colnames(aa)
aa
as.vector(aa)
remove <- c ("X","Id")
aa %in% remove
aa=aa [! aa %in% remove]
length(aa)
for (i in 1:38) {
  a = (aa[i])
  print(a)
  # min(MyData[,a])
  m = min(train[,a])
  M = max(train[,a])
  R=M-m
  # print(M)
  train[,a]=(train[,a]-m)/R
}

##normalisation on test
nums <- sapply(test, is.numeric)
aa<-test[ , nums]
as.data.frame(aa)
aa=colnames(aa)
aa
as.vector(aa)
remove <- c ("X","Id")
aa %in% remove
aa=aa [! aa %in% remove]
length(aa)
for (i in 1:38) {
  a = (aa[i])
  print(a)
  min(test[,a])
  #m = min(test[,a])
  M = max(test[,a])
  R=M-m
  #print(M)
  test[,a]=(test[,a]-m)/R
}

test$GarageCars <- as.numeric(as.character(test$GarageCars))
test$GarageArea <- as.numeric(as.character(test$GarageArea))
test$BsmtFinSF1 <- as.numeric(as.character(test$BsmtFinSF1))
train$OverallQual <- as.numeric(as.character(train$OverallQual))
train$OverallCond <- as.numeric(as.character(train$OverallCond))
test$BsmtFinSF2 <- as.numeric(as.character(test$BsmtFinSF2))
test$BsmtFullBath <- as.numeric(as.character(test$BsmtFullBath))
test$BsmtHalfBath <- as.numeric(as.character(test$BsmtHalfBath))
test$TotalBsmtSF <- as.numeric(as.character(test$TotalBsmtSF))
test$BsmtUnfSF <- as.numeric(as.character(test$BsmtUnfSF))


#regression model
model=lm(SalePrice ~ .,data = train)
summary(model)
model1 <- step(lm(SalePrice ~ .,data = train),direction = "both")
summary(model1)
singular.ok=TRUE
vif(model1)
all.model <- lm(SalePrice ~ ., data = train)
# predict on test set
p2<-predict(model1,train)
rmse(train$SalePrice,p2)

#polinomial  
#svm
svm_model <- svm(SalePrice ~.,data=train)
summary(svm_model)
p3<-predict(svm_model,train)
rmse(train$SalePrice,p3)


tune.out=tune(svm,SalePrice ~ .,data=train ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary (tune.out)
## best model

bestmod=tune.out$best.model
summary (bestmod )
p4=predict (bestmod,train)
p5=predict (bestmod,data=test)
rmse(train$SalePrice,p4)
table(predict =p4 , truth= train$SalePrice )
p4
p5




##bagging and random forest 

library (randomForest)
set.seed (1)
bag.boston =randomForest(SalePrice~.,data=train ,
                         mtry=13, importance =TRUE)
bag.boston

yhat.bag = predict (bag.boston ,data=test)
rmse(train$SalePrice,yhat.bag)

library (tree)
library (ISLR)


# set seed for reproducibility
set.seed(256)
# create 5 folds to be used in cross validation
myFolds <- createFolds(train, k = 5)
# create a custom trainControl object to use our folds; index = myFolds
myControl = trainControl(verboseIter = FALSE, index = myFolds)


rforest <- randomForest(SalePrice ~ ., data = train , trcontrol=fitcontrol)
class(rforest)
summary(rforest)
p7=predict (rforest,train)
rmse(train$SalePrice,p7)
#knn
set.seed(579)
# Train glmnet with custom trainControl and tuning: model
knn1 <- train(
  # formula
  SalePrice ~ ., 
  # data
  train,
  # knn regression
  method = "kknn",
  # trainControl
  trControl = myControl
)
# Print model to console
print(knn1)
summary(knn1)
p8=predict(knn1,train)
rmse(train$SalePrice,p8)

rforest1 <- randomForest(SalePrice ~ ., data = train ,method=glmboost)
summary(rforest1)
p9=predict (rforest1,data=train)
rmse(train$SalePrice,p9)

#seeing the prediction value with the best rmse to test data
p7
