# Clearing environment
rm(list = ls())

#Loading libraries
library(dplyr)
library(fastDummies)
library(caret)
library(gains)
library(FNN)
library(rpart)
library(corrplot)
library(rpart.plot)
library(randomForest)

#### DATA PREPROCESSING ####
# Loading the data
shoppers.df <- read.csv("online_shoppers_intention.csv")
dim(shoppers.df)
summary(shoppers.df)
sapply(shoppers.df, class)
sum(is.na(shoppers.df))

##### Variable analysis and removal of outliers #####
#Administrative
boxplot(shoppers.df$Administrative , main = "Administrative original")
#Administrative Duration
boxplot(shoppers.df$Administrative_Duration , main = "Administrative_Duration original")
#Informational
boxplot(shoppers.df$Informational , main = "Informational original")
#Informational_Duration
boxplot(shoppers.df$Informational_Duration , main = "Informational_Duration original")
#ProductRelated
boxplot(shoppers.df$ProductRelated , main = "ProductRelated original")
#ProductRelated_Duration
boxplot(shoppers.df$ProductRelated_Duration , main = "ProductRelated_Duration original")
#PageValues
boxplot(shoppers.df$PageValues , main = "PageValues original")


#Removing Outliers
#Administrative
outliers = boxplot(shoppers.df$Administrative, plot=FALSE)$out
shoppers.df1 <- shoppers.df[!(shoppers.df$Administrative > 12),]
boxplot(shoppers.df1$Administrative , main = "Administrative without outliers")
#Administrative Duration
outliers1 = boxplot(shoppers.df1$Administrative_Duration, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$Administrative_Duration > 600),]
boxplot(shoppers.df1$Administrative_Duration , main = "Administrative_Duration without Outliers" )
#Informational
outliers2 = boxplot(shoppers.df1$Informational, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$Informational > 4),]
boxplot( shoppers.df1$Informational , main = "Informational without outliers" )
#Informational_Duration
outliers3 = boxplot(shoppers.df1$Informational_Duration, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$Informational_Duration > 300),]
boxplot(shoppers.df1$Informational_Duration , main = "Informational_Duration without outliers" )
#ProductRelated
outliers4 = boxplot(shoppers.df1$ProductRelated, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$ProductRelated > 200),]
boxplot(shoppers.df1$ProductRelated , main = "ProductRelated without ouliers" )
#ProductRelated_Duration
outliers5 = boxplot(shoppers.df1$ProductRelated_Duration, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$ProductRelated_Duration > 6000),]
boxplot(shoppers.df1$ProductRelated_Duration , main = "ProductRelated_Duration without oultiers")
#PageValues
outliers6 = boxplot(shoppers.df1$PageValues, plot=FALSE)$out
shoppers.df1 <- shoppers.df1[!(shoppers.df1$PageValues > 150),]
boxplot(shoppers.df1$PageValues , main = "PageValues without outliers")


##### Converting categorical variables to factors #####
shoppers.df1$SpecialDay <- as.factor(shoppers.df1$SpecialDay)
shoppers.df1$Month <- as.factor(shoppers.df1$Month)
shoppers.df1$OperatingSystems <- as.factor(shoppers.df1$OperatingSystems)
shoppers.df1$Browser <- as.factor(shoppers.df1$Browser)
shoppers.df1$Region <- as.factor(shoppers.df1$Region)
shoppers.df1$TrafficType <- as.factor(shoppers.df1$TrafficType)
shoppers.df1$VisitorType <- as.factor(shoppers.df1$VisitorType)
shoppers.df1$Weekend <- as.factor(shoppers.df1$Weekend)
shoppers.df1$Revenue <- as.factor(shoppers.df1$Revenue)

##### Reduce levels in categorical variables #####
#Operating systems
# Levels and frequency
OSLevels <- data.frame(table(shoppers.df1$OperatingSystems))
OSLevels[order(OSLevels$Freq, decreasing = TRUE),]

# list for least 100 records
KeepOS <- OSLevels[OSLevels$Freq > 100, ]
KeepOS[order(KeepOS$Freq, decreasing = TRUE),]
KeepOSVar <- as.factor(KeepOS$Var1)

# Combines other levels into "Other" 
shoppers.df1$OperatingSystems <- as.factor(ifelse(shoppers.df1$OperatingSystems %in% KeepOSVar, as.character(shoppers.df1$OperatingSystems), "Other"))
OSLevels1 <- data.frame(table(shoppers.df1$OperatingSystems))
OSLevels1[order(OSLevels1$Freq, decreasing = TRUE),]

#Browser
# Levels and frequency
browserlevels <- data.frame(table(shoppers.df1$Browser))
browserlevels[order(browserlevels$Freq, decreasing = TRUE),]

# list for least 100 records
Keepbrowser <- browserlevels[browserlevels$Freq > 100, ]
Keepbrowser[order(Keepbrowser$Freq, decreasing = TRUE),]
KeepbrowserVar <- as.factor(Keepbrowser$Var1)

# Combines other levels into  "Other" 
shoppers.df1$Browser <- as.factor(ifelse(shoppers.df1$Browser %in% KeepbrowserVar, as.character(shoppers.df1$Browser), "Other"))
browserlevels1 <- data.frame(table(shoppers.df1$Browser))
browserlevels1[order(browserlevels1$Freq, decreasing = TRUE),]


#TrafficType
# Levels and frequency
Trafficlevels <- data.frame(table(shoppers.df1$TrafficType))
Trafficlevels[order(Trafficlevels$Freq, decreasing = TRUE),]

# list for least 100 records
KeepTraffic <- Trafficlevels[Trafficlevels$Freq > 100, ]
KeepTraffic[order(KeepTraffic$Freq, decreasing = TRUE),]
KeepTrafficVar <- as.factor(KeepTraffic$Var1)

# Combines other levels into  "Other" 
shoppers.df1$TrafficType <- as.factor(ifelse(shoppers.df1$TrafficType %in% KeepTrafficVar, as.character(shoppers.df1$TrafficType), "Other"))
Trafficlevels1 <- data.frame(table(shoppers.df1$TrafficType))
Trafficlevels1[order(Trafficlevels1$Freq, decreasing = TRUE),]


##### Creating Dummy Variables using Fast Dummies #####
columns <- colnames(shoppers.df1)
for (i in columns) {
  if (class(shoppers.df1[[i]]) == "factor") {
    shoppers.df1 <- dummy_cols( shoppers.df1,
                               select_columns = i,
                               remove_first_dummy = TRUE,
                               remove_selected_columns = TRUE )
  }
}
summary(shoppers.df1)

##### Renamed Weekend_TRUE and Revenue_TRUE #####
shoppers.df1 = rename(shoppers.df1, Weekend = Weekend_TRUE, Revenue = Revenue_TRUE)

#### Correlation ####

##### Correlation between revenue and others ####
columns1 <- colnames(shoppers.df1)
MaxCorrelation = 0
MaxAbsCorrelation = 0
MaxVariable = ""
for (i in columns1) {
  RevenueCorrelation <- cor( shoppers.df1$Revenue , shoppers.df1[[i]])
  AbsCorrelation <- abs(RevenueCorrelation)
  if (AbsCorrelation > MaxAbsCorrelation && AbsCorrelation < 1) {
    MaxCorrelation <- RevenueCorrelation
    MaxAbsCorrelation <- AbsCorrelation
    MaxVariable <- i
  }
  print(paste(i,RevenueCorrelation))
}
print(paste(MaxVariable,MaxCorrelation))
cm <- cor(shoppers.df1)
corrplot(cm, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
heatmap(cm, col = colorRampPalette(c("orange", "white", "firebrick"))(50), main = "Correlation Heatmap", xlab = "Other Columns", ylab = "Target Column")
#### Partitioning Data into 70:30####
set.seed(1)
train.rows <- sample(rownames(shoppers.df1), nrow(shoppers.df1)*0.7)
valid.rows <- setdiff(rownames(shoppers.df1), train.rows)
shoppers_train <- shoppers.df1[train.rows , ]
shoppers_valid <- shoppers.df1[valid.rows , ]


#### Logistic Regression ####

##### For All Varibles #####
shoppers.lm.all <- glm(Revenue ~ . ,data = shoppers_train,family = "binomial")
summary(shoppers.lm.all)
# predictions for full model
shoppers.lm.all.pred <- predict(shoppers.lm.all, shoppers_valid, type = "response")
#gain <- gains(shoppers_valid$Revenue, shoppers.lm.all.pred, groups = nrow(shoppers_valid))
confusionMatrix(factor(ifelse(shoppers.lm.all.pred >= 0.5, 1, 0)), factor(shoppers_valid$Revenue), positive = "1")


##### For most significant variable i.e page value #####
shoppers.lm.PV <- glm(Revenue ~ PageValues, data = shoppers_train, family = "binomial")           
summary(shoppers.lm.PV)
# predictions with page value
shoppers.lm.PV.pred <- predict(shoppers.lm.PV, shoppers_valid, type = "response")
confusionMatrix(factor(ifelse(shoppers.lm.PV.pred >= 0.5, 1, 0)), factor(shoppers_valid$Revenue), positive = "1")

##### For significant variables i.e:ProductRelated_Duration,ExitRates,PageValues,Month_Dec,Month_Feb,Month_Mar,Month_May####
shoppers.lm.SV <- glm( Revenue ~ ProductRelated_Duration + ExitRates + PageValues +
                       Month_Dec + Month_Feb + Month_Mar + Month_May, data = shoppers_train, family = "binomial")
summary(shoppers.lm.SV)
# predictions with significant variables
shoppers.lm.SV.pred <- predict(shoppers.lm.SV, shoppers_valid, type = "response")
confusionMatrix(factor(ifelse(shoppers.lm.SV.pred >= 0.5, 1, 0)), factor(shoppers_valid$Revenue), positive = "1")


#### KNN Regression ####

##### Scaling #####
shoppers_train_norm <- shoppers_train
shoppers_valid_norm <- shoppers_valid

col1 <- colnames(shoppers_train[, -58])
for (i in col1) {
  shoppers_valid_norm[[i]] <- (shoppers_valid_norm[[i]] - mean(shoppers_train[[i]])) / sd(shoppers_train[[i]])
  shoppers_train_norm[[i]] <- (shoppers_train_norm[[i]] - mean(shoppers_train[[i]])) / sd(shoppers_train[[i]])
}

##### KNN with K=1 #####
shoppers.knn1 <- knn(train = shoppers_train_norm[, -58], test  = shoppers_valid_norm[, -58], cl = shoppers_train_norm$Revenue,k = 1)
confusionMatrix(shoppers.knn1, as.factor(shoppers_valid_norm$Revenue), positive = "1")


##### Finding optimal k #####
accuracy.df <- data.frame(k = seq(1, 20, 1),accuracy = rep(0, 20))
for (i in 1:20) {
  knn.all <- knn(shoppers_train_norm[, -58], shoppers_valid_norm[, -58], cl = shoppers_train_norm$Revenue, k = i)                        
  accuracy.df[i, 2] <- confusionMatrix(knn.all, as.factor(shoppers_valid_norm$Revenue))$overall[1]
}
accuracy.df

##### Using the best k = 11 #####

shoppers.knn.11 <- knn(train = shoppers_train_norm[, -58], test  = shoppers_valid_norm[, -58],cl = shoppers_train_norm$Revenue,k = 11)
confusionMatrix(shoppers.knn.11, as.factor(shoppers_valid_norm$Revenue), positive = "1")


#### Classification Tree ####

##### Classification tree default #####
shoppers.tree <- rpart(Revenue ~ ., data = shoppers_train, method = "class")
prp(shoppers.tree)
shoppers.tree.pred <- predict(shoppers.tree, shoppers_valid, type = "class")
confusionMatrix(shoppers.tree.pred, as.factor(shoppers_valid$Revenue), positive = "1")


##### Classification tree full unpruned #####
shoppers.tree.full <- rpart(Revenue ~ ., data = shoppers_train, method = "class",cp = 0, minsplit = 2)
prp(shoppers.tree.full)
shoppers.tree.full.pred <- predict(shoppers.tree.full, shoppers_valid, type = "class")
confusionMatrix(shoppers.tree.full.pred, as.factor(shoppers_valid$Revenue), positive = "1")
shoppers.tree.full$cptable
##### Classification tree pruned #####
prune.shoppers.tree.full <- prune(shoppers.tree.full, cp=shoppers.tree.full$cptable[which.min(shoppers.tree.full$cptable[,'xerror']), 'CP'])
prp(prune.shoppers.tree.full)
prune.shoppers.tree.full.pred <- predict(prune.shoppers.tree.full, shoppers_valid, type = "class")
confusionMatrix(prune.shoppers.tree.full.pred, as.factor(shoppers_valid$Revenue), positive = "1")


#### Random Forest ####

##### Finding best hyperparameters for Random forest#####
# Define a grid of hyperparameters to search
param_grid <- expand.grid(
  ntree = c(100, 200, 500),
  mtry = c(2, 4, 6),
  nodesize = c(5, 10, 20)
)

# Perform grid search using cross-validation
results <- apply(param_grid, 1, function(params) {
  set.seed(1231)  # Set seed for reproducibility
  model <- randomForest(
    as.factor(Revenue) ~ .,
    data = shoppers_train,
    ntree = params[["ntree"]],
    mtry = params[["mtry"]],
    nodesize = params[["nodesize"]],
    importance = FALSE  # Disable variable importance for simplicity
  )
  
  # Return the mean cross-validated error
  mean(model$err.rate[, "OOB"])
})

# Identify the best hyperparameters
best_params <- param_grid[which.min(results), ]
print(best_params)

##### creating random forest with best fit ntree=500, mtry= 6, nodesize = 20
shoppers.rf <- randomForest(as.factor(Revenue) ~ ., 
                            data = shoppers_train, 
                            ntree = 500,
                            mtry = 6, 
                            nodesize = 20, 
                            importance = TRUE)

##### predictions #####
shoppers.rf.pred <- predict(shoppers.rf, shoppers_valid)
confusionMatrix(shoppers.rf.pred, as.factor(shoppers_valid$Revenue), positive = "1")


#### OVER_SAMPLING ####
##### Oversampling with a ratio of 65:35####
balanced.df.over <- ovun.sample(Revenue ~ .,
                           data = shoppers.df1,
                           seed = 124,
                           method = "over",
                           N = 15000)$data


balanced.df.under <- ovun.sample(Revenue ~ .,
                                data = balanced.df.over,
                                seed = 124,
                                method = "under",
                                N = 10000)$data

table(balanced.df.over$Revenue)

set.seed(2123)
train.rows2 <- sample(rownames(balanced.df.over), nrow(balanced.df.over)*0.7)
valid.rows2 <- setdiff(rownames(balanced.df.over), train.rows2)
shoppers_train2 <- balanced.df.over[train.rows2 , ]
shoppers_valid2 <- balanced.df.over[valid.rows2 , ]



#### Logistic Regression after OverSampling ####

##### For All Varibles OS #####
shoppers.lm.all.OS <- glm(Revenue ~ . ,data = shoppers_train2,family = "binomial")
summary(shoppers.lm.all.OS)
# predictions for full model 
shoppers.lm.all.OS.pred <- predict(shoppers.lm.all.OS, shoppers_valid2, type = "response")
confusionMatrix(factor(ifelse(shoppers.lm.all.OS.pred >= 0.5, 1, 0)), factor(shoppers_valid2$Revenue), positive = "1")


##### For most significant variable i.e page value #####
shoppers.lm.OS.PV <- glm(Revenue ~ PageValues, data = shoppers_train2, family = "binomial")           
summary(shoppers.lm.OS.PV)
# predictions with page value
shoppers.lm.PV.OS.pred <- predict(shoppers.lm.OS.PV, shoppers_valid2, type = "response")
confusionMatrix(factor(ifelse(shoppers.lm.PV.OS.pred >= 0.5, 1, 0)), factor(shoppers_valid2$Revenue), positive = "1")

##### For significant variables i.e:ProductRelated_Duration,ExitRates,PageValues,Month_Dec,Month_Feb,Month_Mar,Month_May####
shoppers.lm.OS.SV <- glm( Revenue ~ ProductRelated_Duration + ExitRates + PageValues +
                         Month_Jul + Month_Feb + Month_Mar + Month_May + Informational_Duration + Month_Nov,
                         data = shoppers_train2, family = "binomial")
summary(shoppers.lm.OS.SV)
# predictions with significant variables
shoppers.lm.SV.OS.pred <- predict(shoppers.lm.OS.SV, shoppers_valid2, type = "response")
confusionMatrix(factor(ifelse(shoppers.lm.SV.OS.pred >= 0.5, 1, 0)), factor(shoppers_valid2$Revenue), positive = "1")



#### KNN Regression after OverSampling####

##### Scaling #####
shoppers_train_norm2 <- shoppers_train2
shoppers_valid_norm2 <- shoppers_valid2

col3 <- colnames(shoppers_train2[, -58])
for (i in col3) {
  shoppers_valid_norm2[[i]] <- (shoppers_valid_norm2[[i]] - mean(shoppers_train2[[i]])) / sd(shoppers_train2[[i]])
  shoppers_train_norm2[[i]] <- (shoppers_train_norm2[[i]] - mean(shoppers_train2[[i]])) / sd(shoppers_train2[[i]])
}
##### Finding optimal k #####
accuracy.df2 <- data.frame(k = seq(1, 20, 1),accuracy = rep(0, 20))
for (i in 1:20) {
  knn.OS.all <- knn(shoppers_train_norm2[, -58], shoppers_valid_norm2[, -58], cl = shoppers_train_norm2$Revenue, k = i)                        
  accuracy.df2[i, 2] <- confusionMatrix(knn.OS.all, as.factor(shoppers_valid_norm2$Revenue))$overall[1]
}
accuracy.df2

##### KNN with K=1 #####
shoppers.OS.knn1 <- knn(train = shoppers_train_norm2[, -58], test  = shoppers_valid_norm2[, -58], cl = shoppers_train_norm2$Revenue,k = 1)
confusionMatrix(shoppers.OS.knn1, as.factor(shoppers_valid_norm2$Revenue), positive = "1")



#### Classification Tree with OverSampling####

##### Classification tree default #####
shoppers.OS.tree <- rpart(Revenue ~ ., data = shoppers_train2, method = "class")
prp(shoppers.OS.tree)
shoppers.tree.OS.pred <- predict(shoppers.OS.tree, shoppers_valid2, type = "class")
confusionMatrix(shoppers.tree.OS.pred, as.factor(shoppers_valid2$Revenue), positive = "1")

##### Classification tree full unpruned #####
shoppers.OS.tree.full <- rpart(Revenue ~ ., data = shoppers_train2, method = "class",cp = 0, minsplit = 2,xval = 10)
prp(shoppers.OS.tree.full)
shoppers.OS.tree.full.pred <- predict(shoppers.OS.tree.full, shoppers_valid2, type = "class")
confusionMatrix(shoppers.OS.tree.full.pred, as.factor(shoppers_valid2$Revenue), positive = "1")
shoppers.OS.tree.full$cptable
##### Classification tree pruned #####
prune.shoppers.OS.tree.full <- prune(shoppers.OS.tree.full, cp=shoppers.OS.tree.full$cptable[which.min(shoppers.OS.tree.full$cptable[,'xerror']), 'CP'])
prp(prune.shoppers.OS.tree.full)
prune.shoppers.OS.tree.full.pred <- predict(prune.shoppers.OS.tree.full, shoppers_valid2, type = "class")
confusionMatrix(prune.shoppers.OS.tree.full.pred, as.factor(shoppers_valid2$Revenue), positive = "1")
table(balanced.df.over$Revenue)


#### Random Forest with over sampling ####

##### Finding best hyperparameters for Random forest#####
# Define a grid of hyperparameters to search
param_grid <- expand.grid(
  ntree = c(100, 200, 500),
  mtry = c(2, 4, 6),
  nodesize = c(5, 10, 20)
)

# Perform grid search using cross-validation
results <- apply(param_grid, 1, function(params) {
  set.seed(1231)  # Set seed for reproducibility
  model <- randomForest(
    as.factor(Revenue) ~ .,
    data = shoppers_train2,
    ntree = params[["ntree"]],
    mtry = params[["mtry"]],
    nodesize = params[["nodesize"]],
    importance = FALSE  # Disable variable importance for simplicity
  )
  
  # Return the mean cross-validated error
  mean(model$err.rate[, "OOB"])
})

# Identify the best hyperparameters
best_params <- param_grid[which.min(results), ]
print(best_params)

##### creating random forest with best fit ntree=500, mtry= 6, nodesize = 20
shoppers.OS.rf <- randomForest(as.factor(Revenue) ~ ., 
                            data = shoppers_train2, 
                            ntree = 500,
                            mtry = 6, 
                            nodesize = 10, 
                            importance = TRUE)

##### predictions #####
shoppers.OS.rf.pred <- predict(shoppers.OS.rf, shoppers_valid2)
confusionMatrix(shoppers.OS.rf.pred, as.factor(shoppers_valid2$Revenue), positive = "1")
