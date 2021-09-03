library(randomForest)
library(caret)
library(dplyr)

tox_DB <- read.csv('GU_toxicity_DB_original_z_1418.csv')

### Random Forest Library (Select 2) ###

rf_train_2 <- tox_DB %>%
  select(c(param14:target))

## Random Forest K-Fold ##
set.seed(50)

flds <- createFolds(rf_train_2$target, k=5, list=TRUE, returnTrain=FALSE)
FOLD_NUM <- 1
inTest <- flds[[FOLD_NUM]]

for(i in 1:1){
  inTest <- flds[[FOLD_NUM]]
  rf_train_2.test <- rf_train_2[inTest, ]
  rf_train_2.train <- rf_train_2[-inTest, ]
  
  rf_model <- randomForest(rf_train_2.train[, -6],
                                  as.factor(rf_train_2.train$target),
                                  ntree=300,
                                  importance=TRUE)
}

rf_imp = varImp(rf_model)

print(rf_model)
print(rf_imp)

##                      ##