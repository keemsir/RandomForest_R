## install list
# install.packages('xlsx') # install error
# install.packages('randomForest')
#install.packages('caret')
#install.packages('tictoc')

## library list
# library(xlsx) # install error
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(tictoc)


## Path Setting
tox_DB <- read.csv('GU_toxicity_DB.csv')
rf_test <- read.csv('GU_toxicity_DB_test.csv')

### Random Forest Model (Select 1) ###
# column select
rf_train_1 <- tox_DB %>%
  select(c(param1:target))

set.seed(50) # Sets the random seed

repeatedCV <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_grid <- expand.grid(mtry = 5) # seq(from = 1, to = ncol(rf_train_1) - 1, by = 1)

tic()
rf_model_1 <- train(
                    x = rf_train_1[ ,-19],
                    y = rf_train_1$target,
                    method = 'rf',
                    trControl = repeatedCV,
                    importance = TRUE,
                    tuneGrid = rf_grid
                    )
toc()


### Random Forest Library (Select 2) ###

rf_train_2 <- tox_DB %>%
  select(c(param1:target))

## Random Forest K-Fold ##
set.seed(50)

flds <- createFolds(rf_train_2$target, k=5, list=TRUE, returnTrain=FALSE)

FOLD_NUM <- 2

inTest <- flds[[FOLD_NUM]]
#Train_x_Idx <- rf_train_2[-inTest, ]
#Train_y_Idx <- rf_train_2[inTest, ]

for(i in 1:1){
  inTest <- flds[[FOLD_NUM]]
  rf_train_2.test <- rf_train_2[inTest, ]
  rf_train_2.train <- rf_train_2[-inTest, ]
  
  rf_model_2_flds <- randomForest(rf_train_2.train[, -19],
                                  as.factor(rf_train_2.train$target),
                                  ntree=300,
                                  importance=TRUE)
}


for(i in 1:1){
  inTest <- flds[[FOLD_NUM]]
  rf_train_2.test <- rf_train_2[inTest, ]
  rf_train_2.train <- rf_train_2[-inTest, ]
  
  rf_model_2_flds <- randomForest(rf_train_2.test[, -19],
                                  as.factor(rf_train_2.test$target),
                                  ntree=300,
                                  importance=TRUE)
}


##                      ##



tic()

rf_model_2 <- randomForest(rf_train_2[,-19],
                           as.factor(rf_train_2$target), #as.factor = classification
                           ntree=300,
                           importance=TRUE
                           )

toc()









### Random Forest important view ###
rf_imp_1 = varImp(rf_model_1)
rf_imp_2 = varImp(rf_model_2)

## ggplot2 (Select 1)
plot_ <- ggplot(rf_imp_1, 
                aes(x = reorder(rf_imp_1, 
                                Importance) , y = Importance)) + 
  geom_bar(stat = 'identity', 
           fill = '#800080') + 
  coord_flip() +
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Importance")+
  ggtitle("Important Features in Random Forest\n") +
  theme(plot.title = element_text(size=18))
  ggsave("important_features.png",
         plot_)
  plot_
  
  

## ggplot2 (Select 2)
important <- importance(rf_model_2, type=1)
Important_Features <- data.frame(Feature = row.names(important), Importance = important[, 1])

plot_ <- ggplot(Important_Features, 
                aes(x = reorder(Feature, 
                                Importance) , y = Importance)) + 
  geom_bar(stat = 'identity', 
           fill = '#800080') + 
  coord_flip() +
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Importance")+
  ggtitle("Important Features in Random Forest\n") +
  theme(plot.title = element_text(size=18))
ggsave("important_features.png",
       plot_)
plot_


## Predict (Select 1)

rf_pred <- predict(rf_model_1, rf_test)

test_predictions <- cbind(rf_test, rf_pred)

test_predictions <- test_predictions %>%
  select(Hosp.No, rf_pred) %>%
  rename(Prediction = rf_pred)

write.csv(test_predictions,
          file = "RF_GU_tox_predict.csv",
          row.names = FALSE,
          quote = FALSE)
