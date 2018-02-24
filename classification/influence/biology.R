library(caret)
library(ggplot2)
library(pROC)


fileToSave = "biology-auc"

data = read.csv("biology.csv", header = TRUE, sep = ",")

data$modularity_class = gsub(',','',as.character(data$modularity_class))
data$citations = gsub(',','',as.character(data$citations))
data$avg_part = gsub(',','',as.character(data$avg_part))

data$modularity_class = ifelse(is.na(data$modularity_class), -1, as.numeric(data$modularity_class)) 
data$citations = ifelse(is.na(data$citations), 0, as.numeric(data$citations)) 
data$avg_part =  ifelse(is.na(data$avg_part), 0, as.numeric(data$avg_part)) 
  






set.seed(825)
trainIndex <- createDataPartition(data$class2, p = .6, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]



runModel = TRUE

if(runModel) {
  fitControl <- trainControl(## 5-fold CV
    method = "repeatedcv",
    number = 5, 
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    repeats = 10)  
  
  
  
  modelFit <- train(class2 ~ modularity_class + citations + avg_part, data = dataTrain, 
                    method = "gbm",  
                    trControl = fitControl,
                    #tuneGrid = grid,
                    metric = "ROC",
                    verbose = FALSE)
  print(modelFit)

  
  
  print("Prediction")
  predictions <- predict(modelFit, newdata = dataTest)
  
  cm = confusionMatrix(predictions, dataTest$class2)
  
  print(cm)
  
  precision <- cm$byClass['Pos Pred Value']    
  recall <- cm$byClass['Sensitivity']
  
  print("F-measure")
  print((2 * precision * recall)/(precision + recall))
  
  #trellis.par.set(caretTheme())
 # print(plot(modelFit, metric = "ROC"))
  
 # print(plot(modelFit, metric = "ROC", plotType = "level",
 #            scales = list(x = list(rot = 90))))
  
  predictions <- predict(modelFit, newdata = dataTest, type = "prob")
  r = roc(dataTest$class2, predictions[[2]])
  print(r$auc)
  
  
  write(r$auc, file = fileToSave, append = FALSE)
  
  
}