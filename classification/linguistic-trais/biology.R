library(caret)
library(ggplot2)
library(pROC)


fileToSave = "biology-auc"

data = read.csv("biology.csv", header = TRUE, sep = ",")

data$ari = gsub(',','',as.character(data$ari))
data$words = gsub(',','',as.character(data$words))
data$complexwords = gsub(',','',as.character(data$complexwords))
data$sentences = gsub(',','', as.character(data$sentences))
data$syllables = gsub(',','', as.character(data$syllables))
data$characters = gsub(',','', as.character(data$characters))
data$count_i_usage = as.numeric(data$count_i_usage)
data$count_you_usage = as.numeric(data$count_you_usage)
data$count_she_he_usage = as.numeric(data$count_she_he_usage)

data$ari = as.numeric(data$ari)
data$characters = as.numeric(data$characters)
data$complexwords = as.numeric(data$complexwords)
data$syllables = as.numeric(data$syllables)
data$sentences = as.numeric(data$sentences)
data$words = as.numeric(data$words)
data$count_i_usage = as.numeric(data$count_i_usage)
data$count_you_usage = as.numeric(data$count_you_usage)
data$count_she_he_usage = as.numeric(data$count_she_he_usage)


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
  
  
  
  modelFit <- train(class2 ~ ari + characters + complexwords + syllables + sentences + words + count_i_usage + count_you_usage + count_she_he_usage , data = dataTrain, 
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