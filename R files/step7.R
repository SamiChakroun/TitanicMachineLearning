#==============================================================================
# Subset Selection - Random Forests
#==============================================================================

  #================
  #RANDOM FOREST 1
  #================
  
  #load the randomForest library
  library(randomForest)
  
  #Train a Random Forest with the default parameters using all features except the outcome of course
  rf.train.1 = data.clean[1:891,-1]
  rf.label = as.factor(train$Survived)
  
  set.seed(1111) #For reproducibility
  rf.1 = randomForest(x = rf.train.1, y = rf.label, importance = TRUE) #Train a Random Forest
  rf.1
  
  #Interpretation:
  #Out of bag value (OOB)
  #OOB estimate of  error rate: 16.61%
  
  # The confusion matrix in the results of the Random Forest is very interesting since it tells us that this model is
  # Good at predicting who will die (around 99% accuracy to predict it correctly)
  # Worse at predicting who will survive (around 73% accuracy to predict it correctly)
  
  varImpPlot(rf.1)
  #This graph is what we will base our feature selection on.
  #It tells us which features are most significant in making better predictions.
  
  #================
  #RANDOM FOREST 2
  #================
  
  #Train a Random Forest with less features by removing the least significant ones
  rf.train.2 = data.clean[1:891,-c(1,5,6)]
  rf.label = as.factor(train$Survived)
  
  set.seed(1111) #For reproducibility
  rf.2 = randomForest(x = rf.train.2, y = rf.label, importance = TRUE) #Train a Random Forest
  rf.2
  
  #Interpretation:
  #OOB estimate of  error rate: 16.5%
  #We got a smaller error rate which is good!
  
  varImpPlot(rf.2)
  #Embarked is now not very significant so let's remove it and see!
  
  #================
  #RANDOM FOREST 3
  #================
  
  #Train a Random Forest with less features by removing the least significant ones
  rf.train.3 = data.clean[1:891,-c(1,5,6,8)] #8 is the index of Embarked
  rf.label = as.factor(train$Survived)
  
  set.seed(1111) #For reproducibility
  rf.3 = randomForest(x = rf.train.3, y = rf.label, importance = TRUE) #Train a Random Forest
  rf.3
  
  #Interpretation:
  #OOB estimate of  error rate: 16.05%
  #Even smaller error rate!
  
  varImpPlot(rf.3)
  #Now, what if we remove Age?
  
  #================
  #RANDOM FOREST 4
  #================
  
  #Train a Random Forest with less features by removing the least significant ones
  rf.train.4 = data.clean[1:891,-c(1,5,6,8,4)] #4 is the index of Age
  rf.label = as.factor(train$Survived)
  
  set.seed(1111) #For reproducibility
  rf.4 = randomForest(x = rf.train.4, y = rf.label, importance = TRUE) #Train a Random Forest
  rf.4
  #Interpretation:
  #OOB estimate of  error rate: 16.61%
  #HIGHER error rate!
  
  varImpPlot(rf.4)
  #Maybe removing Sex could improve our model?
  
  #================
  #RANDOM FOREST 5
  #================
  
  #Train a Random Forest with less features by removing the least significant ones
  rf.train.5 = data.clean[1:891,-c(1,5,6,8,3)] #3 is the index of column Sex
  rf.label = as.factor(train$Survived)
  
  set.seed(1111) #For reproducibility
  rf.5 = randomForest(x = rf.train.5, y = rf.label, importance = TRUE) #Train a Random Forest
  rf.5
  #Interpretation:
  #OOB estimate of  error rate: 16.05%
  #Same as rf.3 but this one is simpler so we will keep it.
  
  varImpPlot(rf.5)
  
  #==============================================================================
  # Subset Selection - Cross Validation
  #==============================================================================
  
  #====================
  # Cross Validation 1
  #====================
  
  library(caret)
  
  #We will perform a 10-fold cross validation 10 times.
  
  #First we create our 10 folds from our rf.label
  #Stratification is done automatically in this step. This should give us more accurate results
  #Same proportions of survived and died
  cv.folds.10 = createMultiFolds(rf.label, k = 10, times = 10)
  
  # Set up caret's trainControl object per above.
  control.1 = trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.folds.10)
  
  # Set seed for reproducibility and train
  set.seed(1111)
  rf.5.cv.1 = train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 500, trControl = control.1)
  
  # Check the results
  rf.5.cv.1
  
  #We have a good accuracy of 83.49% which is a bit worse than what we predicted without the cross validation.
  #Accuracy without cv was 100 - 16.05 = 83.95
  
  #====================
  # Cross Validation 2
  #====================
  
  #We will perform a 5-fold cross validation 10 times.
  
  #First we create our 5 folds from our rf.label
  #Stratification is done automatically in this step. This should give us more accurate results
  #Same proportions of survived and died in each fold
  cv.folds.5 = createMultiFolds(rf.label, k = 5, times = 10)
  
  # Set up caret's trainControl object per above.
  control.2 = trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.folds.5)
  
  # Set seed for reproducibility and train
  set.seed(1111)
  rf.5.cv.2 = train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 500, trControl = control.2)
  
  # Check the results
  rf.5.cv.2
  
  #We got almost the same accuracy (83.47%) which is good.
  
  
  #=======================
  # Kaggle submission 1
  #=======================
  
  # Subset our test records and features
  submit = data.clean[892:1309, -c(1,5,6,8,3)]
  
  # Make predictions
  rf.5.preds = predict(rf.5, submit)
  table(rf.5.preds) #looks correct: 2/3 by 1/3 almost.
  
  # Write out a CSV file for submission to Kaggle
  submission = data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
  
  write.csv(submission, file = "Submission_1_Sami.csv", row.names = F)
  
  # This submission gave a score of 0.77990.
  # 77.99% is different from our accuracy we predicted from the training data.
  # This means we have some overfitting in our modelling.
  
  #=======================
  # Kaggle submission 2
  #=======================
  
  # Let's try a submission with the model that had one more variable and gave the same accuracy.
  
  # Subset our test records and features
  submit = data.clean[892:1309, -c(1,5,6,8)]
  
  # Make predictions
  rf.5.preds = predict(rf.3, submit)
  table(rf.5.preds) #looks correct: 2/3 by 1/3 almost.
  
  # Write out a CSV file for submission to Kaggle
  submission = data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
  
  write.csv(submission, file = "Submission_2_Sami.csv", row.names = F)
  
  # This submission improved the score by 0.00957.
  # The score is now 0.78947
  # We still have some overfitting but it seems that Age is an important feature.
  