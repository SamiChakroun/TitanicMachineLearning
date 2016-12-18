#==============================================================================
# Subset Selection - Modelling
#==============================================================================

  #Since it is a classification problem, we will first try to predict using a logistical regression model and see the accuracy
  
  #create a logistic regression model with the training data
  
  #Predicting survival based on passenger the 4 features we picked using best subset selection
  # Title, Pclass, FamilySize and Sex
  logistic.model = glm(Survived~Title+Pclass+FamilySize+Sex, family = "binomial", data=data.train)
  
  #Plot the model
  plot(logistic.model)

  #Summary
  summary(logistic.model)
  
  #generate predictions for training data using the predict method of the logistic model
  training_predictions = predict(logistic.model, type = "response")
  
  #compute training error use an outcome cutoff at 0.5
  training_error = sum((training_predictions >= 0.5) != 1)/nrow(data.train)
  #Training error is is very very high compared Random Forests
  training_error
  
  #training error for predictions in {0,1}
  test_predictions = predict(logistic.model, data.test, type = "response")
  
  #using a probability cutoff of 0.5 for outcome of survived, default missing to deceased
  test_predictions[test_predictions >=0.5] = 1
  test_predictions[ test_predictions != 1] = 0
  test_predictions[is.na(test_predictions)] = 0
  
  table(test_predictions)
  
  #Conclusion: Logistic regression is fast but its not accurate enough in predicting survival.
  #It is better in this case to stick to Random Forest since it gave us a much better accuracy.
  

  #===============================
  # Cross validation of glm
  #===============================
  
  #In order to make our results more accurate, we need to perform cross validation on our logistical regression model
  
  #Load caret library
  library(caret)
  
  cv.folds.10 = createMultiFolds(regfit.full, k = 10, times = 10)
  
  # Set up caret's trainControl object per above.
  control.2 = trainControl(method = "repeatedcv", number = 10, repeats = 10)
  
  # Drop the NA factor
  data.train$Survived = factor(data.train$Survived)
  
  # Set seed for reproducibility and train
  set.seed(1111)
  glm.10.cv.1 = train(x = data.train[,-1], y = data.train$Survived, method ="glm", family="binomial", trControl = control.2)
  
  # Check the results
  glm.10.cv.1
  
  # Another way to see the results and calculate accuracy mannually
  PredTrain = predict(glm.10.cv.1, newdata=data.train, type="raw") 
  table(data.train$Survived, PredTrain)
  
  #almost the same accuracy as given by bs.10.cv.1
  accuracy = (1 - (86/485)) * 100
  accuracy
  