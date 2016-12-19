#==============================================================================
# Subset Selection - Modelling with Support Vector Machine
#==============================================================================

  #========
  # SVM 1
  #========
  
  #Load one of the libraries containing an SVM implementation
  library(e1071)
  
  #Fit a model using SVM
  svmfit=svm(Survived~., data=data.train, kernel="linear", cost=10,scale=FALSE)
  
  #Check the summary
  summary(svmfit)
  
  #Plot the support vector classifier obtained
  #plot(svmfit, data.train) #ERROR: not possible to plot all features
  
  #For reproducibility
  set.seed(1111)
  
  #Perform cross-validation
  tune.out=tune(svm,Survived~.,data=data.train,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
  
  #Check the results
  summary(tune.out)
  
  #Our best performance obtained is 0.1717
  #Meaning an accuracy of
  (1 - 0.1717353) *100 #Not bad for a first try.
  
  bestmod=tune.out$best.model
  
  summary(bestmod)
  
  #========
  # SVM 2
  #========
  
  #Now let's make some changes to our SVM parameters
  
  #Fit a model using SVM
  svmfit.1=svm(Survived~., data=data.train[,-c(5,6,8)], kernel="linear", cost=10,scale=FALSE)
  
  #Check the summary
  summary(svmfit.1)
  
  #For reproducibility
  set.seed(1111)
  
  #Perform cross-validation
  tune.out.1=tune(svm,Survived~.,data=data.train[,-c(5,6,8)],kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
  
  #Check the results
  summary(tune.out.1)
  
  #The result is barely better.
  
  #===========================
  # Kaggle submission SVM 1
  #===========================
  
  # Make predictions
  Survived.pred = predict(bestmod , data.test)
  
  #Overview of the results
  table(Survived.pred)
  
  # Write out a CSV file for submission to Kaggle
  submission3 = data.frame(PassengerId = rep(892:1309), Survived = Survived.pred)
  
  write.csv(submission3, file = "Submission_3_Sami.csv", row.names = F)
  
  # This submission gave a score of 0.78469
  # This score is very close to what we obtained using random forests