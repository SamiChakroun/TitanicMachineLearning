#==============================================================================
# Getting Started - Summary Statistics
#==============================================================================

  #First open the data from the global environment area to have a look at it and get a general idea (data.combined)
  
  #We also use the functions head and tail to look at some rows of our data
  head(data.combined)
  tail(data.combined)
  
  #Using str gives us an idea about the type of each feature
  str(data.combined)
  
  #Some types do not match the features we have therefore we should change them: Survived and Pclass
  
  #Convert "Survived" to type factor
  data.combined$Survived = as.factor(data.combined$Survived)
  
  #Convert "Pclass" to type factor
  data.combined$Pclass = as.factor(data.combined$Pclass)
  
  #Check the results
  str(data.combined)
  
  #Using summary gives us some information about the distribution of each feature
  summary(data.combined)
  
#==============================================================================
# Striking observations from the first 7 variables:
  
# Survival has a ratio of around 2/3 perish 1/3 survive in the train data.
# 3rd class people are the predominent passengers on the Titanic.
# The name seems to always indicate a title such as Mr. or Mrs.
# The passengers are around 2/3 males and 1/3 females.
# The age has too many missing values and we need to fix that since it could be very useful for predictions.
# Parch and SibSp have medians of 0 which tells us that most people were traveling alone.
  
#It looks like the last 4 variables "Ticket", "Fare", "Cabin" and "Embarked" wouldn't help us much:
  #The Ticket numbers are very messed up.
  #We have only 1 Fare value missing so we should check if it has any relation with survival.
  #Cabin has a lot of NAs even though they don't show as NAs in the summary. We can see that directly from looking at the data.  
  #Embarked should not have an impact on survival but we will check if there is a pattern there.
#==============================================================================
  
  #See the pairs plot. Since this is a classification problem, this won't help us much.
  pairs(data.combined)
  
#=======================================
# We now know a bit more about our data.
#=======================================
