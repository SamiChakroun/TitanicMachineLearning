#==============================================================================
# Subset Selection - Feature Selection
#==============================================================================

  #We first start by eliminating some of the features we know won't be useful. (Name, Cabin and Ticket)
  
  #Check their indices
  names(data.combined)
  #Make a clean copy of the data
  data.clean = data.combined[,-c(3,8,10)]
  #The result is good.
  str(data.clean)
  
  #From now on, we won't need the test data to work with so we can go ahead and split our data
  data.train = data.clean[1:891,]
  data.test = data.clean[892:nrow(data.clean),]
  
  #Best subset selection
  
  #Load the leaps library
  library(leaps)
  
  #Best subset
  regfit.full=regsubsets(Survived~.,data.train)
  reg.summary=summary(regfit.full)
  reg.summary
  
  par(mfrow=c(2,2))
  plot(regfit.full ,scale ="r2")
  plot(regfit.full ,scale ="adjr2")
  plot(regfit.full ,scale ="Cp")
  plot(regfit.full ,scale ="bic")
  
  #Best subset forward
  
  regfit.fwd=regsubsets(Survived~.,data.train, method = "forward")
  reg.summary=summary(regfit.fwd)
  reg.summary
  
  par(mfrow=c(2,2))
  plot(regfit.fwd ,scale ="r2")
  plot(regfit.fwd ,scale ="adjr2")
  plot(regfit.fwd ,scale ="Cp")
  plot(regfit.fwd ,scale ="bic")
  
  
  #Best subset backward
  
  #Best subset
  regfit.bwd=regsubsets(Survived~.,data.train, method = "backward")
  reg.summary=summary(regfit.bwd)
  reg.summary
  
  par(mfrow=c(2,2))
  plot(regfit.bwd ,scale ="r2")
  plot(regfit.bwd ,scale ="adjr2")
  plot(regfit.bwd ,scale ="Cp")
  plot(regfit.bwd ,scale ="bic")
  
#===================================================================================================================
  #The results confirm our assumptions but also are a bit surprising.
  
  #The best features seem to be by far Pclass and Title.
  #Gender also is significant but not that much which is understandable since title gives an idea about the gender.
  
  #SibSp is surprisingly significant. But we see that FamilySize is even more significant.
  #The plotting suggests that we should change our FamilySize feature in the following way:
  #FamilySize should be a factor of 3 levels
  #Passengers traveling alone (FamilySize=1)
  #Passengers of small family size (FamilySize <= 4) 
  #Passengers of big family size (FamilySize>=5)
#===================================================================================================================

  
  #In order to make our results more accurate, we need to perform cross validation to choose the best subsets