#==============================================================================
# Subset Selection - Feature Selection
#==============================================================================

  #We first start by eliminating some of the features we know won't be useful. (Name, Cabin and Ticket)
  
  #Check their indices
  names(data.combined)

  #3 for name, 8 for Ticket, 10 for Cabin

  #Make a clean copy of the data
  data.clean = data.combined[,-c(3,8,10)]
  
  #The result is good.
  str(data.clean)
  
  #From now on, we won't need the test data to work with so we can go ahead and split our data
  data.train = data.clean[1:891,]
  data.test = data.clean[892:nrow(data.clean),]
  
  #=======================
  #Best subset selection
  #=======================
  
  #Since our number of features is relatively small (around 10 features), we can perform best subset selection without
  #worrying about computational cost. The function will build 2^10 models very quickly.
  
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
  #Gender also is significant but not that much which is understandable since Title gives an idea about the gender.
  
  #SibSp is surprisingly significant. But we see that FamilySize is even more significant.
  #The plotting suggests that we should change our FamilySize feature in the following way:
    #FamilySize should be a factor of 3 levels
      #Passengers traveling alone (FamilySize=1)
      #Passengers of small family size (FamilySize <= 4) 
      #Passengers of big family size (FamilySize>=5)
    #This is due to the fact that larger families tend to perish together.
#===================================================================================================================

  
  #============================================================================================================
  #In order to make our results more accurate, we need to perform cross validation to choose the best subsets
  #============================================================================================================
  
  
  #Since there is no predict function for regsubsets we create one
  predict.regsubsets = function (object ,newdata ,id ,...) {
    form=as.formula(object$call[[2]])
    mat=model.matrix(form,newdata)
    coefi=coef(object ,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
  }
  
  k=10
  set.seed(1111)
  folds=sample(1:k,nrow(data.train),replace=TRUE)
  cv.errors=matrix(NA,k,10, dimnames=list(NULL, paste(1:10)))
  
  
  #ERROR HERE
  for(j in 1:k){
    best.fit = regsubsets(Survived~., data=data.train[folds!=j,])
    for(i in 1:10){
      pred=predict(best.fit,data.train[folds==j,],id=i) 
      cv.errors[j,i]=mean((as.integer(data.train$Survived[folds==j])-pred)^2)
      }
  }
                           
  mean.cv.errors=apply(cv.errors ,2,mean)
  mean.cv.errors
  
  par(mfrow=c(1,1))
  plot(mean.cv.errors ,type="b")
  
  