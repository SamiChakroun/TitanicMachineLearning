#==============================================================================
# Getting Started - Extracting Features
#==============================================================================

  #Title feature
  
  #Function to extract titles from names
  extractTitle = function(Name) {
    
    Name = as.character(Name) #Since the Name variable is of type factor
    
    if(length(grep("Miss.", Name))>0 | length(grep("Ms.", Name))>0 | length(grep("Mlle.", Name))>0) #grep for pattern matching, >0 to see if the grep function returned an index
      return("Miss.")
    else if(length(grep("Mrs.", Name))>0 | length(grep("Mme.", Name))>0 | length(grep("the Countess.", Name))>0) #important to put Mrs before Mr since otherwise all Mrs. will be taken as Mr.
      return("Mrs.")
    else if(length(grep("Mr.", Name))>0 | length(grep("Don.", Name))>0 | length(grep("Jonkheer.", Name))>0) #Jonkheer and Don are equivalent to Mr.
      return("Mr.")
    else if(length(grep("Master.", Name))>0)
      return("Master.")
    else if(length(grep("Dr.", Name))>0)
      return("Dr.")
    else if(length(grep("Rev.", Name))>0 | length(grep("Col.", Name))>0 | length(grep("Major.", Name))>0 | length(grep("Capt.", Name))>0)
      return("Military.")
    else return("NA")
    
  }
  
  #Vector to store titles
  titles = c()
  
  #For loop over the names to extract titles
  for(i in 1:nrow(data.combined)) {
    titles = c(titles, extractTitle(data.combined[i,"Name"]))
  }
  
  #Finally we append this vector to our data.combined as a factor variable type
  data.combined$Title = as.factor(titles)
  
  #Now we can see how many passengers of each title we have in our data
  summary(data.combined$Title)
  
  #We have some NA's in Title so let's see what these are
  data.combined[which(data.combined$Title == "NA"),]
  
  #After checking the NA's we add some types of titles we didn't notice before such as Dr. , Rev. and Mme.
  
  #FamilySize feature
  
  #Get the SibSp and Parch values and sum them to get family size
  temp.sibsp = c(train$SibSp, test$SibSp)
  temp.parch = c(train$Parch, test$Parch)
  data.combined$FamilySize = as.factor(temp.sibsp + temp.parch + 1) #+1 for the passenger we're considering
  
  
  # Visualize it to see if it is predictive
  
  #install package if needed
  install.packages("ggplot2")
  
  #load ggplot2 library for visualizations
  library(ggplot2)
  
  #Plot
  ggplot(data.combined[1:891,], aes(x = FamilySize, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("Pclass, Title") +
    xlab("familySize") +
    ylab("Number of passengers") +
    ylim(0,125) +
    labs(fill = "Survived")
  
