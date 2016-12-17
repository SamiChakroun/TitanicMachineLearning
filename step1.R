#==============================================================================
# Getting Started - Loading the data
#==============================================================================

  #Clean the workspace to avoir any confusions with other variables
  rm(list=ls()) #remove variables
  
  #Check working directory and set it
  
  #Get current working directory
  getwd()
  
  #Set the working directory to the Titanic folder
  setwd("/Users/samichakroun/Desktop/TitanicMachineLearning/TitanicMachineLearning")
  
  #Loading the data
  
  #Load the raw data from the csv files and store it in variables
  train = read.csv("train.csv", header = T) #header is set to true since when we open the files in excel we see that the first row is a header name row
  test = read.csv("test.csv", header = T)

  #==============================================================#
  # There is no outcome in test.                                 #
  # We will combine test and train.                              #
  # This allows us to look at more general aspects of the data.  #
  # Also passenger id will be removed (row.names = passenger id) #
  #==============================================================#
  
  #Let's combine both sets into one in order to look at the whole data
  
  #First let's make the row names in test match passenger id
  row.names(test) = test[,1]
  
  #Remove passenger id column
  test = test[,-1]
  train = train[,-1]
  
  #Create data frame with survived column for the test set and fill it with NAs
  test.survived = data.frame(Survived = rep("NA", nrow(test)), test[,])
  
  #Combine datasets
  data.combined = rbind(train, test.survived)

#================================================================================
#The results of step1 is our data combined and well organized in data.combined
#================================================================================
   