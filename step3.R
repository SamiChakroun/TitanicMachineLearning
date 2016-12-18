#==============================================================================
# Getting Started - Cleaning the data
#==============================================================================

  #Duplicates
  
  #Checking the duplicate names and storing them in a vector
  duplicated.names = data.combined[which(duplicated(as.character(data.combined$Name))),"Name"]
  
  #Go through the names of the combined data and if the name is in duplicated.names pull it
  data.combined[which(data.combined$Name %in% duplicated.names),]
  
  #The duplicate names seem to correspond to different people since the age, ticket numbers, and fares are different for each of them
  
  #NA values in Age
  
  #============================================================
  #THIS STEP IS DONE AFTER STEP4 (Extracting the title feature)
  #============================================================
  
  #Get all the medians
  
  #Median of "Master." for missing values
  master = data.combined[which(data.combined$Title == "Master."),]
  median.master = summary(master$Age)[3] #Summary[3] corresponds to the median
  
  #Median of "Mrs." for missing values
  mrs = data.combined[which(data.combined$Title == "Mrs."),]
  median.mrs = summary(mrs$Age)[3]
  
  #Median of "Mr." for missing values
  mr = data.combined[which(data.combined$Title == "Mr."),]
  median.mr = summary(mr$Age)[3]
  
  #Median of "Miss." for missing values
  miss = data.combined[which(data.combined$Title == "Miss."),]
  median.miss = summary(miss$Age)[3]
  
  for(i in 1:nrow(data.combined)){
    if((data.combined[i,"Title"] == "Mrs.") & is.na(data.combined[i,"Age"]))
    {
      data.combined[i,"Age"] = median.mrs
    }
    
    else if((data.combined[i,"Title"] == "Mr.") & is.na(data.combined[i,"Age"]))
    {
      data.combined[i,"Age"] = median.mr
    }
    
    else if((data.combined[i,"Title"] == "Master.") & is.na(data.combined[i,"Age"]))
    {
      data.combined[i,"Age"] = median.master
    }
    
    else if((data.combined[i,"Title"] == "Miss.") & is.na(data.combined[i,"Age"]))
    {
      data.combined[i,"Age"] = median.miss
    }
  }
  
  sum(is.na(data.combined[,"Age"]))
  
  #We still have one NA value in age that we will change manually
  
  last.na.index = which(is.na(data.combined[,"Age"]))
  
  data.combined[last.na.index,]
  
  #We will give the Dr. the median of Doctors
  dr = data.combined[which(data.combined$Title == "Dr."),]
  median.dr = summary(dr$Age)[3]
  data.combined[last.na.index,"Age"] = median.dr
  
  #DONE WITH AGE!
  
  #Replacing the NA's with the median value of 29 for Mr. isn't the best solution and will probably lead to overfitting as we see in the plottings in step5.
  
  summary(data.combined$Age)
  
  #We can now do some plotting to see what it gives us when it comes to survival!
  
  #Outliers
  
  #We don't seem to have any particular outliers. Except for one person who paid 512 pounds for their ticket and survived.
  #We won't remove this row since every passenger matters.
  