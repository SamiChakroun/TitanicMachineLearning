#==============================================================================
# Exploratory analysis - Plotting using ggplot2
#==============================================================================

  #load ggplot2 library for visualizations
  library(ggplot2)

  #remember our variables are:
  names(data.combined)
  
  #we will see how each of them relates to the outcome through plottings.
  #using the train data which is rows 1:891 in the data.combined (contains values for outcome Survived)
  
  #Hypothesis: 1st class people have more chances of survival
  ggplot(data.combined[1:891,], aes(x = Pclass, fill = factor(Survived))) +
    geom_bar() +
    ggtitle("Distribution among classes of passengers who survived and passenger who perished") +
    xlab("Pclass") +
    ylab("Passengers Count") +
    labs(fill = "Survived") 
  
  
  #Visualize the 3-way relationship of gender, pclass, and survival
  ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) + 
    ggtitle("Pclass") +
    xlab("Sex") +
    ylab("Passengers Count") +
    labs(fill = "Survived")
  
  #Hypothesis: Titles are indicative of survival or not
  ggplot(data.combined[1:891,], aes(x = Title, fill = factor(Survived))) +
    geom_bar() +
    ggtitle("Distribution among titles of passengers who survived and passenger who perished") +
    xlab("Title") +
    ylab("Passengers Count") +
    labs(fill = "Survived") 
  
  #Hypothesis: Family size tells us about survival
  ggplot(data.combined[1:891,], aes(x = FamilySize, fill = factor(Survived))) +
    geom_bar() +
    ggtitle("Distribution among family sizes of passengers who survived and passenger who perished") +
    xlab("FamilySize") +
    ylab("Passengers Count") +
    labs(fill = "Survived") 
  
  #Hypothesis: Age distribution among classes determines survival
  ggplot(data.combined[1:891,], aes(x = Age, fill = factor(Survived))) +
    geom_bar() +
    ggtitle("Distribution of age among passengers who survived and passenger who perished") +
    xlab("Age") +
    ylab("Passengers Count") +
    labs(fill = "Survived") 
  
  #A scatter plot might show better results
  ggplot(data.combined[1:891,], aes(x=Age, y=Pclass, color = Survived)) +
    geom_point(shape=4) +
    ggtitle("Distribution of age among passengers who survived and passenger who perished") +
    labs(fill = "Survived") 

  
  #Hypothesis: Fare could be indicative of survival or not
  ggplot(data.combined[1:891,], aes(x=Fare, y=Pclass, color = Survived)) +
    geom_point(shape=4) +
    ggtitle("Distribution of Fare among classes showing who survived and who died") +
    labs(fill = "Survived") 
<<<<<<< HEAD
  
#The purpose of these plots is to detect any patterns explaining survival according to the different features we have.
=======
  
>>>>>>> fbceeaef6787e3f2b49c909054670167b9c50c24
