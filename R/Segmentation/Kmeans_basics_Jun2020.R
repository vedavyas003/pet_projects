# Read the titanic dataset from CSV
titanic <- read.csv("titanic.csv")

# Get the dimensions of the dataset
dim(titanic)

# Compute the mean age, excluding missing values
meanage <- sum(na.omit(titanic$Age)) / length(na.omit(titanic$Age))
meanage

# Replace missing age values with the mean age and round to the nearest integer
titanic$Age[is.na(titanic$Age)] = meanage
titanic$Age = round(titanic$Age)

# Categorize age into age groups
titanic$AgeCat[titanic$Age >= 0 & titanic$Age <= 16] = "0-16"
titanic$AgeCat[titanic$Age >= 17 & titanic$Age <= 32] = "17-32"
titanic$AgeCat[titanic$Age >= 33 & titanic$Age <= 48] = "33-48"
titanic$AgeCat[titanic$Age >= 49 & titanic$Age <= 64] = "49-64"
titanic$AgeCat[titanic$Age >= 65] = "65 and above"

# Map survived values to "Survived" and "Not Survived" categories
titanic$Survived[titanic$Survived == 0] = "Not Survived"
titanic$Survived[titanic$Survived == 1] = "Survived"

# Convert variables to factors
titanic$Pclass = factor(titanic$Pclass)
titanic$AgeCat = factor(titanic$AgeCat)
titanic$Survived = factor(titanic$Survived)
titanic$Embarked = as.character(titanic$Embarked)

# Map Embarked values to their corresponding locations
titanic$Embarked[titanic$Embarked == "S"] = "Southampton"
titanic$Embarked[titanic$Embarked == "C"] = "Cherbourg"
titanic$Embarked[titanic$Embarked == "Q"] = "Queenstown"
titanic$Embarked = factor(titanic$Embarked)

# Remove unwanted columns (9 and 11) from the dataset
titanic = titanic[c(-9, -11)]

# View the updated dataset
View(titanic)

# Write the updated dataset to a new CSV file
write.csv(titanic, file = "./titanicNew.csv")

# Read the new CSV file
titanicNew = read.csv("titanicNew.csv")

# Create a copy of the dataset
titanicUpdated = titanicNew

# Convert "Survived" to numerical values (0 for "Not Survived", 1 for "Survived")
SurvivedNum = ifelse(titanicUpdated$Survived == "Not Survived", 0, 1)
titanicUpdated = data.frame(titanicUpdated, SurvivedNum)

# Convert "Sex" to numerical values (1 for "male", 0 for "female")
SexN = ifelse(titanicUpdated$Sex == "male", 1, 0)
titanicUpdated = data.frame(titanicUpdated, SexN)

# Convert "Embarked" to numerical values (1 for "Southampton", 2 for "Cherbourg", 0 for "Queenstown")
EmbarkedN = ifelse(titanicUpdated$Embarked == "Southampton", 1,
                   ifelse(titanicUpdated$Embarked == "Cherbourg", 2, 0))
titanicUpdated = data.frame(titanicUpdated, EmbarkedN)

# Write the updated dataset to a new CSV file
write.csv(titanicUpdated, file = "./titanicUpdated.csv")

# Scale the selected variables in the dataset
titanic.scaled = scale(data.frame(titanic$Age, titanic$Parch, titanic$SibSp, titanic$Fare))

# Perform k-means clustering and evaluate total within sum of squares (totwss) and total between sum of squares (btwss)
totwss = vector()
btwss = vector()
for (i in 2:15) {
  set.seed(1234)
  temp = kmeans(titanic.scaled, centers = i)
  totwss[i] = temp$tot.withinss
  btwss[i] = temp$betweenss
}

# Plot total within sum of squares
plot(totwss, xlab = "Number of Clusters", type = "b", ylab = "Total Within Sum of Squares")

# Plot total between sum of squares
plot(btwss, xlab = "Number of Clusters", type = "b", ylab = "Total Between Sum of Squares")

# Install the Rserve package from RForge
install.packages('Rserve', repos = 'http://www.rforge.net/')

# Load the Rserve library
library(Rserve)

# Start the Rserve server
Rserve() 

# connecting this data to tableau