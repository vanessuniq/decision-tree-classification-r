# UMGC DATA 630
# Assignment 3
# Written by Vanessa Fotso

# load libraries into memory

library("party")

library("DescTools")

#Read the data into a data frame, assuming being in the right directory
hof <- read.csv("MLBHOF.csv", as.is = FALSE)

#preview data & structure
head(hof)
str(hof)
summary(hof)

# Data pre-processing

# running Factor function on Hall of fame variable
hof$Hall.Of.Fame.Membership <- factor(hof$Hall.Of.Fame.Membership)

# replace missing values in strikeouts, stolen bases, caught stealing and stolen base runs variables with their respective mean value

hof$Strikeouts[is.na(hof$Strikeouts)] <- mean(hof$Strikeouts, na.rm=TRUE)
hof$Stolen.Bases[is.na(hof$Stolen.Bases)] <- mean(hof$Stolen.Bases, na.rm=TRUE)
hof$Stolen.Base.Runs[is.na(hof$Stolen.Base.Runs)] <- mean(hof$Stolen.Base.Runs, na.rm=TRUE)
hof$Caught.Stealing[is.na(hof$Caught.Stealing)] <- mean(hof$Caught.Stealing, na.rm=TRUE)

summary(hof)

# split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(hof), replace = TRUE, prob = c(0.7, 0.3))
train.data <- hof[ind == 1, ]
test.data <- hof[ind == 2, ]

# Run the method on a training data
myFormula<-Hall.Of.Fame.Membership~.
model <- ctree(myFormula, data = train.data)

# output the tree structure
print(model) 

# visualize the tree

plot(model)


# confusion matrix
table(predict(model), train.data$Hall.Of.Fame.Membership)
prop.table(table(predict(model), train.data$Hall.Of.Fame.Membership))

# Evaluate the model on a test data
testPred <- predict(model, newdata = test.data)
# confusion matrix
table (testPred, test.data$Hall.Of.Fame.Membership)

# End Script