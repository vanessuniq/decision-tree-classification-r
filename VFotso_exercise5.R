# UMGC DATA 630
# Exericse 5
# Written by Vanessa Fotso

# Install and load "party" into memory
install.packages("party")
library("party")

library("DescTools")

#Read the data into a data frame, assuming being in the right directory
credit <- read.csv("CreditApproval.csv", as.is = FALSE)

#preview data & structure
head(credit)
str(credit) 

# Data pre-processing
# Remove the unique identifier
credit$Key <- NULL

# running Factor function on zipcode variable
credit$Zipcode <- factor(credit$Zipcode)

# data summary
summary(credit)

# replace missing values in age variable
credit$Age[is.na(credit$Age)] <- mean(credit$Age, na.rm=TRUE)

# replace missing values in zipcode with mode
credit$Zipcode[is.na(credit$Zipcode)] <- Mode(credit$Zipcode, na.rm = TRUE)
summary(credit)


# split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(credit), replace = TRUE, prob = c(0.7, 0.3))
train.data <- credit[ind == 1, ]
test.data <- credit[ind == 2, ]

# Run the method on a training data
myFormula<-class~.
model <- ctree(myFormula, data = train.data)

# output the tree structure
print(model) 

# visualize the tree

plot(model)


# confusion matrix
table(predict(model), train.data$class)
prop.table(table(predict(model), train.data$class))

# Evaluate the model on a test data
testPred <- predict(model, newdata = test.data)
table (testPred, test.data$class)

# End Script