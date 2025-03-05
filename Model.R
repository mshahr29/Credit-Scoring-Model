#Data without Pensioners

#Importing Data Set
cc <- read.csv(file.choose())


str(cc)

#Removing ID variable
cc <- cc[ ,-1]

#Converting variables in factor variables
cc <- as.data.frame(unclass(cc), stringsAsFactors = TRUE)
str(cc)

cc$DECISION <- as.factor(cc$DECISION)
cc$FLAG_WORK_PHONE <- as.factor(cc$FLAG_WORK_PHONE)
cc$FLAG_PHONE <- as.factor(cc$FLAG_PHONE)
cc$FLAG_EMAIL <- as.factor(cc$FLAG_EMAIL)
cc$FAMILY_ADULTS <- as.factor(cc$FAMILY_ADULTS)

Train <- cc[1:14242, ]
Test <- cc[14243:16755, ]

#ROSE Code
library(ROSE)
majority <- subset(Train, Train$DECISION == 0)
minority <- subset(Train, Train$DECISION == 1)
oversampled_data <- ROSE(formula= DECISION~., data = Train, seed = 123, N = 14242)
oversampled_data <- oversampled_data$data
table(oversampled_data$DECISION)
# Now you can use 'oversampled_data' for modeling purposes

# Train a logistic regression model

set.seed(123)

#model run
model <- glm(formula = DECISION~., 
             data = oversampled_data,
             family = "binomial")
summary(model)

#StepWise Regression Model

model2 <- step(model)
summary(model2)

#ROC Plots and PR Curve
library(PRROC)

roc <- roc.curve(scores.class0 = model2$fitted.values, #predicted probabilities 
                 weights.class0 = as.numeric(as.character(oversampled_data$DECISION)), #actual flag, 
                 curve = T)
print(roc)
plot(roc)

prcurve <- pr.curve(scores.class0 = model2$fitted.values, 
                    weights.class0 = as.numeric(as.character(oversampled_data$DECISION)), 
                    curve = T)
print(prcurve)
plot(prcurve)

#predictions
trainpreds <- model$fitted.values
testpreds <- predict(model, #model name
                     Test, #data name
                     type = "response")

allpreds <- c(trainpreds, testpreds)

write.csv(allpreds, file = "PredictionNoPensioner.csv")

#Decision Tree

library(rpart)
set.seed(123)
decisiontreemodel <- rpart(formula = DECISION ~ ., 
                           data = oversampled_data)

decisiontreemodel
summary(decisiontreemodel)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(decisiontreemodel)

decisiontreemodel2 <- rpart(formula = DECISION~., 
                            data = oversampled_data, 
                            control = rpart.control(minbucket = 500))

rpart.plot(decisiontreemodel2)
summary(decisiontreemodel2)
