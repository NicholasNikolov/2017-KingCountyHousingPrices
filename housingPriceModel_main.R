### Housing Price Prediction Model ###

### MAT 453 - Nick, Ronny, Alex ###

# 1. DATA IMPORT AND VARIABLE CREATION ------------------------------------

# Note: I removed variable date because it was causing issues.

# For some reason lm() thought each date was a separate variable.

library(car)

housingData <- read.csv("kc_house_data.csv",na.omit=TRUE)

housingData = housingData[,-c(1,2,6,18,19)]
housingData$zipcode = as.factor(housingData$zipcode) #categorizes Zip Code
housingData_dv = model.matrix(price~.,data=housingData)[,-1] # Creates dummy variables for zip code
housingData_dv = cbind(housingData[,1],housingData_dv) # creates full dataset including price
housingData_dv = as.data.frame(housingData_dv) # Convert data into data frame
names(housingData_dv)[1] = "price" # Correct column 1 name from "v1" to "price"

# EXTRA: Basic Descriptive Statistics -------------------------------------

# 

# 2. DATA PREPROCESSING ---------------------------------------------------

# Split data into training set and test set with 75% obsv in training

set.seed(4243)

train = sample(length(housingData_dv[,4]),length(housingData_dv[,4])/1.75) # Random sample of numbers from 1 to 21613 which will index data
test = (-train) # Test set

# 3. LINEAR REGRESSION MODEL ----------------------------------------------

pricePred <- lm(price~., data = as.data.frame(housingData_dv)[train,])
summary(pricePred)

# 4. INITIAL MODEL EVALUATION ---------------------------------------------

# This step searches for multicollinearity and influential points.

library(corrplot)
library(qpcR)
library(ggplot2)

vif(pricePred)



corrplot(cor(housingData[,-14]),method="shade",type="lower",tl.col = "black") # Shows correlation without Zipcode dummies
                                                                              

plot(residuals(pricePred),main="Residual Plot", ylab="Residual")
stdev = sd(residuals(pricePred))

abline(h=mean(residuals(pricePred))+3*stdev,col="red")
abline(h=mean(residuals(pricePred))-3*stdev,col="red")
qqnorm(as.vector(residuals(pricePred)))
qqline(as.vector(residuals(pricePred)),col="red")

pricePred_pred = predict(pricePred,newdata=housingData_dv[test,])
sqrt(mean((as.vector(pricePred_pred)-housingData_dv[test,1])^2))
mean(abs(as.vector(pricePred_pred)-housingData_dv[test,1]))

# 5. COUNTING OUTLIERS -------------------------------------------------------


count=0
for(i in 1:length(residuals(pricePred)))
{
  if(residuals(pricePred)[i]<mean(residuals(pricePred))+3*stdev)
  {
    if(residuals(pricePred)[i]>mean(residuals(pricePred))-3*stdev)
    {
      count = count+1
    }

  }
}

percentDataInControl = count/length(residuals(pricePred))

highOutliers = 0
for(i in 1:length(residuals(pricePred)))
{
  if(residuals(pricePred)[i]>mean(residuals(pricePred))+3*stdev)
  {
    highOutliers = highOutliers+1
  }
}

lowOutliers = 0
for(i in 1:length(residuals(pricePred)))
{
  if(residuals(pricePred)[i]<mean(residuals(pricePred))-3*stdev)
  {
    lowOutliers = lowOutliers+1
  }
}








