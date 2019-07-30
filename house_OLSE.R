
# 1. DETECTION AND TRIMMING OF RESIDUALS ----------------------------------

upperOutliers = as.vector(which(residuals(pricePred)>median(residuals(pricePred))+4*stdev)) # Detecting upper residuals
lowerOutliers = as.vector(which(residuals(pricePred)<median(residuals(pricePred))-4*stdev)) # Detection lower residuals
allOutliers = c(lowerOutliers,upperOutliers) # Combining residuals into a single vector
housingData_trimmed = housingData_dv[-allOutliers,] # Trimming the outliers from the dataset


# 2. FITTING MODEL (& creating training and test indices) --------------------------------------------------------

# Creating updated training and test samples (necessary because of ommissions of data

library(caTools) # Used for sampling process

set.seed(4243)
split = sample.split(housingData_trimmed[,4],SplitRatio = 0.6) # Boolean with 60/40 split
trainSet = subset(housingData_trimmed,split==TRUE) # Subsets dataset where boolean is true
testSet = subset(housingData_trimmed,split==FALSE)
testSet = rbind(testSet,housingData_trimmed[allOutliers,]) # Add omitted observations back


# priceLM_trimmed = lm(log(price)~.-sqft_lot15,data=housingData_trimmed[train_t,]) # Fitting linear model to training data
# summary(priceLM_trimmed)
priceLM_trimmed = lm(price~.-sqft_lot15,data=trainSet) #log price -sqft_lot15
summary(priceLM_trimmed)

# Basic evaluations of var inflation, residual plots
vif(priceLM_trimmed)
plot(residuals(priceLM_trimmed))
qqnorm(residuals(priceLM_trimmed),ylim=c(-4,4))
qqline(residuals(priceLM_trimmed),col="red")
hist(residuals(priceLM_trimmed),xlab="Residual",main="Histogram of Residual Distribution")

priceLM_trimmed.pred = predict(priceLM_trimmed,newdata=testSet) # predict the test set values
# priceLM_trimmed.pred = exp(priceLM_trimmed.pred)

sqrt(mean((as.vector(priceLM_trimmed.pred)-testSet[,1])^2)) # RMSE
mean(abs(as.vector(priceLM_trimmed.pred)-testSet[,1])) # Average deviation from true price
median(abs(as.vector(priceLM_trimmed.pred)-testSet[,1])) 


# average price for various bedrooms
mean(price[which(bedrooms==8)]) # Highest average prices

# 3. SUBSET SELECTION -----------------------------------------------------

# Backward elimination using BIC:
priceLM_trimmed.back=step(priceLM_trimmed,direction="backward",steps=500,trace=10,k=log(length(housingData_trimmed[,4])))

priceLM_trimmed.back.pred = predict(priceLM_trimmed.back,newdata=testSet)
priceLM_trimmed.back.pred = exp(priceLM_trimmed.back.pred)

sqrt(mean((as.vector(priceLM_trimmed.back.pred)-testSet[,1])^2)) # RMSE
mean(abs(as.vector(priceLM_trimmed.back.pred)-testSet[,1])) # Average deviation from true price
median(abs(as.vector(priceLM_trimmed.back.pred)-testSet[,1]))

# Forward elimination using BIC:
priceLM_trimmed.for=step(lm(price~grade,data=housingData_trimmed),scope=formula(lm(price~.,data=housingData_trimmed)),
                         direction="forward",steps=5,trace=10,k=log(length(housingData_trimmed[,4])))

priceLM_trimmed.for.pred = predict(priceLM_trimmed.for,newdata=testSet)
sqrt(mean((as.vector(priceLM_trimmed.for.pred)-testSet[,1])^2)) # RMSE
mean(abs(as.vector(priceLM_trimmed.for.pred)-testSet[,1])) # Average deviation from true price


# Logarithmic Transformation ----------------------------------------------

priceLM_trimmed.log = lm(log(price)~.-sqft_lot15,data=trainSet) #log price -sqft_lot15
summary(priceLM_trimmed.log)

priceLM_trimmed.log.pred = predict(priceLM_trimmed.log,newdata=testSet)
priceLM_trimmed.log.pred = exp(priceLM_trimmed.log.pred)

sqrt(mean((as.vector(priceLM_trimmed.log.pred)-testSet[,1])^2)) # RMSE
mean(abs(as.vector(priceLM_trimmed.log.pred)-testSet[,1])) # Average deviation from true price
median(abs(as.vector(priceLM_trimmed.log.pred)-testSet[,1]))
