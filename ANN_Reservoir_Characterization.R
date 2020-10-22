# Uncomment and run the line below to install package for this script to run if not already installed.
# You will be prompted to choose your preferred CRAN Mirror. Select the one closest to your current location.

# install required packages
install.packages("knitr") # install knitr
install.packages("rmarkdown") # install the rmarkdown package
install.packages("ggplot2") # install ggplot for plotting
install.packages("dplyr") # install dplyr for data manipulation
install.packages("neuralnet") # install neuralnet for artificial neural network
install.packages("NeuralNetTools")
install.packages("xfun") # install xfun for miscellaneous function

# load libraries
library(knitr)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(neuralnet)
library(NeuralNetTools)
library(xfun)

# call in dataset
set.seed(440)
data <- read.csv("Q4_data.csv", header=TRUE)

# summaries
summary(data)

# check that no data is missing
apply(data,2,function(x) sum(is.na(x)))

# data analysis
# train-test random splitting for linear model
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

# fitting linear model to the train dataset
lm.fit <- lm(Gas_prod~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit, test) # Predicted data from linear model fit
MSE.lm <- sum((pr.lm - test$Gas_prod)^2)/nrow(test) # Test MSE

# neural network fitting
# scaling data for the Neural Network
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
# train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]

# training the neural network
n <- names(train_)
f <- as.formula(paste("Gas_prod ~", paste(n[!n %in% "Gas_prod"], collapse = " + ")))
neural_net <- neuralnet(f, data = train_, hidden = c(5,3), linear.output = T)

# visual plot of the model
plot(neural_net)

# prediction
pr.nn <- compute(neural_net, test_[,1:14])
# results from neural networks are normalized (scaled)
# descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(data$Gas_prod)-min(data$Gas_prod))+min(data$Gas_prod)
test.r <- (test_$Gas_prod)*(max(data$Gas_prod)-min(data$Gas_prod))+min(data$Gas_prod)
# calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# compare the two Mean Squared Errors (MSEs)
print(paste(MSE.lm, MSE.nn))

# plot predictions
par(mfrow=c(1,2))
plot(test$Gas_prod, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend='NN', pch=18, col='red', bty='n')

plot(test$Gas_prod, pr.lm, col='blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend='LM', pch=18, col='blue', bty='n', cex=.95)

# compare predictions on the same plot
plot(test$Gas_prod, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
points(test$Gas_prod, pr.lm, col='blue', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend=c('NN','LM'), pch=18, col=c('red','blue'))

# neural net fitting
set.seed(440)
mydata <- read.csv("Q4_data.csv", header=TRUE)
# normalizing the dataset
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(mydata, normalize))

# training and test data
trainset <- maxmindf[1:312, ]
testset <- maxmindf[313:446, ]

# neural network training for (3,2) hidden layer configuration
library(neuralnet)
nn <- neuralnet(Gas_prod ~ Perf_Int + Frac_vol + Proppant + N.frac + 
                Tubing.depth + Casing.depth + FTP + Choke.Size + SITHP + 
                SG_gas + Well_Type + Latitude + Long. + Acid, data=trainset,
                hidden=c(3,2), linear.output=TRUE, threshold=0.01)
nn$result.matrix
# visual plot of the model
plot(nn)

#test the resulting output
temp_test <- subset(testset, select = c("Gas_prod", "Perf_Int", "Frac_vol", 
                                        "Proppant", "N.frac", "Tubing.depth", 
                                        "Casing.depth",  "FTP", "Choke.Size", 
                                        "SITHP", "SG_gas", "Well_Type", "Latitude", 
                                        "Long.", "Acid"))
head(temp_test)
nn.results <- compute(nn, temp_test)

#comparison of predicted to actual
results <- data.frame(actual = testset$Gas_prod, prediction = nn.results$net.result)
results

#testing the accuracy of the model
Gas_prod <- trainset$Gas_prod
predicted = results$prediction * abs(diff(range(Gas_prod))) + min(Gas_prod)
actual = results$actual * abs(diff(range(Gas_prod))) + min(Gas_prod)
comparison = data.frame(predicted, actual)
deviation = ((actual-predicted)/actual)
comparison = data.frame(predicted, actual, deviation)

# model accuracy
accuracy = 1-abs(mean(deviation))
accuracy
