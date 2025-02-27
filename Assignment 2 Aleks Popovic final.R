#libraries
library(tidyverse)
library(caTools)
library(ggplot2)


View(admit)

#Exploratory Data Analysis. 

# Convert 'am' to a factor (categorical variable)
admit$admit <- as.factor(admit$admit)

# View structure of dataset
str(admit)

# Summary statistics
summary(admit)

#Are the classes (admit/ don't admit) balanced in the data set?
# It is not balanced as there are 127 admits and 273 non admits

#How would you describe the distribution of GRE scores? Is it skewed or approximately normal?
# It is technically slightly right skewed as the mean is higher then the median 
#although it is only by 7 meaning you could say it is approximately normal


# Split the data into training and testing sets.
set.seed(1)

#Split the dataset into training (70%) and testing (30%)
split <- sample.split(admit$admit, SplitRatio = 0.7)

#training and testing sets
train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)

# Check dimensions
dim(train_data)
dim(test_data)


#Fit the Logistic Regression Model

# Train the logistic regression model
log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial) 

summary(log_model)

#Our intercept is -3.09, every 1 increase in GRE leads to a 0.0008 increase in the 
#probability of admission, every 1 increase in GPA leads to a 0.9 increase in the probability of admission
#every 1 class rank leads to a -0.5 decrease int he probability of admission

# GPA and Rank are the only significant variables, GPA is significant at the 5% level
# While Rank is significant at the 0.1% level. Our intercept is -3.09 

# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs
# Convert probabilities to binary predictions (threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))

#Evaluate model performance

# Create confusion matrix
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

#Interpretation:
# for our test data we have 77 true negatives we have 9 true positives
#We have 29 false negatives and 5 false positives

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
#0.71
#Recall
recall<-77/106
recall
#0.72
#Precision
precision<-77/82
precision
#0.94
# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Rank is the most important variable for predicting admission as it is has the highest
#level of significance at the 0.1% level

# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admission",
       x = "Graduate Record Exam scores (GRE)",
       y = "Admission (0 = Rejection, 1 = admission)") +
  scale_color_manual(values = c("gold", "green"), name = "Prediction")

