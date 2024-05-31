diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

featureNames = colnames(diabetes) # list of feature names
print(featureNames)

# Raw Model No preprocessing

diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'DiffWalk', 'MentHlth','PhysHlth','GenHlth',
                'Sex','Age','Income','Education')
#Decision Tree
dt_base_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary) #, rules = TRUE)

summary(dt_base_model)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred_base <- predict(dt_base_model, newdata = test)

evaluation <- cbind(test, pred_base)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred_base,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 73.901% accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")




#Now Feature selection 
diabetes_f <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv',stringsAsFactors = F)

# 1 Step-wise regression

library(MASS)

fit <-lm(Diabetes_binary ~ . , data=diabetes_f)

step <- stepAIC(fit, direction = "both")

step$anova
 

# DT model after Removing NoDocbcCost & Smoker according to result of StepWise Regression


diabetes_fs1<- subset(diabetes_f, select =-c(NoDocbcCost, Smoker))

diabetes_fs1$Diabetes_binary <- as.factor(diabetes_fs1$Diabetes_binary)

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes_fs1))
train_idx <- sample(nrow(diabetes_fs1), size = sampling, replace = FALSE)
train <- diabetes_fs1[train_idx,] 
test <- diabetes_fs1[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare',
                'DiffWalk', 'MentHlth','PhysHlth','GenHlth',
                'Sex','Age','Income','Education')
#Decision Tree
dt_model1 <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary) #, rules = TRUE)

summary(dt_model1)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred1 <- predict(dt_model1, newdata = test)

evaluation1 <- cbind(test, pred1)
#head(evaluation)

evaluation1$correct <- ifelse(evaluation1$Diabetes_binary == evaluation1$pred1,1,0)
#head(evaluation)

accuracy <- sum(evaluation1$correct)/nrow(evaluation1) # accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n") #accuracy is 73.87%





# 2 Random Forest variable importance


library(party)

cf <- cforest(Diabetes_binary ~ . , data = diabetes_f, control=cforest_unbiased(mtry=3,ntree=50)) 

(varimp(cf)) # Top Features: GenHlth = 0.02, BMI = 0.014, HighBP = 0.011, Age = 0.01


diabetes_fs2<- subset(diabetes_f, select =-c(NoDocbcCost, Smoker))

diabetes_fs2$Diabetes_binary <- as.factor(diabetes_fs2$Diabetes_binary)

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes_fs2))
train_idx <- sample(nrow(diabetes_fs2), size = sampling, replace = FALSE)
train <- diabetes_fs2[train_idx,] 
test <- diabetes_fs2[-train_idx,]


predictors <- c('HighBP','BMI','GenHlth','Age')
#Decision Tree
dt_model2 <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary) #, rules = TRUE)

summary(dt_model2)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred2 <- predict(dt_model2, newdata = test)

evaluation2 <- cbind(test, pred2)
#head(evaluation)

evaluation2$correct <- ifelse(evaluation2$Diabetes_binary == evaluation2$pred2,1,0)
#head(evaluation)

accuracy <- sum(evaluation2$correct)/nrow(evaluation2) # accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n") #accuracy is 73.63%



# 3. Mutual Information Feature Selection
library(Rdimtools)


diabetes_data <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

diabetes_data.dat = as.matrix(diabetes_data[,2:22])
diabetes_data.lab = as.factor(diabetes_data[,1])

out1 = do.mifs(diabetes_data.dat , diabetes_data.lab, beta=0)
out2 = do.mifs(diabetes_data.dat , diabetes_data.lab, beta=0.5)
out3 = do.mifs(diabetes_data.dat , diabetes_data.lab, beta=1)

#Unable to install package


# 4. Lasso Regression

diabetes<- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

library(data.table) 
# used for data manipulation and joining
library(dplyr) 
# used for regression
library(glmnet)
# used for plotting 
library(ggplot2) 
# used for modeling
library(caret) 
# used for building XGBoost model
library(xgboost)  
# used for skewness
library(e1071)  
# used for combining multiple plots 
library(cowplot)    

breaks <- c(0, 5, 8, 13)
labels <- c("Adult", "Middle_Age", "Elderly")

# Discretize the Age column
diabetes$Age_discretized <- cut(diabetes$Age, breaks = breaks, labels = labels, include.lowest = TRUE)

Age_discretized_counts <- table(diabetes$Age_discretized)

barplot(Age_discretized_counts, 
        main="Bar Chart for Age_discretized Categories",
        xlab="Age_discret Category",
        ylab="Number of Vehicles")

# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7)
labels <- c("School", "College")

# Discretize the Education column
diabetes$Education_discretized <- cut(diabetes$Education, breaks = breaks, labels = labels, include.lowest = TRUE)


# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7, 9)
labels <- c("0_to_25k", "25k_to_50k", "50k_or_more")

# Discretize the Income column
diabetes$Income_discretized <- cut(diabetes$Income, breaks = breaks, labels = labels, include.lowest = TRUE)


#One Hot Encoding, the discretized columns

dummy_age <- dummyVars(~ Age_discretized, data = diabetes)
one_hot_encoded_age <- predict(dummy_age, newdata = diabetes)

# Perform one-hot encoding for Income_discretized
dummy_income <- dummyVars(~ Income_discretized, data = diabetes)
one_hot_encoded_income <- predict(dummy_income, newdata = diabetes)

# Perform one-hot encoding for Education_discretized
dummy_education <- dummyVars(~ Education_discretized, data = diabetes)
one_hot_encoded_education <- predict(dummy_education, newdata = diabetes)

diabetes_encoded <- cbind(diabetes, one_hot_encoded_age, one_hot_encoded_income, one_hot_encoded_education)

colnames(diabetes_encoded)

#Scaling & Centering

skewness(diabetes_encoded$BMI) 
skewness(diabetes_encoded$MentHlth)
skewness(diabetes_encoded$PhysHlth)
skewness(diabetes_encoded$GenHlth)


# log + 1 to avoid division by zero

diabetes_encoded$BMIlogtransformed <- log(diabetes_encoded$BMI+1)
diabetes_encoded$MentHlthlogtransformed <- log(diabetes_encoded$MentHlth+1)
diabetes_encoded$PhysHlthlogtransformed <- log(diabetes_encoded$PhysHlth+1)



# Scaling and Centering data

#Create Lasso Regression Model

sampling <- floor(0.8 * nrow(diabetes_encoded))
train_idx <- sample(nrow(diabetes_encoded), size = sampling, replace = FALSE)
train <- diabetes_encoded[train_idx,] 
test <- diabetes_encoded[-train_idx,]



set.seed(200)
control = trainControl(method ="cv", number = 5)
Grid_la_reg = expand.grid(alpha = 1,
                          lambda = seq(0.001, 0.1, by = 0.0002))



predictors <- c('HighBP','HighChol','CholCheck','Stroke','HeartDiseaseorAttack','PhysActivity',
                'Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','GenHlth','DiffWalk',
                'Sex','Age_discretized.Adult','Age_discretized.Middle_Age','Age_discretized.Elderly',
                'Income_discretized.0_to_25k','Income_discretized.25k_to_50k','Income_discretized.50k_or_more',
                'Education_discretized.School','Education_discretized.College','BMIlogtransformed','MentHlthlogtransformed',
                'PhysHlthlogtransformed')


# Training lasso regression model
lasso_model <- train(x = train[, predictors],
                    y = train$Diabetes_binary,
                    method = "glmnet",
                    trControl = control,
                    tuneGrid = Grid_la_reg)
lasso_model

library(pROC)


summary(glm(diabetes_encoded$Diabetes_binary ~ .,        #the dot includes all the variables 
            family=binomial,
            data=diabetes_encoded[,-1] ))   

dt_model <- (rpart(diabetes_encoded$Diabetes_binary ~ .,        #the dot includes all the variables 
            family=binomial,
            data=diabetes_encoded[,-1] ))   


X <-  model.matrix(diabetes_encoded$Diabetes_binary ~  ., 
                   data=diabetes_encoded[,-1])
Y <- diabetes_encoded[,"Diabetes_binary"]


cv.model<- cv.glmnet(x=X,y=Y, 
                     family = "binomial", 
                     alpha=1)
                                           #alpha=1 is lasso
cv.model<- cv.rpart(x=X,y=Y, 
                     family = "binomial", 
                     alpha=1)



plot(cv.model)

l.min <- cv.model$lambda.min

lasso.model <- glmnet(x=X,y=Y, 
                      family = "binomial", 
                      alpha=1, 
                      lambda = l.min)

lasso.model$beta


assess.glmnet(lasso.model, newx=X, newy = Y)

plot(roc.glmnet(lasso.model, 
                newx = X, 
                newy = Y ), 
     type="l") 

plot(cv.model$glmnet.fit, xvar="lambda")

# Lasso 2.0


data2 = data.matrix(diabetes_encoded)
Matrix(data2, sparse = TRUE)

split = sample(nrow(diabetes_encoded), floor(0.7*nrow(diabetes_encoded)))
train = diabetes_encoded[split,]
test = diabetes_encoded[-split,]

train_sparse = sparse.model.matrix(~., train[,2:36])
test_sparse = sparse.model.matrix(~., test[,2:36])

# Train the model
glmmod = glmnet(x=train_sparse, y=as.factor(train[,1]), alpha=1, family="binomial")
plot(glmmod, xvar="lambda")
glmmod
coef(glmmod)



cv.glmmod = cv.glmnet(x=train_sparse, y=as.factor(train[,1]), alpha=1, family="binomial")
plot(cv.glmmod)

lambda = cv.glmmod$lambda.1se # the value of lambda used by default
lambda

coefs = as.matrix(coef(cv.glmmod)) # convert to a matrix (618 by 1)
ix = which(abs(coefs[,1]) > 0)
length(ix)

coefs[ix,1, drop=FALSE]


test$cv.glmmod <- predict(cv.glmmod,newx=test_sparse,type='response')[,1]

########################

# Get optimal lambda
best.lambda <- cv.glmmod$lambda.min
best.lambda



# Predict the test set using the model
pred_lasso = predict(glmmod, test_sparse, type="response", s=best.lambda)
pred_lasso

# Apply a threshold
new_pred_lasso = ifelse(pred_lasso >= 0.5, 1, 0)
new_pred_lasso = data.frame(new_pred_lasso)
data_lasso = cbind(test[,2], new_pred_lasso)
names(data_lasso) = c("actual", "pred")
xtab_lasso = table(data_lasso$actual, data_lasso$pred)

cm_lasso = confusionMatrix(xtab_lasso)

overall_accuracy_lasso = cm_lasso$overall['Accuracy'] # 79.8% accuracy

# Get performance measures

diabetes_encoded$Diabetes_binary <- as.factor(diabetes_encoded$Diabetes_binary)

sampling <- floor(0.8 * nrow(diabetes_encoded))
train_idx <- sample(nrow(diabetes_encoded), size = sampling, replace = FALSE)
train <- diabetes_encoded[train_idx,] 
test <- diabetes_encoded[-train_idx,]



dt_lasso = C5.0(X=train, data = train[,2:36])

dt_lasso <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary)
summary(dt_lasso)






# Decision Tree
set.seed(225)
diabetes_encoded$Diabetes_binary <- as.factor(diabetes_encoded$Diabetes_binary)

sampling <- floor(0.8 * nrow(diabetes_encoded))
train_idx <- sample(nrow(diabetes_encoded), size = sampling, replace = FALSE)
train <- diabetes_encoded[train_idx,] 
test <- diabetes_encoded[-train_idx,]




predictors <- c('HighBP','HighChol','CholCheck','BMIlogtransformed','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','GenHlth','MentHlth',
                'PhysHlth','Age','Income','Education','Age_discretized','Education_discretized','Income_discretized',
                'BMIlogtransformed',
               'MentHlthlogtransformed','PhysHlthlogtransformed','DiffWalk',
                'Sex','Age_discretized.Adult','Age_discretized.Middle_Age','Age_discretized.Elderly',
               'Income_discretized.0_to_25k','Income_discretized.25k_to_50k','Income_discretized.50k_or_more',
               'Education_discretized.School','Education_discretized.College')


predictors1 <- c('HighBP','HighChol','CholCheck','BMI','Stroke','HeartDiseaseorAttack','Veggies',
                 'HvyAlcoholConsump','GenHlth','MentHlth','PhysHlth','DiffWalk','Sex','Age',
                 'Income','Age_discretized.Middle_Age','Age_discretized.Elderly','Education_discretized.College',
                 'Income_discretized.50k_or_more')

#Decision Tree
dt_model <- C5.0.default(x = train[,predictors1], y = train$Diabetes_binary, trials = 100)

summary(dt_model)

pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # accuracy of decision tree

cat("Testing Accuracy is ", accuracy ,"\n")

confusion_matrix <- data.frame(predicted = rf_pred, actual =test[,'Diabetes_binary'])
table(confusion_matrix)

TP <- 5561  #confusion_matrix[2, 2]  # True Positives
FP <- 1966#confusion_matrix[2, 1]  # False Positives
FN <- 1622   #confusion_matrix[1, 2]  # False Negatives


# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)

# Print Precision and Recall
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

F1 <-  2 * (precision * recall)/(precision + recall)

cat("F1 Score:", F1, "\n")



#Loop for storing results
# Initialize an empty data frame to store evaluation metrics
evaluation_table <- data.frame(Seed = integer(),
                               Accuracy = double(),
                               Precision = double(),
                               Recall = double(),
                               F1_Score = double())

# Seeds to iterate over
seeds <- c(205, 215, 225, 235, 245, 255 ,265, 275, 285, 295)

# Loop through each seed
for (seed in seeds) {
  # Set the seed
  set.seed(seed)
  
  # Split the data into train and test sets
  sampling <- floor(0.8 * nrow(diabetes_encoded))
  train_idx <- sample(nrow(diabetes_encoded), size = sampling, replace = FALSE)
  train <- diabetes_encoded[train_idx,] 
  test <- diabetes_encoded[-train_idx,]
  
  # Train the model
  #dt_model <- C5.0.default(x = train[,predictors1], y = train$Diabetes_binary, trials = 100)
  rf_model <- randomForest(x = train[,predictors1], y = train$Diabetes_binary)
  # Make predictions on the test set
  pred <- predict(rf_model, newdata = test)
  
  # Calculate evaluation metrics
  cm <- confusionMatrix(data = pred, reference = test$Diabetes_binary)
  accuracy <- cm$overall['Accuracy']
  precision <- cm$byClass['Precision']
  recall <- cm$byClass['Recall']
  f1_score <- cm$byClass['F1']
  
  # Store evaluation metrics in the evaluation table
  evaluation_table <- rbind(evaluation_table, 
                            data.frame(Seed = seed,
                                       Accuracy = accuracy,
                                       Precision = precision,
                                       Recall = recall,
                                       F1_Score = f1_score))
}

# Print the evaluation table
print(evaluation_table)




























#Predict on the training data
train_pred <- predict(dt_model, newdata = train)

# Evaluate training accuracy
train_evaluation <- cbind(train, train_pred)
train_evaluation$correct <- ifelse(train_evaluation$Diabetes_binary == train_evaluation$train_pred, 1, 0)
train_accuracy <- sum(train_evaluation$correct) / nrow(train_evaluation)

cat("Training Accuracy is ", train_accuracy, "\n")

predictors1 <- c('HighBP','HighChol','CholCheck','Stroke','GenHlth','HeartDiseaseorAttack',
                 'BMIlogtransformed','Sex','Age_discretized.Elderly')
rf_model <- randomForest(x = train[,predictors1], y = train$Diabetes_binary)

#summary(rf_model)
#plot(rf_model)


rf_pred <- predict(rf_model, newdata = test)

evaluation <- cbind(test, rf_pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$rf_pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) #74.94% accuracy of random forest
cat("Testing Accuracy is ", accuracy ,"\n")

confusion_matrix <- data.frame(predicted = rf_pred, actual =test[,'Diabetes_binary'])
table(confusion_matrix)

TP <- 5529  #confusion_matrix[2, 2]  # True Positives
FP <- 1998#confusion_matrix[2, 1]  # False Positives
FN <- 1512   #confusion_matrix[1, 2]  # False Negatives


# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)

# Print Precision and Recall
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

F1 <-  2 * (precision * recall)/(precision + recall)

cat("F1 Score:", F1, "\n")


TPR <- sum(evaluation$rf_pred == '1' & evaluation$Diabetes_binary == 0) / sum(evaluation$Diabetes_binary == 0) 

cat("TPR:",TPR,"\n")

TNR <- sum(evaluation$rf_pred == '0' & evaluation$Diabetes_binary == 1) / sum(evaluation$Diabetes_binary == 1) 

cat("TNR:",TPR,"\n")

FPR <- sum(evaluation$rf_pred == '1' & evaluation$Diabetes_binary == '1') / sum(evaluation$Diabetes_binary == '1') 

cat("FPR:",TPR,"\n")



#ROC

###   Validate test dataset
evaluation <- test
evaluation$prob <- predict(rf_model, newdata = evaluation, type = "response")


count_no_diabetes <- nrow(subset(diabetes_encoded, diabetes_encoded$Diabetes_binary == 0) )
baseline <- count_no_diabetes / nrow(diabetes_encoded)
baseline


g <- roc(evaluation$Diabetes_binary ~ evaluation$prob, data = evaluation)
#   ROC curve
plot(g)

# K fold cross validation

diabetes_select <- select(diabetes_encoded,'HighBP','HighChol','CholCheck','Stroke','GenHlth','HeartDiseaseorAttack',
                          'BMIlogtransformed','Sex','Age_discretized.Elderly','Diabetes_binary')

sampling <- floor(0.8 * nrow(diabetes_select))
train_idx <- sample(nrow(diabetes_select), size = sampling, replace = FALSE)
train <- diabetes_select[train_idx,] 
test <- diabetes_select[-train_idx,]


set.seed(123)

# define training control which
# generates parameters that further
# control how models are created
train_control <- trainControl(method = "cv", 
                              number = 10)

model <- train(Diabetes_binary~., data = train, 
               trControl = train_control, 
               method = "rpart")

print(model)


# Preprocessing

# Step 1: Removing Outliers; Winsorization Technique

diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

featureNames = colnames(diabetes) # list of feature names
print(featureNames)


winsorize <- function(x, trim = 0.2) {
  q <- quantile(x, c(0, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

diabetes$BMIWinsorized <- winsorize(diabetes$BMI)
diabetes$MentHlth_winsorized = winsorize(diabetes$MentHlth)
diabetes$PhysHlth_winsorized = winsorize(diabetes$PhysHlth)


diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes_select))
train_idx <- sample(nrow(diabetes_select), size = sampling, replace = FALSE)
train <- diabetes_select[train_idx,] 
test <- diabetes_select[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk', 
                'MentHlth','PhysHlth','GenHlth',
                'Sex','Age','Income','Education','MentHlth_winsorized','PhysHlth_winsorized',
                'BMIWinsorized')
#Decision Tree
dt_model <- C5.0(x = train[,predictors], y = train$Diabetes_binary, trials = 10, C5.0Control=c(min_n= 5 , earlyStopping=FALSE)) #, rules = TRUE)

C5.0.

summary(dt_model)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred1 <- predict(dt_model, newdata = test)

evaluation1 <- cbind(test, pred1)
#head(evaluation)

evaluation1$correct <- ifelse(evaluation1$Diabetes_binary == evaluation1$pred1,1,0)
#head(evaluation)

accuracy <- sum(evaluation1$correct)/nrow(evaluation1) # accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")

# Using Random Forest

library(randomForest)


rf_model <- randomForest(x = train[,predictors1], y = train$Diabetes_binary)

#summary(rf_model)
#plot(rf_model)


rf_pred <- predict(rf_model, newdata = test)

evaluation <- cbind(test, rf_pred)
head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$rf_pred,1,0)
head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) #74.94% accuracy of random forest
cat("Testing Accuracy is ", accuracy ,"\n")

# Bagging using Rpart
library(ipred)
library(rpart)
bag <- bagging( 
  formula = Diabetes_binary ~ ., 
  data = diabetes_fs1, 
  nbagg = 50,    
  coob = TRUE, 
  control = rpart.control(minsplit = 2, cp = 0,  
                          min_depth=2) 
) 

#summary(bag_pred)
#plot(bag)

bag_pred <- predict(bag, newdata = test)

evaluation <- cbind(test, bag_pred)
head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$bag_pred,1,0)
head(evaluation)

accuracy = sum(evaluation$correct)/nrow(evaluation) #accuracy 98.84%

cat("Testing Accuracy is ", accuracy ,"\n")

confusion_matrix <- data.frame(predicted = bag_pred, actual =test[,'Diabetes_binary'])
table(confusion_matrix)

TP <- 6936  #confusion_matrix[2, 2]  # True Positives
FP <- 58#confusion_matrix[2, 1]  # False Positives
FN <- 106   #confusion_matrix[1, 2]  # False Negatives


# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)

# Print Precision and Recall
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

F1 <-  2 * (precision * recall)/(precision + recall)

cat("F1 Score:", F1, "\n")



# Step 2: Feature discretization

diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')


featureNames = colnames(diabetes) # list of feature names
print(featureNames)


winsorize <- function(x, trim = 0.01) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

diabetes$BMIWinsorized <- winsorize(diabetes$BMI)
diabetes$MentHlth_winsorized = winsorize(diabetes$MentHlth)
diabetes$PhysHlth_winsorized = winsorize(diabetes$PhysHlth)

breaks <- c(0, 5, 8, 13)
labels <- c("Adult", "Middle_Age", "Elderly")

# Discretize the Age column
diabetes$Age_discretized <- cut(diabetes$Age, breaks = breaks, labels = labels, include.lowest = TRUE)

Age_discretized_counts <- table(diabetes$Age_discretized)

barplot(Age_discretized_counts, 
        main="Bar Chart for Age_discretized Categories",
        xlab="Age_discret Category",
        ylab="Number of Vehicles")

# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7)
labels <- c("School", "College")

# Discretize the Education column
diabetes$Education_discretized <- cut(diabetes$Education, breaks = breaks, labels = labels, include.lowest = TRUE)


# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7, 9)
labels <- c("0_to_25k", "25k_to_50k", "50k_or_more")

# Discretize the Income column
diabetes$Income_discretized <- cut(diabetes$Income, breaks = breaks, labels = labels, include.lowest = TRUE)

set.seed(200)
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]



#colnames(diabetes)
#Accuracy 86.73%, trials = 3/5 = 86.58%
predictors <- c('HighBP','HighChol','CholCheck','BMIWinsorized','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare',
                'GenHlth','MentHlth_winsorized','PhysHlth_winsorized','DiffWalk',
                'Sex','Age_discretized','Education_discretized','Income_discretized')


#Decision Tree
dt_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary)

pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # accuracy of decision tree

cat("Testing Accuracy is ", accuracy ,"\n")

#Predict on the training data
train_pred <- predict(dt_model, newdata = train)

# Evaluate training accuracy
train_evaluation <- cbind(train, train_pred)
train_evaluation$correct <- ifelse(train_evaluation$Diabetes_binary == train_evaluation$train_pred, 1, 0)
train_accuracy <- sum(train_evaluation$correct) / nrow(train_evaluation)

cat("Training Accuracy is ", train_accuracy, "\n")





# Scaling Features, log transform features which are non-binary features
#BMI, GenHlth, Age

diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')
diabetes$BMIlogtransformed <- log(diabetes$BMI)
diabetes$GenHlthlogtransformed <- log(diabetes$GenHlth)
diabetes$Agelogtransformed <- log(diabetes$Age)
diabetes$Educationlogtransformed <- log(diabetes$Education)
diabetes$Incomelogtransformed <- log(diabetes$Income)


summary(diabetes)

winsorize <- function(x, trim = 0.2) {
  q <- quantile(x, c(0, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

diabetes$MentHlth_winsorized = winsorize(diabetes$MentHlth)
diabetes$PhysHlth_winsorized = winsorize(diabetes$PhysHlth)


featureNames = colnames(diabetes) # list of feature names
print(featureNames)


set.seed(200)
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]



#Accuracy 86.73%, trials = 3/5 = 86.58%
predictors <- c('HighBP','HighChol','CholCheck','BMIlogtransformed','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare',
                'GenHlthlogtransformed','MentHlth_winsorized','PhysHlth_winsorized','DiffWalk',
                'Sex','Agelogtransformed','Educationlogtransformed','Incomelogtransformed')


predictors1 <- c('BMI', 'HighBP','HvyAlcoholConsump','HeartDiseaseorAttack')

#Decision Tree
dt_model <- C5.0.default(x = train[,predictors1], y = train$Diabetes_binary)

pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # accuracy of decision tree

cat("Testing Accuracy is ", accuracy ,"\n")

#Predict on the training data
train_pred <- predict(dt_model, newdata = train)

# Evaluate training accuracy
train_evaluation <- cbind(train, train_pred)
train_evaluation$correct <- ifelse(train_evaluation$Diabetes_binary == train_evaluation$train_pred, 1, 0)
train_accuracy <- sum(train_evaluation$correct) / nrow(train_evaluation)

cat("Training Accuracy is ", train_accuracy, "\n")


# Reference Lasso Regression: https://spectdata.com/index.php/2019/08/08/variable-selection-using-lasso/
#https://bookdown.org/tpinto_home/Regularisation/lasso-regression.html
