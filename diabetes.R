# Data Description: https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf
# Age group classification: https://www.researchgate.net/figure/Age-group-comparison-between-Young-adult-18-25-age-Adult-26-44-age-Middle-age_tbl1_338842581#:~:text=Figure-,Age%20group%20comparison%20between%20Young%20adult%20(18%2D25%20age),age)%20about%20four%20sequential%20patterns.&text=Background%20Working%20memory%20updating%20(WMU,crucial%20for%20cognitive%20executive%20function.


# Before Preprocessing


diabetes <- read.csv('diabetes_binary_health_indicators_BRFSS2015.csv')

totalNum  = nrow(diabetes) # number of rows

emptyNum = nrow(diabetes[!complete.cases(diabetes),]) # number of missing rows

featureNames = colnames(diabetes) # list of feature names
print(featureNames)

class(diabetes)

head(diabetes, 10) #high level overview of data

# BMI, GenHlth, MentHlth, PhysHlth, Age, Education, Income are non-binary columns

# HighBP, HighChol, CholCheck, Smoker, Stroke, HeartDiseaseorAttack, PhysActivity
# Fruits, Veggies, HvyAlcholConsump, AnyHealthcare, NoDocbcCost, DiffWalk, Sex
# Diabetes_binary are binary columns

library(calibrate)
#> Loading required package: MASS
library(ggplot2)
library(corrplot)
#> corrplot 0.92 loaded


#Correlation plot

R <- cor(diabetes)

corrplot(R, method="circle",type="lower")

summary(diabetes) #descriptive statistics of attributes


diabetes$HighBP = factor(diabetes$HighBP, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$HighChol = factor(diabetes$HighChol, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$CholCheck = factor(diabetes$CholCheck, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$Smoker = factor(diabetes$Smoker, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$Stroke = factor(diabetes$Stroke, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$HeartDiseaseorAttack = factor(diabetes$HeartDiseaseorAttack, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$PhysActivity = factor(diabetes$PhysActivity, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$Fruits = factor(diabetes$Fruits, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$Veggies = factor(diabetes$Veggies, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$HvyAlcoholConsump = factor(diabetes$HvyAlcoholConsump, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$AnyHealthcare = factor(diabetes$AnyHealthcare, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$NoDocbcCost = factor(diabetes$NoDocbcCost, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$DiffWalk = factor(diabetes$DiffWalk, levels=c(0, 1), labels=c("No", "Yes"))
diabetes$Sex = factor(diabetes$Sex, levels=c(0, 1), labels=c("Female", "Male"))
diabetes$Diabetes_binary = factor(diabetes$Diabetes_binary, levels=c(0, 1), labels=c("No", "Yes"))

hist(diabetes$BMI, #Histogram of BMI
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of BMI",      # title of the histogram
     xlab="BMI",      # label of x axis
     ylab="Number of Rows")      # label of y axis

boxplot(BMI ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "BMI Boxplot",       
        ylab = "BMI",                  
        xlab = "Diabetes" ) 

#BMI column is a right skewed data and has many outliers.

library(dplyr)

# Function to perform winsorization
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

# Winsorize the BMI column
diabetes$BMIWinsorized <- winsorize(diabetes$BMI)

# Checking the statistics before and after winsorization
summary(diabetes$BMI)
summary(diabetes$BMIWinsorized)

hist(diabetes$BMIWinsorized, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of BMIWinsorized",      # title of the histogram
     xlab="BMI",      # label of x axis
     ylab="Number of Rows")   

boxplot(BMIWinsorized ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "BMIWinsorized Boxplot",       
        ylab = "BMI_winsorized",                  
        xlab = "Diabetes" ) 


# Log Transform to handle righ-skew data

# Log transform the BMI_winsorized column
diabetes$BMIlogtransformed <- log(diabetes$BMIWinsorized)

# Checking the summary statistics of the log-transformed BMI column
summary(diabetes$BMIlogtransformed)

hist(diabetes$BMIlogtransformed, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of BMIlogtransformed",      # title of the histogram
     xlab="BMIlogtransformed",      # label of x axis
     ylab="Number of Rows")   


hist(diabetes$GenHlth, #Histogram of GenHlth
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of GenHlth",      # title of the histogram
     xlab="GenHlth",      # label of x axis
     ylab="Number of Rows")   # From graph, its normally distributed

hist(diabetes$MentHlth, #Histogram of MentHlth
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of MentHlth",      # title of the histogram
     xlab="MenHlth",      # label of x axis
     ylab="Number of Rows") 

boxplot(MentHlth ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "MentHlth outlier",       
        ylab = "MentHtlh",                  
        xlab = "Diabetes" ) 

#MentHlth column is a right skewed data and has many outliers.

# Winsorize the MentHlth column
diabetes$MentHlthWinsorized <- winsorize(diabetes$MentHlth)

# Checking the statistics before and after winsorization
summary(diabetes$MentHlth)
summary(diabetes$MentHlthWinsorized)

hist(diabetes$MentHlthWinsorized, #Histogram of MentHlth_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of MentHlthWinsorized",      # title of the histogram
     xlab="MentHlth_winsorized",      # label of x axis
     ylab="Number of Rows")   

boxplot(MentHlthWinsorized ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "MentHlthWinsorized Boxplot",       
        xlab = "Diabetes",                  
        ylab = "MentHlth_winsorized" ) 



hist(diabetes$MentHlthWinsorized, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of MentHlth_winsorized",      # title of the histogram
     xlab="MentHlth_winsorized",      # label of x axis
     ylab="Number of Rows")   

hist(diabetes$PhysHlth, #Histogram of PhysHlth
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of PhysHlth",      # title of the histogram
     xlab="MenHlth",      # label of x axis
     ylab="Number of Rows") 

boxplot(PhysHlth ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "PhysHlth outlier",       
        ylab = "PhysHlth",                  
        xlab = "Diabetes" ) 

#PhysHlth column is a right skewed data and has many outliers.

# Winsorize the PhysHlth column
diabetes$PhysHlthWinsorized <- winsorize(diabetes$PhysHlth)

# Checking the statistics before and after winsorization
summary(diabetes$PhysHlth)
summary(diabetes$PhysHlthWinsorized)

hist(diabetes$PhysHlthWinsorized, #Histogram of PhysHlth_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of PhysHlthwinsorized",      # title of the histogram
     xlab="PhysHlthWinsorized",      # label of x axis
     ylab="Number of Rows")   

boxplot(PhysHlthWinsorized ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "PhysHlthwinsorized BoxPlot",       
        xlab = "Diabetes",                  
        ylab = "PhysHlth_winsorized" ) 
# No change after winsorization

# Log Transform to handle righ-skew data

#histogram of Age
hist(diabetes$Age, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of Age",      # title of the histogram
     xlab="Age",      # label of x axis
     ylab="Number of Rows")


# Discretization for Age

# Defining the breaks and labels for Age discretization
breaks <- c(0, 1, 5, 8, 13)
labels <- c("Young_Adult", "Adult", "Middle_Age", "Elderly")

# Discretize the Age column
diabetes$Age_discretized <- cut(diabetes$Age, breaks = breaks, labels = labels, include.lowest = TRUE)

# Check the summary of the discretized Age column
summary(diabetes$Age_discretized)

#histogram of Age_discretized
Age_discretized_counts <- table(diabetes$Age_discretized)

barplot(Age_discretized_counts, 
        main="Bar Chart for Age_discretized Categories",
        xlab="Age_discret Category",
        ylab="Number of Vehicles")


#histogram of Education
hist(diabetes$Education, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of Education",      # title of the histogram
     xlab="Education",      # label of x axis
     ylab="Number of Rows")


# Discretization for Education

# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7)
labels <- c("School", "College")

# Discretize the Education column
diabetes$Education_discretized <- cut(diabetes$Education, breaks = breaks, labels = labels, include.lowest = TRUE)



Education_discretized_counts <- table(diabetes$Education_discretized)

barplot(Education_discretized_counts, 
        main="Bar Chart for Education_discretized Categories",
        xlab="Education_discretized Category",
        ylab="Number of rows")



# Check the summary of the discretized Age column
summary(diabetes$Education_discretized)

#histogram of Income
hist(diabetes$Income, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of Income",      # title of the histogram
     xlab="Income",      # label of x axis
     ylab="Number of Rows")

# Discretization for Income

# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7, 9)
labels <- c("0_to_25k", "25k_to_50k", "50k_or_more")

# Discretize the Income column
diabetes$Income_discretized <- cut(diabetes$Income, breaks = breaks, labels = labels, include.lowest = TRUE)


Income_discretized_counts <- table(diabetes$Income_discretized)

barplot(Income_discretized_counts, 
        main="Bar Chart for Income_discretized Categories",
        xlab="Income_discretized Category",
        ylab="Number of rows")

# Check the summary of the discretized Age column
summary(diabetes$Income_discretized)


#plot Diabetes_binary column

diabetes_counts <- table(diabetes$Diabetes_binary)
barplot(diabetes_counts, 
        main = "Proportion of Yes and No in diabetes_binary Column",
        xlab = "Diabetes Binary",
        ylab = "Frequency")
        #col = "skyblue")


#plot for age_discretized column

age_counts <- table(diabetes$Age_discretized)
barplot(age_counts, 
        main = "Proportion of Age in Age_discretized Column",
        xlab = "Categories of Age",
        ylab = "Frequency",
        col = "skyblue")


#plot for Income_discretized column

income_counts <- table(diabetes$Income_discretized)
barplot(income_counts, 
        main = "Proportion of Income in Income_discretized Column",
        xlab = "Categories of Income",
        ylab = "Frequency",
        col = "skyblue")

#plot for Education_discretized column

education_counts <- table(diabetes$Education_discretized)
barplot(education_counts, 
        main = "Proportion of Education in Education_discretized Column",
        xlab = "Categories of Education",
        ylab = "Frequency",
        col = "skyblue")



# ML Models packages
install.packages("C50") #macos binary package error resolved using: Sys.setenv(PATH = paste("/usr/bin:/bin:/usr/sbin:/sbin", Sys.getenv("PATH"), sep=":")) 
library(C50)
install.packages('neuralnet')
library(caret)

# Train/Test Split

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]

#colnames(diabetes)
#Accuracy 86.73%, trials = 3/5 = 86.58%
predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'GenHlth','BMIlogtransformed','MentHlthWinsorized','PhysHlthWinsorized','DiffWalk',
                'Sex','Age_discretized','Education_discretized','Income_discretized')

#Remocing correlated: HighBP, Age_discretized,PhysHlthWinsorized, accuracy is 86.90%  
# Removing BMI, accuracy us 86.74%
predictors1 <- c( 'HighBP','HighChol','CholCheck','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'GenHlth','BMIlogtransformed','MentHlthWinsorized','DiffWalk',
                'Sex','Age_discretized','Income_discretized','PhysHlthWinsorized')

#Decision Tree
dt_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary, trials = 5)

summary(dt_model)
plot(dt_model)

plot(dt_model, trial =0 )


pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 86.51 accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")


confusion_matrix <- data.frame(predicted = pred, actual =test[,'Diabetes_binary'])
table(confusion_matrix)

TP <- 887  #confusion_matrix[2, 2]  # True Positives
FP <- 670#confusion_matrix[2, 1]  # False Positives
FN <- 6304   #confusion_matrix[1, 2]  # False Negatives


# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)

# Print Precision and Recall
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

F1 <-  2 * (precision * recall)/(precision + recall)

cat("F1 Score:", F1, "\n")

# Define the parameter grid for tuning
param_grid <- expand.grid(
  trials = c(1, 5, 10, 20),   # Number of boosting iterations
  model = "tree",              # Indicate C5.0 decision tree model
  winnow = c(TRUE, FALSE),     # Whether to use winnowing (feature selection)
  rules = c(FALSE, TRUE)       # Whether to use rules
)



library(caret)

# Perform grid search with cross-validation
tuned_model <- train(
  Diabetes_binary ~ .,        # Formula for the target variable and predictors
  data = train,               # Training data
  method = "C5.0",            # Specify C5.0 decision tree method
  tuneGrid = param_grid,      # Parameter grid for tuning
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross-validation
  metric = "Accuracy"         # Performance metric to optimize
)

# Print the tuned model
print(tuned_model)

# Extract the best model
best_model <- tuned_model$finalModel






# Random Forest
install.packages("randomForest")
library(randomForest)


rf_model <- randomForest(x = train[,predictors], y = train$Diabetes_binary)

summary(rf_model)
plot(rf_model)


rf_pred <- predict(rf_model, newdata = test)

evaluation <- cbind(test, rf_pred)
head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$rf_pred,1,0)
head(evaluation)

sum(evaluation$correct)/nrow(evaluation) #86.49 accuracy of random forest

# Naive Bayes
library(e1071)

diabetes_1 <- subset(diabetes, select = -c(MentHlth, PhysHlth, Age, Education, Income, Sex, BMI, Fruits, Smoker, HeartDiseaseorAttack))


sampling <- floor(0.7 * nrow(diabetes_1))
train_idx <- sample(nrow(diabetes_1), size = sampling, replace = FALSE)
train <- diabetes_1[train_idx,] 
test <- diabetes_1[-train_idx,]
diabetes_1.model <- naiveBayes(as.factor(Diabetes_binary) ~ . , data = train)
diabetes_1.predict <- predict(diabetes_1.model, test, type ='class') #running model on test data
confusion_matrix <- data.frame(predicted = diabetes_1.predict, actual =test[,'Diabetes_binary'])
table(confusion_matrix)

correct_predictions <- sum(confusion_matrix$predicted == confusion_matrix$actual)
total_predictions <- nrow(confusion_matrix)
accuracy <- correct_predictions / total_predictions
print(paste("Naive Bayes Overall Prediction Accuracy:", accuracy)) 
# Accuracy of Naive Bayes 74.57%

# undersampling and oversampling for handling imbalanced dataset

library(ROSE)

diabetes_bal <- ovun.sample(Diabetes_binary~., data=diabetes,
                                N= 30000, p =0.5 ,
                                seed=1, method="both")$data

nrow(diabetes_bal)

diabetes_counts <- table(diabetes_bal$Diabetes_binary)
barplot(diabetes_counts, 
        main = "Proportion of Yes and No in diabetes_binary Column after resampling",
        xlab = "Diabetes Binary",
        ylab = "Frequency")
        #col = "skyblue")


head(diabetes_bal)
class(diabetes_bal)

sampling <- floor(0.7 * nrow(diabetes_bal))
train_idx <- sample(nrow(diabetes_bal), size = sampling, replace = FALSE)
train <- diabetes_bal[train_idx,] 
test <- diabetes_bal[-train_idx,]

predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'GenHlth','BMIlogtransformed','MentHlthWinsorized','PhysHlthWinsorized','DiffWalk',
                'Sex','Age_discretized','Education_discretized','Income_discretized')


#Decision Tree
dt_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary)

summary(dt_model)
plot(dt_model)


pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
head(evaluation)

sum(evaluation$correct)/nrow(evaluation) 
# 74.46 accuracy on oversampling & undersampling with p = 0.5
# 79.31 accuracy on oversampling & undersampling  with p=0.7
# 75.42 accuracy on oversampling & undersampling with p = 0.3


#Result of all the models: 
#Decision Tree 86.51% accuracy, 
#Random Forest 86.49% accuracy,
#Naive Bayes.  74.57% accuracy,

# Decision Tree balanced oversampling & undersampling with p = 0.5, 74.46%
# Decision Tree balanced oversampling & undersampling with p = 0.3, 75.42%
# Decision Tree balanced oversampling & undersampling with p = 0.7, 79.31%
# Decision Tree balanced undersampling, 86.28%
# Decision Tree balanced oversamplign, 86.41%


#Neural Network

diabetes_data <- data.frame(Age_discretized = c('Young_Adult', 'Adult', 'Middle_Age', 'Elderly', 'Adult', 'Middle_Age'),
                            Income_discretized = c("0_to_25k", "25k_to_50k", "50k_or_more", "0_to_25k", "25k_to_50k", "50k_or_more"),
                            Education_discretized = c('School', 'College','School', 'College','School', 'College'),
                            # Add other columns of the dataset here...,
                            'HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                            'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                            'GenHlth','BMIlogtransformed','MentHlthWinsorized','PhysHlthWinsorized','DiffWalk',
                            'Sex','Education_discretized')

dummy_age <- dummyVars(~ Age_discretized, data = diabetes)
one_hot_encoded_age <- predict(dummy_age, newdata = diabetes)

# Perform one-hot encoding for Income_discretized
dummy_income <- dummyVars(~ Income_discretized, data = diabetes)
one_hot_encoded_income <- predict(dummy_income, newdata = diabetes)

# Perform one-hot encoding for Education_discretized
dummy_education <- dummyVars(~ Education_discretized, data = diabetes)
one_hot_encoded_education <- predict(dummy_education, newdata = diabetes)

# Concatenate one-hot encoded columns with original DataFrame
diabetes_encoded <- cbind(diabetes, one_hot_encoded_age, one_hot_encoded_income, one_hot_encoded_education)

colnames(diabetes_encoded)

# Removing columns
diabetes_encoded <- subset(diabetes_encoded, select = -c(MentHlth, PhysHlth, Age, Education, Income, Age_discretized, Education_discretized
                                                         , Income_discretized,BMI, BMIWinsorized))



#f <- Diabetes_binary~HighBP+HighChol+CholCheck+Smoker+Stroke+HeartDiseaseorAttack+PhysActivity+Fruits+Veggies+HvyAlcoholConsump+AnyHealthcare+NoDocbcCost+GenHlth+DiffWalk+Sex+BMIlogtransformed+MentHlthWinsorized+PhysHlthWinsorized+Age_discretized.Young_Adult+Age_discretized.Adult+Age_discretized.Middle_Age+Age_discretized.Elderly+Income_discretized.0_to_25k+Income_discretized.25k_to_50k+Income_discretized.50k_or_more+Education_discretized.School+Education_discretized.College

replace_yes_no <- function(x) {
  ifelse(x == "No", 0, ifelse(x == "Yes", 1, x))
}

diabetes_encoded <- as.data.frame(lapply(diabetes_encoded, replace_yes_no))

diabetes_encoded <- mutate_all(diabetes_encoded, ~ifelse(. == "No", 0, ifelse(. == "Yes", 1, .)))


maxs <- apply(diabetes_encoded[,2:28], 2, max)
mins <- apply(diabetes_encoded[,2:28], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(diabetes_encoded[,2:28], center = mins, 
                                   scale = maxs - mins))

Diabetes_binary <- as.numeric(diabetes_encoded$Diabetes_binary) - 1
data <- cbind(Diabetes_binary, scaled.data)

sampling <- floor(0.7 * nrow(data))
train_idx <- sample(nrow(data), size = sampling, replace = FALSE)
train <- diabetes_encoded[train_idx,] 
test <- diabetes_encoded[-train_idx,]

features <- names(scaled.data)

f <-paste(features, collapse=' + ')
f<- paste('Diabetes_binary ~', f)

# Convert to formula

f <- as.formula(f)
library(neuralnet)
nn <- neuralnet(f, train, hidden = 6,  rep = 100, linear.output = FALSE)

predicted <- compute(nn, test[2:28])

colnames(diabetes_encoded)


# Reference Apriori & Decision Tree: https://www2.cs.uh.edu/~ordonez/pdfwww/w-2011-IDA-ardtcmp.pdf

#Question: In Decision tree, after each node there is some information gain and we know feature importance
# and in apriori algo we know after whkch feature which feature is used and confidence/support shows
# the importance of those feature with the target column. Is there any relation between
# Apriori and Decision Tree, can they be used together to imporve results?


