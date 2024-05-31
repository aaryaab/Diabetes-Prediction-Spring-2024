diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')


featureNames = colnames(diabetes) # list of feature names
print(featureNames)

class(diabetes)

head(diabetes, 10) 


#plot Diabetes_binary column

diabetes_counts <- table(diabetes$Diabetes_binary)
barplot(diabetes_counts, 
        main = "Proportion of Yes and No in diabetes_binary Column",
        xlab = "Diabetes Binary",
        ylab = "Frequency")


stroke_counts <- table(diabetes$Stroke)
barplot(stroke_counts, 
        main = "Proportion of Yes and No in Stroke Column",
        xlab = "Stroke",
        ylab = "Frequency")


HeartDiseaseorAttack_counts <- table(diabetes$HeartDiseaseorAttack)
barplot(HeartDiseaseorAttack_counts, 
        main = "Proportion of Yes and No in HeartDiseaseorAttack_counts Column",
        xlab = "Stroke",
        ylab = "Frequency")


HvyAlcoholConsump_counts <- table(diabetes$HvyAlcoholConsump)
barplot(HvyAlcoholConsump_counts, 
        main = "Proportion of Yes and No in HvyAlcoholConsump_counts Column",
        xlab = "Stroke",
        ylab = "Frequency")


NoDocbcCost_counts <- table(diabetes$NoDocbcCost)
barplot(NoDocbcCost_counts, 
        main = "Proportion of Yes and No in NoDocbcCost_counts Column",
        xlab = "Stroke",
        ylab = "Frequency")

boxplot(BMI ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "BMI Boxplot",       
        ylab = "BMI",                  
        xlab = "Diabetes" ) 

hist(diabetes$MentHlth, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of MentHlth",      # title of the histogram
     xlab="MentHlth",      # label of x axis
     ylab="Number of Rows")  

boxplot(MentHlth ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "MentHlth Boxplot",       
        ylab = "MentHlth",                  
        xlab = "Diabetes" ) 

hist(diabetes$PhysHlth, #Histogram of BMI_winsorized
     #breaks=seq(from = 12, to = 98, by = 10),
     main="Histogram of PhysHlth",      # title of the histogram
     xlab="PhysHlth",      # label of x axis
     ylab="Number of Rows")  

boxplot(PhysHlth ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "PhysHlth Boxplot",       
        ylab = "PhysHlth",                  
        xlab = "Diabetes" ) 


# getting quantile values for winsorization

print(quantile(diabetes$BMI))
print(quantile(diabetes$BMI, probs=c(0.75,0.80,0.90,0.95,0.98,0.99, 0.992,0.995, 0.998)))

print(quantile(diabetes$MentHlth))
print(quantile(diabetes$MentHlth, probs=c(0.75,0.80,0.90,0.95,0.98,0.99, 0.992,0.995, 0.998)))

print(quantile(diabetes$PhysHlth))
print(quantile(diabetes$PhysHlth, probs=c(0.75,0.80,0.90,0.95,0.98,0.99, 0.992,0.995, 0.998)))



# We are dropping Stroke, HvyAlcoholConsump, NoDocbcCost, AnyHealthcare
# We are winsorizing BMI at 99% on right tail only
# We are winsorizing MentHlth at 80% on right tail only
# We are winsorizing PhysHlth at 80% on right tail only



diabetes_clean <- subset(diabetes, select = -c(Stroke,HvyAlcoholConsump,NoDocbcCost))

featureNames = colnames(diabetes) # list of feature names
print(featureNames)

winsorize_func <- function(x, trim = 0.2) {
  q <- quantile(x, c(0, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}


diabetes$BMI_winsorized = winsorize_func(diabetes$BMI)

boxplot(BMI_winsorized ~ Diabetes_binary,   
        data = diabetes_clean,   
        col = "grey",    
        main = "BMI Boxplot after winsorization",       
        ylab = "BMI",                  
        xlab = "Diabetes" ) 

diabetes$MentHlth_winsorized = winsorize_func(diabetes$MentHlth)

boxplot(MentHlth_winsorized ~ Diabetes_binary,   
        data = diabetes_clean,   
        col = "grey",    
        main = "MentHlth Boxplot after winsorization",       
        ylab = "MentHlth",                  
        xlab = "Diabetes" ) 

diabetes$PhysHlth_winsorized = winsorize_func(diabetes$PhysHlth)

boxplot(PhysHlth_winsorized ~ Diabetes_binary,   
        data = diabetes_clean,   
        col = "grey",    
        main = "PhysHlth Boxplot after winsorization",       
        ylab = "PhysHlth",                  
        xlab = "Diabetes" ) 


summary(diabetes_clean)

diabetes_clean <- subset(diabetes_clean, select = -c(BMI,PhysHlth,MentHlth))

diabetes_clean <- subset(diabetes_clean, select = -c(AnyHealthcare))


#> Loading required package: MASS
library(ggplot2)
library(corrplot)
library(RColorBrewer)
#Getting correlation of features

R <- cor(diabetes_clean)

corrplot(R, method = "shade", type="full")


#PhysHlth_winsorized & DiffWalk are correlated
# GenHlth & DiffWalk are correlated
# GenHlth & PhysHlth_winsorized are correlated
# Education & Income are correlated


# Remove PhysHlth_winsorized, Education, Diffwalk

diabetes_clean <- subset(diabetes_clean, select = -c(Education, PhysHlth_winsorized))
diabetes_clean <- subset(diabetes_clean, select = -c(DiffWalk))

library(C50)

# Decision Tree
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'GenHlth','DiffWalk','BMI_winsorized','MentHlth_winsorized','PhysHlth_winsorized',
                'Sex','Age','Income','Education')
#Decision Tree
dt_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary) #, rules = TRUE)

summary(dt_model)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 86.51 accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")

#10 triaks 74.55%
#100 trials 74.41%






# Raw Decision Tree model, no preprocessing, nothing
diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')


featureNames = colnames(diabetes) # list of feature names
print(featureNames)



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

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 86.51 accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")

# Without any preprocessing












#PhysHlth & GenHlth are correlated,
# GenHlth & DiffWalk are correlated,
# Income & Education are correlated,
# Age & HighBp are correlated


# No Winsorize, No Log Transform

diabetes$Diabetes_binary <- as.numeric(diabetes$Diabetes_binary)
diabetes$BMIWinsorized <- as.numeric(diabetes$BMIWinsorized)
diabetes$BMIlogtransformed <- as.numeric(diabetes$BMIlogtransformed)
diabetes$MentHlthWinsorized <- as.numeric(diabetes$MentHlthWinsorized)
diabetes$PhysHlthWinsorized <- as.numeric(diabetes$PhysHlthWinsorized)
diabetes$Age_discretized <- as.numeric(diabetes$Age_discretized)
diabetes$Education_discretized <- as.numeric(diabetes$Education_discretized)
diabetes$Income_discretized <- as.numeric(diabetes$Income_discretized)

diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
diabetes$HighBP <- as.factor(diabetes$HighBP)
diabetes$HighChol <- as.factor(diabetes$HighChol)
diabetes$CholCheck <- as.factor(diabetes$CholCheck)
diabetes$BMI <- as.factor(diabetes$BMI)
diabetes$Smoker <- as.factor(diabetes$Smoker)
diabetes$Stroke <- as.factor(diabetes$Stroke)
diabetes$HeartDiseaseorAttack <- as.factor(diabetes$HeartDiseaseorAttack)
diabetes$PhysActivity <- as.factor(diabetes$PhysActivity)
diabetes$Fruits <- as.factor(diabetes$Fruits)
diabetes$Veggies <- as.factor(diabetes$Veggies)
diabetes$HvyAlcoholConsump <- as.factor(diabetes$HvyAlcoholConsump)
diabetes$AnyHealthcare <- as.factor(diabetes$AnyHealthcare)
diabetes$NoDocbcCost <- as.factor(diabetes$NoDocbcCost)
diabetes$GenHlth <- as.factor(diabetes$GenHlth)
diabetes$MentHlth <- as.factor(diabetes$MentHlth)
diabetes$PhysHlth <- as.factor(diabetes$PhysHlth)
diabetes$DiffWalk <- as.factor(diabetes$DiffWalk)
diabetes$Sex <- as.factor(diabetes$Sex)
diabetes$Age <- as.factor(diabetes$Age)
diabetes$Education <- as.factor(diabetes$Education)
diabetes$Income <- as.factor(diabetes$Income)

winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}


boxplot(BMI ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "BMI Boxplot",       
        ylab = "BMI",                  
        xlab = "Diabetes" ) 


diabetes$BMIWinsorized <- winsorize(diabetes$BMI)
diabetes$BMIlogtransformed <- log(diabetes$BMIWinsorized)
diabetes$MentHlthWinsorized <- winsorize(diabetes$MentHlth)
diabetes$PhysHlthWinsorized <- winsorize(diabetes$PhysHlth)

#Age
breaks <- c(0, 1, 5, 8, 13)
labels <- c("Young_Adult", "Adult", "Middle_Age", "Elderly")
# Discretize the Age column
diabetes$Age_discretized <- cut(diabetes$Age, breaks = breaks, labels = labels, include.lowest = TRUE)

#Education
# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7)
labels <- c("School", "College")
# Discretize the Education column
diabetes$Education_discretized <- cut(diabetes$Education, breaks = breaks, labels = labels, include.lowest = TRUE)


# Income # not working
breaks <- c(1, 5, 7, 9)
labels <- c("0_to_25k", "25k_to_50k", "50k_or_more")
# Discretize the Income column
diabetes$Income_discretized <- cut(diabetes$Income, breaks = breaks, labels = labels, include.lowest = TRUE)


# ML Models packages
install.packages("C50") #macos binary package error resolved using: Sys.setenv(PATH = paste("/usr/bin:/bin:/usr/sbin:/sbin", Sys.getenv("PATH"), sep=":")) 
library(C50)


# Decision Tree

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'GenHlth','BMIlogtransformed','MentHlthWinsorized','PhysHlthWinsorized','DiffWalk',
                'Sex','Age_discretized','Education_discretized','Income_discretized')
#Decision Tree
dt_model <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary, rules = TRUE)

summary(dt_model)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred <- predict(dt_model, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 86.51 accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")


# Balanced dataset, wihtout removing correlated features, the accuracy is 74.18%
# On increasing trials = 74.19%
# For rules = True, = 74.38%

# 1st Change

# Removing featues which were correlated before winsorization and discretization

# After winsorization & discretization, following columns are highly correlated:
#BMIlogtransformed & BMIWinsorized & BMI,
# PhysHlth_Winsorized & GenHlth, DiffWalk & GenHtlh, Education & Income

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]


predictors <- c('HighBP', 'HighChol','CholCheck','BMI','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
               'BMIlogtransformed','MentHlthWinsorized', 'GenHlth',
                'Sex','Education_discretized')
#Decision Tree
dt_model1 <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary)

#summary(dt_model1)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred <- predict(dt_model1, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) #

cat("Accuracy is ", accuracy ,"\n")

# After removing 3 correlated columns(GenHlth, Age_discretized, Income_discretized) accruacy dropped to 71.9%
# After removing 3 correlated columns(PhysHlthWinsorized, Age_discretized, Income_discretized) accruacy dropped to 73.24%
# After removing 4 correlated columns(PhysHlthWinsorized, DiffWalk ,Age_discretized, Income_discretized) accruacy dropped to 73.36%


# 2nd Change

# Using columsn without any discretization & Winsorization

set.seed(200)
sampling <- floor(0.8 * nrow(diabetes))
train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
train <- diabetes[train_idx,] 
test <- diabetes[-train_idx,]


predictors <- c('HighChol','CholCheck','Smoker','Stroke','HeartDiseaseorAttack',
                'PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost',
                'BMI','MentHlth', 'PhysHlth','Income','HighBP',
                'Sex')
#Decision Tree
dt_model1 <- C5.0.default(x = train[,predictors], y = train$Diabetes_binary)

#summary(dt_model1)
#plot(dt_model)

#plot(dt_model, trial =0 )


pred <- predict(dt_model1, newdata = test)

evaluation <- cbind(test, pred)
#head(evaluation)

evaluation$correct <- ifelse(evaluation$Diabetes_binary == evaluation$pred,1,0)
#head(evaluation)

accuracy <- sum(evaluation$correct)/nrow(evaluation) # 86.51 accuracy of decision tree

cat("Accuracy is ", accuracy ,"\n")

# Removing correlated features on not preprocessed features: got 73.44% accuracy
# using all columsn without any preprocessing, accuracy is 73.90%
# Remove PhysHlth, Income,DiffWalk, HighBP, accuracy is 73.60%
# Removing GenHlth, Education, Age, accuracy is 72.3%


# Naive Bayes

diabetes_1 <- subset(diabetes, select = -c(MentHlth, PhysHlth, Age, Education, Income, Sex, BMI, Fruits, Smoker, HeartDiseaseorAttack))


sampling <- floor(0.8 * nrow(diabetes_1))
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

# accuracy is 71.49%

# Neural Network

diabetes_nn <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

maxs <- apply(diabetes[,2:22], 2, max)
mins <- apply(diabetes[,2:22], 2, min)

scaled.data <- as.data.frame(scale(diabetes_nn[,2:22], center = mins, 
                                   scale = maxs - mins))

Diabetes_binary <- as.numeric(diabetes_nn$Diabetes_binary) - 1
data <- cbind(Diabetes_binary, scaled.data)


sampling <- floor(0.7 * nrow(data))
train_idx <- sample(nrow(data), size = sampling, replace = FALSE)
train <- diabetes_nn[train_idx,] 
test <- diabetes_nn[-train_idx,]

features <- names(scaled.data)

f <-paste(features, collapse=' + ')
f<- paste('Diabetes_binary ~', f)

# Convert to formula

f <- as.formula(f)
library(neuralnet)
nn <- neuralnet(f, train, hidden = 6,  rep = 100, linear.output = FALSE)

predicted <- compute(nn, test[2:28])


