diabetes <- read.csv('diabetes_binary_5050split_health_indicators_BRFSS2015.csv')

#Using Logistics Regression

breaks <- c(0, 8, 13)
labels <- c("Adult", "Elderly")

# Discretize the Age column
diabetes$Age_discretized <- cut(diabetes$Age, breaks = breaks, labels = labels, include.lowest = TRUE)


Age_discretized_counts <- table(diabetes$Age_discretized)
barplot(Age_discretized_counts, 
        main = "Bar chart for Age_discretized categroies",
        xlab = "Age",
        ylab = "Frequency")


# Defining the breaks and labels for discretization
breaks <- c(1, 5, 7)
labels <- c("School", "College")
diabetes$Education_discretized <- cut(diabetes$Education, breaks = breaks, labels = labels, include.lowest = TRUE)

breaks <- c(1, 5, 7, 9)
labels <- c("0_to_25k", "25k_to_50k", "50k_or_more")

# Discretize the Income column
diabetes$Income_discretized <- cut(diabetes$Income, breaks = breaks, labels = labels, include.lowest = TRUE)

Income_discretized_counts <- table(diabetes$Income_discretized)
barplot(Income_discretized_counts, 
        main = "Bar chart for Income_discretized categroies",
        xlab = "Income",
        ylab = "Frequency")

#winsorizeBMI
winsorize_BMI <- function(x, trim = 0.01) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

#winsorizeHlth
winsorize <- function(x, trim = 0.02) {
  q <- quantile(x, c(0, 1 - trim), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

diabetes$BMIWinsorized <- winsorize_BMI(diabetes$BMI)
diabetes$MentHlth_winsorized = winsorize(diabetes$MentHlth)
diabetes$PhysHlth_winsorized = winsorize(diabetes$PhysHlth)

boxplot(BMIWinsorized ~ Diabetes_binary,   
        data = diabetes,   
        col = "grey",    
        main = "BMIWinsorized Boxplot",       
        ylab = "BMI_winsorized",                  
        xlab = "Diabetes" ) 

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m, ymin=ymin,ymax=ymax))
}

diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
library(ggplot2)
# Basic violin plot
p <- ggplot(diabetes, aes(x=Diabetes_binary, y=PhysHlth, color = Diabetes_binary)) + 
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(diabetes, aes(x=Diabetes_binary, y=BMIWinsorized,color = Diabetes_binary)) + 
  geom_violin(trim=FALSE) + labs(title="Violin Plot of BMIWinsorized")




p + stat_summary(fun.data=data_summary)



p + geom_boxplot(width=0.1)




#training_index <- sample(nrow(titanic), size = size, replace = FALSE)        
size <- floor(0.8 * nrow(diabetes))        
training_index <- sample(nrow(diabetes), size = size, replace = FALSE) 
train <- diabetes[training_index,]
test <- diabetes[-training_index,]


reg <- glm(Diabetes_binary ~ . , data = train, family = binomial() )
###  Model detail
summary(reg)

evaluation <- test
evaluation$prob <- predict(reg, newdata = evaluation, type = "response")

diabetes$Diabetes_binary = factor(diabetes$Diabetes_binary, levels=c(0, 1), labels=c("No", "Yes"))

g <- roc(evaluation$Diabetes_binary ~ evaluation$prob, data = evaluation)

plot(g)

g # with preprocess auc is 0.8287

#features from logistic regression:
#HighBP,HighChol,ChilCheck,BMI,Stroke,HeartDiseaseorAttack,Veggies,HvyAlcoholConsump,
#GenHlth,PhysHlth, DiffWalk, Sex, Age,Education, Income, Age_discretized.Middle_Age,
# Age_discretized.Elderly,MentHlth_winsorized , BMIWinsorized


predictors1 <-c('HighBP','HighChol','CholCheck','Stroke', 'HvyAlcoholConsump',
                'HeartDiseaseorAttack','Veggies',
                'GenHlth','PhysHlth', 'DiffWalk', 'Sex','Education_discretized', 
                'Income','Age_discretized',
                'MentHlth_winsorized', 'BMIWinsorized')

#Veggies, 
#seeds <- c(205,215,225,235,245,255,265,275,285,295)
seeds <- (265)
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
evaluation_table <- data.frame(Seed = integer(),
                               Accuracy = double(),
                               Precision = double(),
                               Recall = double(),
                               F1_Score = double())
# Loop through each seed
for (seed in seeds) {
  # Set the seed
  #set.seed(265)
  # Split the data into train and test sets
  sampling <- floor(0.8 * nrow(diabetes))
  train_idx <- sample(nrow(diabetes), size = sampling, replace = FALSE)
  train <- diabetes[train_idx,] 
  test <- diabetes[-train_idx,]
  
  # Train the model
  dt_model <- C5.0.default(x = train[,predictors1], y = train$Diabetes_binary,trials = 100)
  #rf_model <- randomForest(x = train[,predictors1], y = train$Diabetes_binary)
  # Make predictions on the test set
  pred <- predict(dt_model, newdata = test)
  
  #summary(dt_model)
  
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

