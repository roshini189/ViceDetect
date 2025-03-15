library(mice)
library(party)
library(tidyverse)
#library(MASS)
library(VIM)
library(pls)
library(glmnet)
library(caret)
library(earth)
library(car)
library(partykit)

#Reading the data 
SmokingData <- read.csv("P:/Intelligent Data Analytics/Project/smoking_driking_dataset_Ver01.csv")
view(SmokingData)


#Splitting into Numeric and non-numeric Dataframes

SmokingNumeric <- SmokingData %>%
  select_if(is.numeric)
SmokingNumeric <- SmokingData[, sapply(SmokingData, function(x) is.numeric(x) && length(unique(x)) > 8)]
#view(SmokingNumeric)

#we'll consider numeric variables with less than 8 levels as factor Variables
SmokingDiscrete <- SmokingData[, sapply(SmokingData, function(x) length(unique(x)) < 8)]
#view(SmokingDiscrete)

#Function to find 1st and 3rd Quartile
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#Function to find different summary parameters for the numeric table
SmokingNumericSummary <- function(x){
  c(length(x), n_distinct(x),((n_distinct(x)/length(x))*100), sum(is.na(x)),((sum(is.na(x))/length(x))*100), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

#Using Summarize to apply the function to entire table
SmokingNumericTableSummary <- SmokingNumeric %>%
  summarize(across(everything(), SmokingNumericSummary))

view(SmokingNumericTableSummary)


#adding column names to summary table
SmokingNumericTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_Percentage", "mean","min","Q1","median","Q3","max","sd"),
  SmokingNumericTableSummary)
view(SmokingNumericTableSummary)


#applying Kable to create final summary report
SmokingNumericSummaryFinal <- SmokingNumericTableSummary %>%
  pivot_longer("age":"gamma_GTP", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)%>% 
  #mutate(missing_pct = 100*missing/n,
  #unique_pct = 100*unique/n) %>%
  select(variable, n, missing,  unique, everything())
#view(SmokingNumericSummaryFinal)

library(knitr)
options(digits=3)
options(scipen=99)

SmokingNumericSummaryFinal %>% kable()

#Displaying Final Summary report
view(SmokingNumericSummaryFinal)

#for Discrete Data
#function to get modes
getmodes <- function(v,type=1) {
  if(sum(is.na(v))==length(v)){
    return(NA)
  }
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

#function to get modes count
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

#Function to find different summary parameters for the Discrete table
TrainDiscreteSummary <- function(x){
  c(length(x), n_distinct(x),(n_distinct(x)*100/length(x)), sum(is.na(x)),(sum(is.na(x))*100/length(x)),  getmodes(x, type=1), getmodesCnt(x, type =1),
    getmodes(x, type= -1), getmodesCnt(x, type = -1), (getmodesCnt(x, type =1)*100/getmodesCnt(x, type = -1)))
}

##Using Summarize to apply the function to entire table
result1 <- lapply(SmokingDiscrete, TrainDiscreteSummary)
result_matrix <- do.call(cbind, result1)

# Convert the matrix into a dataframe

SmokingDiscreteTableSummary <- as.data.frame(result_matrix)

#adding column names to summary table
SmokingDiscreteTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_percentage","1st mode", "first_mode_freq", 
         "least common", "least common freq","Freq_Ratio"),
  SmokingDiscreteTableSummary)
view(SmokingDiscreteTableSummary)

#applying Kable to create final summary report
DiscreteFactorSummaryFinal <- SmokingDiscreteTableSummary %>%
  pivot_longer("sex":"DRK_YN", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)


#Displaying Final Summary Report
view(DiscreteFactorSummaryFinal)

#displaying Histogram of waistline column to explain the Data problem

hist(SmokingData$waistline)
table(SmokingData$waistline)

#displaying Histogram of sight_left and sight_right to explain the Data problem
par(mfrow = c(1, 2))
hist(SmokingData$sight_left)
hist(SmokingData$sight_right)

#visualisations
ggplot(data = SmokingData) + 
  geom_boxplot(aes(x = DRK_YN, y = age, fill = sex))  + 
  labs(x = "Drinker Yes or No", y = "Age", title = "Comparison of Male/Female Drinkers")                            
#From the plot we can say that there are more female drinkers around age 20-30 when compared to male. Whereas male drinkers are falling 
#in more among 30-50 age group. So, we can conclude that in the average age life cycle there are more male drinkers 
#whereas there are more young female drinkers. 

ggplot(data = SmokingData, mapping=aes(x= sex, group = LDL_chole, fill = LDL_chole)) +
  geom_density()+
  facet_wrap(~DRK_YN)
# From the plot we can observe that the male drinkers are having more bad cholesterol. So, we can 
#conclude that Drinking is affecting the individual health.

##DATA PREPROCESSING

#Values to be treated as missing
SmokingData$waistline[SmokingData$waistline == 999] <- NA
SmokingData$HDL_chole[SmokingData$HDL_chole == 8110] <- NA
SmokingData$LDL_chole[SmokingData$LDL_chole == 5119] <- NA
SmokingData$SGOT_AST[SmokingData$SGOT_AST == 7000] <- NA
SmokingData$SGOT_AST[SmokingData$SGOT_AST == 9999] <- NA
SmokingData$SGOT_ALT[SmokingData$SGOT_ALT == 7210] <- NA

# Specifying the variables to impute
variables_to_impute <- c("HDL_chole", "waistline", "LDL_chole", "triglyceride", "SGOT_AST","SGOT_ALT")

# Creating an imputation model
imputation_model <- mice(SmokingData[, variables_to_impute])

# doing the imputation
imputed_data <- complete(imputation_model)

# Replacing the imputed values in the original data
SmokingData[, variables_to_impute] <- imputed_data[, variables_to_impute]



# Set a threshold value
threshold <- 4

# Replace values greater than the threshold with the threshold value for sight_left
SmokingData$sight_left[SmokingData$sight_left > threshold] <- threshold

# Repeat the process for sight_right
SmokingData$sight_right[SmokingData$sight_right > threshold] <- threshold

# Check unique values in sight_left and sight_right after replacement
unique_values_left <- unique(SmokingData$sight_left)
unique_values_right <- unique(SmokingData$sight_right)

print("Unique values in sight_left after replacement:")
print(unique_values_left)

print("Unique values in sight_right after replacement:")
print(unique_values_right)

# Plot histograms after handling outlier values
par(mfrow = c(1, 2))

hist(SmokingData$sight_left, main = "Histrogram of sight_left", xlab = "Value")
hist(SmokingData$sight_right, main = "Histrogram of sight_right", xlab = "Value")

#Splitting the Data set into Train and Test
total_rows <- nrow(SmokingData)

# Specify the number of rows for the training set
num_train_rows <- 700000

# Set seed for reproducibility
set.seed(123)

# Generate random indices for the training set
train_indices <- sample(1:total_rows, num_train_rows)

# Create the training set
train_data <- SmokingData[train_indices, ]

# Create the test set by excluding the training set indices
test_data <- SmokingData[-train_indices, ]

# Print the dimensions of the training and test sets
cat("Dimensions of Train Data:", dim(train_data), "\n")
cat("Dimensions of Test Data:", dim(test_data), "\n")

train_data_smk <- train_data[, !colnames(train_data) %in% 'DRK_YN']

# Remove 'DRK_YN' column from test_data
test_data_smk <- test_data[, !colnames(test_data) %in% 'DRK_YN']

# Specify the target variable
target_variable <- "SMK_stat_type_cd"

train_data[[target_variable]] <- as.factor(train_data[[target_variable]])
test_data[[target_variable]] <- as.factor(test_data[[target_variable]])

par(mfrow = c(1, 1))
hist(train_data_smk$waistline)

params <- list(
  booster = "gbtree",
  objective = "multi:softmax",  # For multi-class classification
  num_class = 3,  # Number of classes
  eval_metric = "mlogloss"  # Evaluation metric
)
char_vars <- sapply(train_data_smk, is.numeric)
train_data_smk$sex <- ifelse(train_data_smk$sex == 'male', 1, 0)

