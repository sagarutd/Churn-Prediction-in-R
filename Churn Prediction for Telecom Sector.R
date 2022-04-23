#Adding the original dataset
setwd("C:/Users/ssg200012/OneDrive - The University of Texas at Dallas/University/SEM 2/BA with R/Project")
original.df <- read.csv("Project_dataset.csv")
View(original.df)
#typeof(original.df)

#Isolating NUll/missing values and replacing with calculated values 
sum(is.na(original.df$TotalCharges))
#11 records have missing values under TotalCharges variable and have 0 value for tenure variable.
#For replacing this we will be calculating the average tenure of a customer and will assign that to these records and then accordingly calculating totalcharges values.
avg_tenure <- round(mean(original.df$tenure[original.df$tenure>0]))
original.df$tenure[original.df$tenure== 0] <- avg_tenure
original.df$TotalCharges[is.na(original.df$TotalCharges)] <- (original.df$tenure * original.df$MonthlyCharges)

#Removing customerID variable
original.df <- original.df[,-c(1)]

#Converting Yes/No of Churn variable in 0/1
original.df$Churn[original.df$Churn == "Yes"] <- 1
original.df$Churn[original.df$Churn == "No"] <- 0
original.df$Churn <- as.numeric(original.df$Churn)

#Adding dummy variables
install.packages("fastDummies")
library(fastDummies)
original.df <- dummy_cols(original.df, select_columns = c('gender', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService', 'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies', 'Contract', 'PaperlessBilling', 'PaymentMethod'))
original.df <- original.df[,-c(1,3,4,6,7,8,9,10,11,12,13,14,15,16,17)]
typeof(original.df)

#Dividing original dataset in Training and Validation dataset
even_rows = seq_len(nrow(original.df))%%2
train.df = original.df[even_rows == 1,]
valid.df = original.df[even_rows == 0,]

#Decision tree model
library(rpart)
library(rpart.plot)
set.seed(1)
train.df.dec <- rpart(Churn ~ ., data = train.df, method = "class")
rpart.plot(train.df.dec)

#Creating confusion matrix with training data set
library(caret)
confmat.train.df.dec <- predict(train.df.dec,train.df,type = "class")
confusionMatrix(confmat.train.df.dec, as.factor(train.df$Churn))
#Accuracy = 0.789

#Validating decision model with validation dataset
confmat.train.valid.df.dec <- predict(train.df.dec,valid.df,type = "class")
confusionMatrix(confmat.train.valid.df.dec, as.factor(valid.df$Churn))
#Accuracy = 0.7932

#Logistic Regression
train.df.logit <- glm(Churn ~ ., data = train.df, family = "binomial")
options(scipen = 999)
summary(train.df.logit)

# use predict() with type = "response" to compute predicted probabilities with training data
train.df.logit.pred <- predict(train.df.logit, train.df, type = "response")
confusionMatrix(as.factor(ifelse(train.df.logit.pred > 0.5, 1, 0)), as.factor(train.df$Churn))
#Accuracy = 0.8061

# use predict() with type = "response" to compute predicted probabilities with validation data
train.df.logit.pred2 <- predict(train.df.logit, valid.df, type = "response")
confusionMatrix(as.factor(ifelse(train.df.logit.pred2 > 0.5, 1, 0)), as.factor(valid.df$Churn))
#Accuracy = 0.8037

#Logistic regression conclusion
w3 <- coef(train.df.logit)
w3
w3 <- w3[-1]
w3 <- sort(w3, decreasing = TRUE)
barplot(w3, width = 0.1, space = NULL, cex.names = 0.65, cex.lab = 0.65, las=2, ylim = c(-10,10))


#ROC curve for decision tree using Validation data.
library(pROC)
confmat.train.df.dec2 <- predict(train.df.dec,valid.df,type = "prob")
r1 <- roc(valid.df$Churn,confmat.train.df.dec2[,1])
plot.roc(r1)
auc(r1)
#Area under curve = 0.7966

#ROC curve for logistic regression using Validation data.
r2 <- roc(valid.df$Churn,train.df.logit.pred2, auc = TRUE)
plot.roc(r2)
auc(r2)
#Area under curve = 0.8508



#DATA VISUALIZATION
#1 Senior citizen pie chart
install.packages("plotrix")
library(plotrix)
sc.table <- table(original.df$SeniorCitizen)
pie3D(sc.table, labels = names(sc.table), explode = 0.3, main = "SeniorCitizen Ratio In Data Set")

#2Customers by Tenure
counts <- table(original.df$tenure)
barplot(counts, main = "Number Of Customers by Tenure", xlab = "Tenure", ylab = "Number of Customers")

