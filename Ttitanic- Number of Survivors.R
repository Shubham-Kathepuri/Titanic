#importing the dataset

train=read.csv('train.csv', stringsAsFactors = FALSE, header = TRUE)
test=read.csv('test.csv', stringsAsFactors = FALSE, header = TRUE)

#adding a new column to differentiate train and test after combining

train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE

#Creating column Survived to test before combining

test$Survived <- NA

#combine train and test to clean them together
full <- rbind(train,test)

#Treating the missing values

# Checking for NA values
names(full)
table(is.na(full$Pclass))
table(is.na(full$Sex))
table(is.na(full$SibSp))
table(is.na(full$Parch))
table(is.na(full$Ticket))

# #1 Embarked
table(full$Embarked)

#Add null values to the 'S' because it is in plethora
full[full$Embarked =='', "Embarked"]<- 'S'

# #2 Age
hist(full$Age)
age.median <- median(full$Age, na.rm = TRUE)
table(is.na(full$Age))
full[is.na(full$Age), "Age"] <- age.median

# #3 Fare
hist(full$Fare)
table(is.na(full$Fare))

median(full$Fare, na.rm = TRUE)
fare.median <- median(full$Fare, na.rm = TRUE)

full[is.na(full$Fare), "Fare"] <- fare.median


# Categorical casting
full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)


# Separating the train and test set
train <- full[full$IsTrainSet==TRUE,]
test <- full[full$IsTrainSet==FALSE,]

# cast survived in train set as category
train$Survived <- as.factor(train$Survived)

# Hypothesis testing
names(train)
str(train)

#1 - Does the passenger class determines who survived
chisq.test(train$Pclass,train$Survived)
# p < 0.05 
#inference - it determines who survived

#2 - does number of survivors depend on sex
chisq.test(train$Sex,train$Survived)
# p < 0.05
# inference - it does

#3- does number of survivors depend on age
t.test(train$Age~train$Survived)
# p is slightly greater than 0.05
# inference - it does not

#4 - does number of survivors depend on number of siblings and spouse
t.test(train$SibSp~train$Survived)
# p > 0.05
# inference it does not

#5 - does number of survivors depend on number of parents and child
t.test(train$Parch~train$Survived)
# p > 0.05
# inference it does not

# 6 - does number of survivors depend on fare
t.test(train$Fare~train$Survived)
# p < 0.05
# inference it does

#7 - does number of survivors depend on embarked
chisq.test(train$Embarked,train$Survived)
# p < 0.05
#inference - it does

#Formulating the equation
equation<- "Survived ~ Pclass + Sex + Fare + Embarked"

#Logistic Regression

classifier1 <- glm(formula = equation,
                   family = binomial,
                   data = train)
summary(classifier1)

#Predicting the test results
y_pred1 <- predict(classifier1, 
                   type = 'response',
                   newdata = test)
y_pred1 <- ifelse(y_pred1 > 0.5, 1,0) 

#Adding the survived column back to test set
test$Survived <- y_pred1

#Combining the Train & Test sets 
Titanic <- rbind(train,test)

# Visualization

install.packages(ggplot2)
library(ggplot2)


# Survived vs Deceased
ggplot(data= Titanic, aes(x=Survived, fill= Survived))+geom_bar()+
  labs(title =  "Deceased Vs. Survivors") + theme(plot.title=element_text(hjust = 0.5))

#Survivors based on Fare
ggplot(data= Titanic, aes(x=Fare, fill= Survived))+geom_histogram()+
  labs(title =  "Survivors based on Fare") + theme(plot.title=element_text(hjust = 0.5))

#Survivors based on Sex
ggplot(data= Titanic, aes(x=Sex, fill= Survived))+geom_bar()+
  labs(title =  "Survivors based on Sex") + theme(plot.title=element_text(hjust = 0.5))

#Survivors based on Age Group
ggplot(data= Titanic, aes(x=Age, fill= Survived))+geom_histogram()+
  labs(title =  "Survivors based on Age Group") + theme(plot.title=element_text(hjust = 0.5))

#Survivors based on Passenger Class
ggplot(data= Titanic, aes(x=Pclass,fill= Survived))+geom_bar()+ 
  labs(title =  "Survivors based on Passenger Class") + theme(plot.title=element_text(hjust = 0.5))
