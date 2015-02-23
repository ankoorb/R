# Read .csv file
train = read.csv("train.csv")

# Structure of the dataframe
str(train)

# Summary of data
summary(train)

# Create new column in data frame with all values = 0
train$Child = 0

# Strings are imported as factors in R
# To create text strings from factors
train$Name = as.character(train$Name)

# To split a string for 1 to 5 entries
strsplit(train$Name[1:5], split='[,.]')

# To extract a string for all rows
train$Title = sapply(train$Name, FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})

# To remove spaces from " Mr" to return "Mr"
train$Title = sub(' ','',train$Title)

# To see unique text/values
unique(train$Title)

# To combine unusual texts, %in% operator checks to see if a value is part of the vector we are comparint to it.
train$Title[train$Title %in% c('Mme','Mlle')]='Mlle'
train$Title[train$Title %in% c('Don','Rev','Major','Sir','Col','Capt')]='Mr'
train$Title[train$Title %in% c('Dona','Lady','the Countess','Jonkheer')]='Mrs'
train$Title[train$Title %in% c('Ms','Mlle')]='Miss'




# Mosaic Plot
mosaicplot(prop.table(table(train$Sex,train$Survived),2),xlab = "Sex",ylab="Survived")
A = prop.table(table(train$Pclass,train$Survived,train$Sex),2)
mosaicplot(A, main = "Passenger Fate by Class and Sex", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")

# Extracting Text (British titles like Mr, Mrs, etc)
start.title = regexpr("\\,[A-Z ]{1,50}\\.", train$Name, TRUE)
end.title = start.title + attr(start.title, "match.length")-1
train$Title = substr(train$Name, start.title+2, end.title-1)

# Checking records()
library(Hmisc)
bystats(train$Age, train$Title)

# Mean and Median
bystats(train$Age, train$Title, fun = function(x)c(Mean=mean(x), Median=median(x)))

# Median age as per Title
master_age = median(train$Age[train$Title == "Master"],na.rm = TRUE)
miss_age = median(train$Age[train$Title == "Miss"],na.rm = TRUE)
mrs_age = median(train$Age[train$Title == "Mrs"],na.rm = TRUE)
mr_age = median(train$Age[train$Title == "Mr"],na.rm = TRUE)
dr_age = median(train$Age[train$Title == "Dr"],na.rm = TRUE)

for (i in 1:nrow(train)){
  if (is.na(train[i,6])){
    if (train$Title[i] == "Master"){
      train$Age[i] = master_age}
    else if (train$Title[i] == "Miss"){
      train$Age[i] = miss_age}
    else if (train$Title[i] == "Mrs"){
      train$Age[i] = mrs_age}
    else if (train$Title[i] == "Mr"){
      train$Age[i] = mr_age}
    else if (train$Title[i] == "Dr"){
      train$Age[i] = dr_age}
    else {print("Unknown")}
  }        
}

# Logistic model
m.fit = glm(Survived ~ Pclass + Sex + Age + Fare, data = train, family = "binomial")

# Prediction
s.hat = predict(m.fit, data = test, type = "response")

# Calculation of fate outcome for test data
fate = vector()
for (i in 1:length(s.hat)){
  if (s.hat[i] > 0.5) {fate[i] = 1}
  else {fate[i] = 0}
}

############# Titanic #############


# Read .csv file
test = read.csv("test.csv")


# Strings are imported as factors in R
# To create text strings from factors
test$Name = as.character(test$Name)


# To extract a string for all rows
test$Title = sapply(test$Name, FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})

# To remove spaces from " Mr" to return "Mr"
test$Title = sub(' ','',test$Title)


# To combine unusual texts, %in% operator checks to see if a value is part of the vector we are comparint to it.
test$Title[test$Title %in% c('Mme','Mlle')]='Mlle'
test$Title[test$Title %in% c('Don','Rev','Major','Sir','Col','Capt')]='Mr'
test$Title[test$Title %in% c('Dona','Lady','the Countess','Jonkheer')]='Mrs'
test$Title[test$Title %in% c('Ms','Mlle')]='Miss'

# Median age as per Title
master_age = median(test$Age[test$Title == "Master"],na.rm = TRUE)
miss_age = median(test$Age[test$Title == "Miss"],na.rm = TRUE)
mrs_age = median(test$Age[test$Title == "Mrs"],na.rm = TRUE)
mr_age = median(test$Age[test$Title == "Mr"],na.rm = TRUE)
dr_age = median(test$Age[test$Title == "Dr"],na.rm = TRUE)


# Assign median age values to NA in Age column based on Title
for (i in 1:nrow(test)){
  if (is.na(test[i,5])){
    if (test$Title[i] == "Master"){
      test$Age[i] = master_age}
    else if (test$Title[i] == "Miss"){
      test$Age[i] = miss_age}
    else if (test$Title[i] == "Mrs"){
      test$Age[i] = mrs_age}
    else if (test$Title[i] == "Mr"){
      test$Age[i] = mr_age}
    else if (test$Title[i] == "Dr"){
      test$Age[i] = dr_age}
    else {print("Uncaught Title")}
  }        
}

train["Child"]
for (i in 1:nrow(train)) {
  if (train$Age[i] <= 12) {
    train$Child[i] = 1
  } else {
    train$Child[i] = 0
  }
}


train["Mom"]
for (i in 1:nrow(train)) {
  if (train$Title[i] == "Mrs" & train$Parch[i] > 0) {
    train$Mom[i] = 1
  } else {
    train$Mom[i] = 0
  }
}
# Prediction
s.hat = predict(m.fit, data = test, type = "response")

# Calculation of fate outcome for test data
fate = vector()
for (i in 1:length(s.hat)){
  if (s.hat[i] >= 0.5) {
    fate[i] = 1
  } else {
    fate[i] = 0
  }
}

kaggle.sub <- cbind(PassengerId,fate)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "kaggle.csv", row.names = FALSE)

