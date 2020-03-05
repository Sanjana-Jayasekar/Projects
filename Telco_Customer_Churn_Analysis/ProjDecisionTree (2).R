library(e1071)
library(caret)
library(gains)
library(ROCR)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)

## Read csv file
Telco.df <- read.csv("TelcoChurnData.csv")

##remove all missing value
Telco.df <-na.omit(Telco.df)

# converting senior citizen from int data type to factor
Telco.df$SeniorCitizen <- as.factor(ifelse(Telco.df$SeniorCitizen==1,'Yes','No'))
glimpse(Telco.df)


###some categorical features that have 'No' and 'No Internet Service' or 
##'No Phone Service' as a category, we can make them as 'No' and clean these features

Telco.df <- data.frame(lapply(Telco.df, function(x) {
  gsub("No internet service", "No", x)}))

Telco.df <- data.frame(lapply(Telco.df, function(x) {
  gsub("No phone service", "No", x)}))



##Standardising Continuous features

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
Telco.df[num_columns] <- sapply(Telco.df[num_columns], as.numeric)

glimpse(Telco.df)

telco_int <- Telco.df[,c("MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

##Creating derived features
Telco.df <- mutate(Telco.df, tenure_bin = tenure)

Telco.df$tenure_bin[Telco.df$tenure_bin >=0 & Telco.df$tenure_bin <= 12] <- '0-1 year'
Telco.df$tenure_bin[Telco.df$tenure_bin > 12 & Telco.df$tenure_bin <= 24] <- '1-2 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 24 & Telco.df$tenure_bin <= 36] <- '2-3 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 36 & Telco.df$tenure_bin <= 48] <- '3-4 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 48 & Telco.df$tenure_bin <= 60] <- '4-5 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 60 & Telco.df$tenure_bin <= 72] <- '5-6 years'

Telco.df$tenure_bin <- as.factor(Telco.df$tenure_bin)

####distribution across tenure
options(repr.plot.width =6, repr.plot.height = 3)
ggplot(Telco.df, aes(tenure_bin, fill = tenure_bin)) + geom_bar()

#creating dummy variable
telco_cat <- Telco.df[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))


#Combining the data(numerical and dummy)
telco_final <- cbind(telco_int,dummy)
head(telco_final)
#Splitting the data(training and partition)
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

library(rpart)
library(rpart.plot)
options(repr.plot.width = 10, repr.plot.height = 8)


#Training
Dtree = rpart(Churn ~., data = train, method = "class")
summary(Dtree)

# plot tree
prp(Dtree, type = 1, extra = 2,under = FALSE, split.font = 2, tweak = 1.2,varlen = -15)


#Predicting 
DTPred <- predict(Dtree,type = "class", newdata = validation[,-23])

confusionMatrix(as.factor(validation$Churn), DTPred)

#### pruned tree

cv.ct <- rpart(Churn ~., data = train, method = "class", 
               control = rpart.control(cp = 0.00001, minsplit = 5, xval = 5))

# use printcp() to print the table. 
printcp(cv.ct)
prp(cv.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

# prune by lower cp
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))  
