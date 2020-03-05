library(tidyverse)
library(MASS)
library(car)
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

##### removed churn column as we are predicting that
newdata <-validation[,-23]

#Build the first model using all variables(logistic regression)
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
model_1 = glm(Churn ~ ., data = train, family = "binomial")
options(scipen=999)
summary(model_1)

pred <- predict(model_1,newdata,type = "response")
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
confusionMatrix(table(actual_churn,pred_churn))

########################################################

### STEP AIC
model_2<- stepAIC(model_1, direction="both")
options(scipen=999)
summary(model_2)

pred <- predict(model_2,newdata,type = "response")
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
confusionMatrix(table(actual_churn,pred_churn))

########################################


## keep all variable where signficale level is starred(all level starred)

model_3<-glm(Churn ~ MonthlyCharges + SeniorCitizen + 
               InternetService.xFiber.optic + InternetService.xNo + OnlineSecurity + 
               OnlineBackup + TechSupport + StreamingTV + 
               Contract.xOne.year + Contract.xTwo.year + 
               PaperlessBilling + 
               PaymentMethod.xElectronic.check +
               tenure_bin.x1.2.years + tenure_bin.x2.3.years + tenure_bin.x4.5.years + 
               tenure_bin.x5.6.years,family = "binomial", data = train)
options(scipen=999)
summary(model_3)

pred <- predict(model_3,newdata,type = "response")
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
confusionMatrix(table(actual_churn,pred_churn))

########################################

## keep all variable where signficale level is only 3 starred

model_4<-glm(Churn ~
               InternetService.xFiber.optic + InternetService.xNo + OnlineSecurity + 
               OnlineBackup + TechSupport +
               Contract.xOne.year + Contract.xTwo.year + 
               PaperlessBilling + 
               PaymentMethod.xElectronic.check +
               tenure_bin.x1.2.years + tenure_bin.x2.3.years + tenure_bin.x4.5.years + 
               tenure_bin.x5.6.years,family = "binomial", data = train)
options(scipen=999)
summary(model_4)

pred <- predict(model_4,newdata,type = "response")
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
confusionMatrix(table(actual_churn,pred_churn))



### using probability cutoff of 32%

cutoff_churn <- factor(ifelse(pred >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
print(conf_final)
print(accuracy)



# plot lift chart

gain <- gains(validation$Churn, pred, groups=10)

plot(c(0,gain$cume.pct.of.total*sum(validation$Churn))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(validation$Churn))~c(0, dim(validation)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(validation$Churn)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)



export.df <- data.frame(validation$Churn, pred)
t.df <- data.frame("Predicted" = pred, "Label" = as.factor(validation$Churn))
View(t.df)

write.csv(export.df, file = "Telcompropensities.csv")

pred <- prediction(t.df$Predicted, t.df$Label)
perf <- performance( pred, "tpr", "fpr" )
plot( perf )




########################################################
###on kaggel site
model_5<-glm(Churn ~ TotalCharges + 
               InternetService.xFiber.optic + InternetService.xNo + 
               Contract.xOne.year + Contract.xTwo.year + 
               tenure_bin.x1.2.years + tenure_bin.x2.3.years + tenure_bin.x4.5.years + 
               tenure_bin.x5.6.years,family = "binomial", data = train)

options(scipen=999)
summary(model_4)


pred <- predict(model_5,newdata,type = "response")
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
confusionMatrix(table(actual_churn,pred_churn))

#########################################################
