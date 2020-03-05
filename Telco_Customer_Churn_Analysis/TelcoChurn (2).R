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

## we can see how the variables were imported, as well as the first few rows.
summary(Telco.df)
glimpse(Telco.df)

##find missing value
miss.val=sapply(Telco.df, function(x) sum(length(which(is.na(x)))))
print(miss.val)

##remove all missing value
Telco.df <-na.omit(Telco.df)
write.csv(Telco.df, "TelcoChurnDataUpdated.csv")

# converting senior citizen from int data type to factor
Telco.df$SeniorCitizen <- as.factor(ifelse(Telco.df$SeniorCitizen==1,'Yes','No'))
glimpse(Telco.df)

### plot churn percent
options(repr.plot.width = 6, repr.plot.height = 4)
Telco.df %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")

##

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(Telco.df, aes(x=gender,fill=Churn))+ geom_bar(), 
          ggplot(Telco.df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(Telco.df, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(Telco.df, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(Telco.df, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(Telco.df, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

#Analyzing the Tenure continuous variables w.r.t CHURN:
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco.df, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#Analyzing the MonthlyCharges continuous variables w.r.t CHURN:

ggplot(Telco.df, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#Analyzing the TotalCharges continuous variables w.r.t CHURN:
######## define ????
ggplot(Telco.df, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#Checking the correlation between continuous variables
options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(Telco.df[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

##Checking for outliers in the continuous variables, and it seems none of the values are beyond the whiskers here.
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(Telco.df$tenure)$out

#?????? why its there and insert the axis name 
boxplot(Telco.df$MonthlyCharges)$out

boxplot(Telco.df$TotalCharges)$out


