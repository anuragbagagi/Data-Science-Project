# clear all the variables
rm(list=ls())

# set the working directory
setwd("D:/Edwisor project/Employee Absenteeism")

library(xlsx)
library(DMwR)
library(tidyr)
library("caret")
library(ggplot2)
library(car)
library(randomForest)
library(rpart)
library(corrplot)
library(DataCombine)
# Read the xls data
df=read.xlsx("Absenteeism_at_work_Project.xls",sheetName="Absenteeism_at_work")

# check the data structure
str(df)

####################### Missing value analysis ##################################
miss_val=data.frame(apply(df,2,function(x){sum(is.na(x))}))
miss_val$Columns=row.names(miss_val)
names(miss_val)[1]="Missing_Percentage"
miss_val$Missing_Percentage = (miss_val$Missing_Percentage/nrow(df)) * 100
miss_val = miss_val[order(-miss_val$Missing_Percentage),]
row.names(miss_val) = NULL
miss_val=miss_val[c(2,1)]
# plot the missing value percebnatge graph
ggplot(data = miss_val[1:18,], aes(x=reorder(Columns, -Missing_Percentage),y = Missing_Percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+
  ggtitle("Missing data percentage") + theme_bw()
###################### Impute the missing value ##################################
#df[45,18] =95
# Mean 79.04201
df$Weight[is.na(df$Weight)] = mean(df$Weight, na.rm = T)
# Median 83
df$Weight[is.na(df$Weight)] = median(df$Weight, na.rm = T)

# KNN imputation
# 95
df = knnImputation(df, k = 3)
# Check for the missing value
sum(is.na(df))
###################### Convert the categorical variable to factor #############
catagorical_vars = c("ID",'Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')
for (cat_col in catagorical_vars)
{  
  df[,cat_col]=as.factor(as.integer(df[,cat_col]))
}
########################## Outlier analysis ######################################

#selecting only numeric
num_ind = sapply(df,is.numeric) 
num_data = df[,num_ind]
colnames=colnames(num_data)
i=0
library("ggplot2")
for (i in 1:length(colnames))
{
  assign(paste0("col",i),ggplot(aes_string(y = (colnames[i]), x = "Absenteeism.time.in.hours"), data = subset(df), group = i) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    geom_boxplot(aes(group = cut_width(Absenteeism.time.in.hours, 20)),outlier.colour="red", fill = "grey" ,outlier.shape=18,
                                       outlier.size=1, notch=FALSE) +
                          theme(legend.position="bottom")+
                          labs(y=colnames[i],x="Absenteeism.time.in.hours")+
                          ggtitle(paste("Box plot of Absenteeism for",colnames[i])
                 ))
}
gridExtra::grid.arrange(col1,col2,col3,ncol=3)
gridExtra::grid.arrange(col4,col5,col6,ncol=3)
gridExtra::grid.arrange(col7,col8,col9,col10,ncol=4)

############################# Feature Selection #####################
## Correlation plot
# correlation plot to check variation amongs the numeric variables
corrplot(cor(num_data),tl.cex = 0.5, type = "full", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = rgb(0,0,0, alpha = 0.8))
# Remove the Weight variable as it is highly correlated to the Body mass index
# Remove the Age variable as it near to threashold level with service  time

## ANOVA test for Categprical variable
summary(aov(formula = Absenteeism.time.in.hours~ID,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Education,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = df))# Remove
summary(aov(formula = Absenteeism.time.in.hours~Son,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = df))# Remove

df=subset(df,select=-c(ID,Month.of.absence,Seasons,Pet,Age,Education,
                       Social.smoker,Weight,Height,Disciplinary.failure,Social.drinker))
############################ Feature Scaling ##############################
# Plot the histogram for all the variables
# Code to plot the histogram for all the numeric columns
num_ind = sapply(df,is.numeric) 
num_data = df[,num_ind]
colnames=colnames(num_data)
df[,num_ind] %>% gather() %>% head()
ggplot(gather(df[,num_ind]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# Perform the normalisation
for(i in colnames){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/(max(df[,i] - min(df[,i])))
}
###################### Split the data b/w trsain and test #######################
set.seed(1234)
train.index = createDataPartition(df$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

DTRF=rpart(Absenteeism.time.in.hours~.,data=df,method='anova')
DTpre=predict(DTRF,test[,-10])
postResample(DTpre,test[,10])
###################### Random Forest ###################################
set.seed(1234)
RFMod=randomForest(Absenteeism.time.in.hours~.,data=train,ntree = 200)
RFpre=predict(RFMod,test[,-10])
postResample(RFpre,test[,10])
##################### Linear regression model #########################
LRMod=lm(Absenteeism.time.in.hours~.,train)
vif(LRMod)
LRpre=predict(LRMod,test[,-10])
postResample(LRpre,test[,10])
##################### Work loss calculation #########################
work_loss = subset(df,select = c(Month.of.absence,Service.time,Absenteeism.time.in.hours,Work.load.Average.day.))
# Workloss/month = (absent time * workload)/service time    mathematical formula
work_loss["month.loss"]=with(work_loss,((work_loss[,"Work.load.Average.day."]*work_loss[,"Absenteeism.time.in.hours"])/work_loss[,"Service.time"]))
for (i in 1:12) {
  emp = work_loss[which(work_loss["Month.of.absence"]==i),]
  print(sum(emp$month.loss))
}

print(emp$month.loss) 