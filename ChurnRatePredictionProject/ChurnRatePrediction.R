# remove all the varibales that are not required
rm(list=ls())

#Set the working directory
setwd("D:/Edwisor project")

#Load Libraries
x = c("ggplot2", "corrplot", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','class','tidyr')

lapply(x, require, character.only = TRUE)
rm(x)
#Read the data from csv ile
train_df=read.csv("train_data.csv",header=T,na.strings = c(" ", "", "NA"))
test_df=read.csv("test_data.csv",header=T,na.strings = c(" ", "", "NA"))
train_df$phone.number <- gsub('-', '', train_df$phone.number)
train_df$phone.number=as.integer(train_df$phone.number)
test_df$phone.number <- gsub('-', '', test_df$phone.number)
test_df$phone.number=as.integer(test_df$phone.number)
# See the structure of data
str(train_df)
str(test_df)

###################### Explodatory data analysis #############################
# After looking at the structure of data most of the variable are of integer
# or number data type so no need to perform univariavte analysis and variable consolidation

####################### Missing value analysis ################################
miss_val1=data.frame(apply(train_df,2,function(x){sum(is.na(x))}))
miss_val2=data.frame(apply(test_df,2,function(x){sum(is.na(x))}))
# After looking at the missing value dataframe I can clonclude the dataset has no missing value
# so no need to perfom missing value analysis any further
# let us convert the string factor into numeric datatype
for (i in 1:ncol(train_df))
{
  if(class(train_df[,i])=='factor')
  {
    train_df[,i]=factor(train_df[,i],labels=(1:length(levels(factor(train_df[,i])))))
    test_df[,i]=factor(test_df[,i],labels=(1:length(levels(factor(test_df[,i])))))
  }
}
####################### Check the outliers values #############################
# Boxplot for outlier
numcols=sapply(train_df,is.numeric)
numdata=train_df[,numcols]
colnames=colnames(numdata)
for (i in 1:length(colnames))
{
  assign(paste0("col",i), ggplot(aes_string(y = (colnames[i]), x = "Churn"), data = subset(train_df))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=colnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",colnames[i])))
}
gridExtra::grid.arrange(col1,col5,col2,ncol=3)
gridExtra::grid.arrange(col4,col5,col6,ncol=3)
gridExtra::grid.arrange(col7,col8,col9,ncol=3)
gridExtra::grid.arrange(col10,col11,ncol=2)
gridExtra::grid.arrange(col12,col13,col14,ncol=3)
gridExtra::grid.arrange(col5,col6,col7,ncol=3)
##############################################################
# Remove the outliers
for(i in numcols){
  val = train_df[,i][train_df[,i] %in% boxplot.stats(train_df[,i])$out]
  train_df = train_df[which(!train_df[,i] %in% val),]
}

for(i in colnames){
  val = train_df[,i][train_df[,i] %in% boxplot.stats(train_df[,i])$out]
  #print(length(val))
  train_df[,i][train_df[,i] %in% val] = NA
}
################## Feature select ###################
numcols=sapply(train_df,is.numeric)
# correlation plot to check variation amongs the numeric variables
corrplot(cor(train_df[,numcols]), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
corrplot(cor(test_df[,numcols]), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# After correlation plot we can drop below variable as they are highly correlated
# total.day.minutes
# total.eve.minutes
# total.night.minutes
# total.intl.minutes

# Chi-square test of independenc
fac_ind=sapply(train_df,is.factor)
fac_data=train_df[,fac_ind]
for (i in 1:4)
{
  print(names(fac_data)[i])
  print(chisq.test(table(fac_data$Churn,fac_data[,i])))
}
################### Feature scaling ###################
# Code to plot the histogram for all the numeric columns
train_df[,numcols] %>% gather() %>% head()
ggplot(gather(train_df[,numcols]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
test_df[,numcols] %>% gather() %>% head()
ggplot(gather(test_df[,numcols]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
##################### Dimension reduction ###############
test_df=subset(test_df,select=-c(state,area.code,phone.number,total.day.minutes,
                                 total.eve.minutes,total.night.minutes,total.intl.minutes))
train_df=subset(train_df,select=-c(state,area.code,phone.number,total.day.minutes,
                                   total.eve.minutes,total.night.minutes,total.intl.minutes))
#Normalisation will be performed because most of the variabled are skewed.
numdata=train_df[,numcols]
colnames=colnames(numdata)
for(i in colnames){
  print(i)
  train_df[,i] = (train_df[,i] - min(train_df[,i]))/(max(train_df[,i] - min(train_df[,i])))
  test_df[,i] = (test_df[,i] - min(test_df[,i]))/(max(test_df[,i] - min(test_df[,i])))
}

# remove the unecessary dataframes
rmExcept("custdf")
custdf=rbind(train_df,test_df)
########################## Data Modelling ########################
# Divide the data into train and test 
set.seed(1234)
train.index=createDataPartition(custdf$Churn,p=0.80,list=FALSE)
train=custdf[train.index,]
test=custdf[-train.index,]

# Decision tree classifier
dt_c50mod=C5.0(Churn ~.,train,trails=200,rules=TRUE)

#write rules into disk
write(capture.output(summary(dt_c50mod)), "c50Rules.txt")

# predict the test cases
dt_pre=predict(dt_c50mod,test[,-14],type='class')

# let us evaluate the performanc of decision tree classification model
DTCF=table(test$Churn,dt_pre)
confusionMatrix(DTCF)
# Accuracy 96
# FNR - 22.69
#    1   2
#1 841   17
#2  38  103

###################### Random Forest Algorithm ############
RFmod=randomForest(Churn ~.,train,importance=TRUE,ntree=50)
# Predict the test data
RF_pre=predict(RFmod,test[,-14])
#Evaluate the performance of Regression classification model
RFCF=table(test$Churn,RF_pre)
confusionMatrix(RFCF)
# Accuracy 95.5%
# FNR 27.65
#     1   2
# 1 852   6
# 2  39 102
######################### Logisic Regression model#################
logMod=glm(Churn~.,data=train,family="binomial")
logPred=predict(logMod,newdata=test,type='response')
logPred=ifelse(logPred > 0.5, 1, 0)

# Evaluate the performanc of confusion matrix
LogCF=table(test$Churn,logPred)
#logPred
#   0   1
#1  838  20
#2  121  20
# Accuracy 85.88
# FNR 85.81
###################### KNN prediction #########################
#predict the test data
knnpre=knn(train[,1:13],test[,1:13],train$Churn,k=5)
knnCF=table(knnpre,test$Churn)
confusionMatrix(knnCF)
#knnpre   1   2
#1       835  91
#2        23  50
# accuracy 87.69
# FNR 31.50
sum(diag(knnCF))/nrow(test)
#################### Naive bayes Model ###########################
# Develop model
NMMod=naiveBayes(Churn~.,data=train)
NBpre=predict(NMMod,test[,1:13],type="class")
nBCF=table(observed=test[,14],predicted=NBpre)
confusionMatrix(nBCF)
#          predicted
#observed   1   2
#1         840  18
#2         107  34
# Accuracy 87.29
# FNR  77.30
