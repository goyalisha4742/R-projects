
#linear regression
# getwd()
# setwd('C:/Users/isha/Downloads/Data science and machine learning bootcamp (R)/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R')
df = read.csv('student-mat.csv')

# since data was seperated by ; add sep filter

df = read.csv('student-mat.csv', sep = ';')
head(df)

library(ggplot2)
library(ggthemes)
library(dplyr)


# num cols
# cor gives the correlation between differnet variables 

num.col = sapply(df, is.numeric)
num.col

cor.data = cor(df[,num.col])
cor.data

# to visualize this data install packages

install.packages('corrgram')
install.packages('corrplot')
library(corrgram)
library(corrplot)

corrplot(cor.data)
corrplot(cor.data, method = 'color')

# for corrplot had to take numeric cols for corrgram can directly pass data frame

corrgram(df)
corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df, aes(x = G3)) + geom_histogram(bins = 20, fill = 'blue', alpha = 0.5)


install.packages('caTools')
library(caTools)

# setting a seed 

set.seed(101)

# splitration 0.7 means 70% training data and 30% testing data
# can take any column doesnt matter

sample = sample.split(df$G3, SplitRatio = 0.7)

train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

head(train)
head(test)

# how to build a model in linear regression
# 
# predict y with specific columns 
# model (variable names) = lm(y ~ x1 + x2, data)
# 
# if you want to predict y with all the columns use (.) called period
# model = lm(y ~. , data)


#****************************************************************************

# creating the model

model = lm(G3 ~ . , train)

summary(model)

res = residuals(model)
res = as.data.frame(res)
head(res)

ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5)

# residual value is the predicted value vs the actual value and the difference between 
#those is essentially what we have to minimise 



G3.pred = predict(model, test)
results = cbind(G3.pred, test$G3)
colnames(results) = c('predicted', 'actual')

results = as.data.frame(results)
head(results)

# taking care of negative predictions

to_zero = function(x){
  if(x<0){
    return(0)
  } else{
    return(x)
  }
}

results$predicted = sapply(results$predicted, to_zero)
min(results)

# mean square error(MSE): how off you are from the actual values

mse = mean((results$actual - results$predicted)^2)
mse

rmse = mse^0.5
rmse

SSE = sum((results$predicted - results$actual)^2)
SST = sum((mean(df$G3) - results$actual)^2)

R2 = 1 - SSE/SST
R2
