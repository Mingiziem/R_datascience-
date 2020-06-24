install.packages("rmarkdown")
kerastuneR::install_kerastuner()
library(caTools)
library(questionr)
library(car)
library(keras)
library(dplyr)
library(purrr)
library(ggplot2)
# importing dataset alaska
df<-read.csv("/cloud/project/Data_Alaska.csv")
head(df)
df_mondya<-df[which(df$DAY_OF_WEEK==1),]
df_monday_week1<-df_mondya[which(df_mondya$DAY_OF_MONTH<=7),]
head(df_monday_week1)
plot(df)
