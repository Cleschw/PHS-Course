# Thursday basic introduction in R 
# data/raw/
This folder contains your raw data sets, such as CSV files or Excel spreadsheets. These files should never be edited.

An example for the content of this folder looks like this:
- original_data.csv
- spreadsheet_data.xlsx

setwd("C:/Users/cleos/OneDrive/Universität Bern/Kurse/R_project_template/data/raw")
# create dataset
id<-c(1,2,3,4,5)
id<-1:5
id<-seq(1,5,1)
id2<-seq(1,10,0.1)

names<-c("Mark","Jack","Jill","Anna", "Tom")

gender<-c(0,0,1,1,0)

a<-1:5
b<-6:10
mat<-cbind(a,b)
mat<-rbind(a,b)

#make a data frame -> vector need to be same length 
dat<-data.frame(ID=id, Name=names, Gender=gender)

dat[1,]
dat[2,]
dat[,2]
names[2]
dat[,c("ID", "Gender")]
# $ to adress every item inthere 
dat$ID
dat$Gender

sel<-dat$Gender == 1
# to get the girls 
dat[sel,]

#open data and assign it to sth
setw <- 
data<-read.csv("perulung_ems.csv")

str(data)
head(data)
tail(data)

view(data)
View(data)
summary(data)

data$sex_f<-factor(data$sex,levels=c(0,1),labels=c("f","m"))

table(data$sex)
table(data$sex_f)

summary(data$sex_f)

#now we work with tidyverse (before it was basic R)
library("tidyverse")
data %>% select(sex, sex_f) %>% filter(sex_f=="m") %>% table

mean(6,8,11,3,5,6)
median(6,8,11,3,6)
median(12,145,26)

