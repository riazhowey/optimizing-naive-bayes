#### The purpose of this script is to implement a lightweight analysis of a given dataset in the same working directory
#Instructions:
#   name of dataset must be "test_df.xlsx", the last column must be named Segment_ID (same capitalization)
#   date-format values should not be included
#   the script will return 1) a basic summary of the data, 2) the accuracy of the script as a whole in predicting Segment_ID
#   and 3) optimizations of combinations of 3 and 4 of the predictors. This can take days depending on dataset size.
###### NOTE: the below packages must all be installed in the R version used to run this script. Updates could cause issues.
#contact: sb@riazhowey.com

#setwd("/Users/nyuad/Desktop/Coople")
library(openxlsx)
library(e1071)
library(caret)
library(dplyr)
library(gtools)
library(stringr)
df <- read.xlsx("test_df.xlsx", 
                colNames = TRUE,
                detectDates = FALSE)

df <- try(transform(
  df,
  age=as.integer(age),
  gender=as.factor(gender),
  edu_lvl=as.factor(edu_lvl),
  Employment_status=as.factor(Employment_status),
  #nationality=as.factor(nationality),
  #last_worked_category=as.factor(last_worked_category),
  hours_worked=as.numeric(hours_worked)
))

df <- try(transform(
  df,
  Profile_completion_score=as.numeric(Profile_completion_score),
  jp_validated=as.integer(jp_validated),
  industries_worked=as.integer(industries_worked),
  nr_of_applications=as.numeric(nr_of_applications),
  nr_of_shifts=as.numeric(nr_of_shifts),
  companies_worked=as.numeric(companies_worked),
  average_rating=as.numeric(average_rating),
  approx_earnings_CHF=as.numeric(approx_earnings_CHF),
  nr_of_logins=as.numeric(nr_of_logins),
  avg_daily_logins=as.numeric(avg_daily_logins),
  recent_shift=as.factor(recent_shift)
))

df <- transform(df,Segment_ID=as.factor(Segment_ID))



####naive bayes function
#df = input data where final column is Segment_ID
#ce = decimal indicating percentage for training partition
#l = laplace parameter (use 0 if no NAs)
nbf <- function(dff,l,ce){
  Train <- createDataPartition(y = dff$Segment_ID,p = ce,list = FALSE)
  training <- dff[Train,]
  testing <- dff[-Train,]
  x = training[,-(length(dff))]
  y = training$Segment_ID
  model = naiveBayes(x,y,laplace = l)
  result = predict(model, testing)
  #accuracy of this model is (%):
  sum(((result==testing$Segment_ID)*1)/nrow(testing))
}

str(df)
df <- df[complete.cases(df),]
str(df)

##first naive bayes overall result
nbf(df,0,.8)
#########

###now the optimization is run, a file is outputted with the mean accuracy of each combination
# finally, the highest accuracy combination is tested and that model's accuracy is returned

combins <- as.data.frame(combinations((ncol(df) - 1), 3, set=TRUE, repeats.allowed=FALSE))
avg_acc <- vector(mode='numeric',length=nrow(combins))
# 
implr <- function(row,i,j){
  tempdf <- subset(df,select= c(as.integer(combins[row,]),ncol(df)))
  v <- vector(mode="double",length=3)
  for (k in 1:3) {
    v[k] <- nbf(tempdf,i,j)
  }
  mean(v)
}
#
for (z in 1:nrow(combins)) {
  avg_acc[z] <- capture.output(implr(z,0,.8), file = NULL)
}
process <- vector("character", nrow(combins))
for (i in 1:nrow(combins)) {
  process[i] <- str_sub(avg_acc[i], start = 5)
}
avg_acc <- as.numeric(process)
###databank
comb_dat_3 <- data.frame(avg_acc)
comb_dat_3$combin <- combins
write.csv(comb_dat_3, file = 'comb_dat_3.csv', row.names = FALSE)
m <- which.max(comb_dat_3$avg_acc)
###problems probably here
sub <- subset(df, select=c(comb_dat_3[as.integer(str_sub(m, start = 5)),]))
v <- vector(mode="double",length=6)
for (k in 1:6) {
  v[k] <- nbf(sub,0,.8)
}
#optimal accuracy
mean(v)
#with
str(sub)

############
#for with four
combins <- as.data.frame(combinations((ncol(df) - 1), 4, set=TRUE, repeats.allowed=FALSE))
avg_acc <- vector(mode='numeric',length=nrow(combins))
# 
implr <- function(row,i,j){
  tempdf <- subset(df,select= c(as.integer(combins[row,]),ncol(df)))
  v <- vector(mode="double",length=3)
  for (k in 1:3) {
    v[k] <- nbf(tempdf,i,j)
  }
  mean(v)
}
#
for (z in 1:nrow(combins)) {
  avg_acc[z] <- capture.output(implr(z,0,.8), file = NULL)
}
process <- vector("character", nrow(combins))
for (i in 1:nrow(combins)) {
  process[i] <- str_sub(avg_acc[i], start = 5)
}
avg_acc <- as.numeric(process)
###databank
comb_dat_4 <- data.frame(avg_acc)
comb_dat_4$combin <- combins
write.csv(comb_dat_4, file = 'comb_dat_4.csv', row.names = FALSE)
m <- which.max(comb_dat_4$avg_acc)
###problems probably here
sub <- subset(df, select=c(comb_dat_4[as.integer(str_sub(m, start = 5)),]))
v <- vector(mode="double",length=6)
for (k in 1:6) {
  v[k] <- nbf(sub,0,.8)
}
#optimal accuracy
mean(v)
#with
str(sub)

