library(dplyr)
library(Metrics)
dataset = read.csv('bank-full_train.csv',stringsAsFactors = FALSE)

dtest= read.csv('bank-full_test.csv',stringsAsFactors = FALSE)

str(dataset)
glimpse(dataset)



table(dataset$job)

dataset=dataset%>%
  mutate(job_admin=as.numeric(job=="admin."),
  job_blue_collar=as.numeric(job=="blue-collar"),
  job_entrepreneur=as.numeric(job=="entrepreneur"),
  job_housemaid=as.numeric(job=="housemaid"),
  job_management=as.numeric(job=="management"),
  job_retired=as.numeric(job=="retired"),
  job_self_employed=as.numeric(job=="self-employed"),
  job_services=as.numeric(job=="services"),
  job_student=as.numeric(job=="student"),
  job_technician=as.numeric(job=="technician"),
  job_unemployed=as.numeric(job=="unemployed")) %>%
    select(-job)


table(dataset$marital)

dataset=dataset%>%
  mutate(marital_married=as.numeric(marital=="married"),
         marital_single=as.numeric(marital=="single")) %>%
  select(-marital)


table(dataset$education)

dataset=dataset%>%
  mutate(edu_primary=as.numeric(education=="primary"),
         edu_tertiary=as.numeric(education=="tertiary"),
         edu_secondary=as.numeric(education=="secondary")) %>%
  select(-education)


table(dataset$housing)

dataset=dataset%>%
  mutate(housing_=as.numeric(housing=="yes")) %>%
  select(-housing)


table(dataset$default)

dataset=dataset%>%
  mutate(default_=as.numeric(default=="yes")) %>%
  select(-default)


table(dataset$loan)

dataset=dataset%>%
  mutate(loan_=as.numeric(loan=="yes")) %>%
  select(-loan)


table(dataset$contact)

dataset=dataset%>%
  mutate(contact_cellular=as.numeric(contact=="cellular"),
         contact_unknown=as.numeric(contact=="unknown")) %>%
  select(-contact)


table(dataset$month)

dataset=dataset%>%
  mutate(month_apr=as.numeric(month=="apr"),
         month_aug=as.numeric(month=="aug"),
         month_dec=as.numeric(month=="dec"),
         month_feb=as.numeric(month=="feb"),
         month_jan=as.numeric(month=="jan"),
         month_jul=as.numeric(month=="jul"),
         month_mar=as.numeric(month=="mar"),
         month_may=as.numeric(month=="may"),
         month_nov=as.numeric(month=="nov"),
         month_oct=as.numeric(month=="oct")) %>%
  select(-month)


table(dataset$poutcome)

dataset=dataset%>%
  mutate(pout_failure=as.numeric(poutcome=="failure"),
         pout_other=as.numeric(poutcome=="other"),
         pout_unknown=as.numeric(poutcome=="unknown")) %>%
  select(-poutcome)





table(dataset$y)

dataset=dataset%>%
  mutate(y=as.numeric(y=="yes"))



# removing outlayers

boxplot(dataset$balance)
summary(dataset$balance)
ul <- 1414 + 1.5*(1414 - 72)
dataset$balance <- ifelse(dataset$balance > ul, ul, dataset$balance)
boxplot(dataset$balance)
ll <- 72 - 1.5*(1414-72)
dataset$balance <- ifelse(dataset$balance < ll, ll, dataset$balance)
boxplot(dataset$balance)




boxplot(dataset$age)
summary(dataset$age)
ul <- 48 + 1.5*(48 - 33)
dataset$age <- ifelse(dataset$age > ul, ul, dataset$age)
boxplot(dataset$age)
ll <- 33 - 1.5*(1414-72)
dataset$age <- ifelse(dataset$age < ll, ll, dataset$age)
boxplot(dataset$age)


boxplot(dataset$duration)
summary(dataset$duration)
ul <- 320 + 1.5*(320 - 103)
dataset$duration <- ifelse(dataset$duration > ul, ul, dataset$duration)
boxplot(dataset$duration)
ll <- 103 - 1.5*(320 - 103)
dataset$duration <- ifelse(dataset$duration < ll, ll, dataset$duration)
boxplot(dataset$duration)


boxplot(dataset$campaign)
summary(dataset$campaign)
ul <- 3 + 1.5*(3 - 1)
dataset$campaign <- ifelse(dataset$campaign > ul, ul, dataset$campaign)
boxplot(dataset$campaign)





# 
str(dataset)
library(car)
for_vif <- lm(y~.-pdays-previous-ID-job_blue_collar-edu_secondary-contact_unknown-pout_unknown
              , data = dataset)
sort(vif(for_vif),decreasing = T)[1:3]


fit=glm(y~.-pdays-previous-ID-job_blue_collar-edu_secondary-contact_unknown-pout_unknown
        , family= "binomial",data = dataset)

fit=step(fit)

summary(fit)

dataset$y_=predict(fit,newdata=dataset,type="response")
library(ggplot2)
ggplot(dataset,aes(x=y_,y=y,color=factor(y)))+geom_point()+geom_jitter()



library(ROCR)

ROCRPred <- prediction(dataset$y_, dataset$y)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(.1, by = 0.1))
abline(a=0, b=1)

res <- predict(fit, dataset, type = "response")

PredictedValue <- res>.1

head(PredictedValue)
pv <- as.numeric(PredictedValue)

head(pv)
pv <- as.factor(pv)
dataset$y <- as.factor(dataset$y)
library(caret)
confusionMatrix(pv, dataset$y)




