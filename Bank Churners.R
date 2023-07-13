setwd("C:/Users/Ramachandran/Desktop/Tableau Docs- BBL/Bank Churners")
bank=read.csv("bank_churners.csv")
str(bank)
summary(bank)
bank$still_customer=as.factor(bank$still_customer)
library(ggplot2)
library(tidyverse)
library(dplyr)
table(bank$still_customer)
ggplot(bank,aes(bank$customer_age,fill=bank$still_customer))+geom_histogram()+
  facet_grid(bank$still_customer~.)
ggplot(bank,aes(bank$still_customer,bank$dependent_count))+
  geom_boxplot()
ggplot(bank,aes(bank$education_level,bank$still_customer,fill=still_customer))+geom_bar(stat = "identity")+
  facet_wrap(bank$income_category~.)
ggplot(bank,aes(bank$marital_status,bank$still_customer,fill=still_customer))+geom_bar(stat = "identity")
ggplot(bank,aes(bank$card_category,bank$still_customer,fill=still_customer))+geom_bar(stat = "identity")
ggplot(bank,aes(bank$education_level,bank$card_category,fill=card_category))+geom_bar(stat = "identity")
ggplot(bank,aes(bank$card_category,bank$months_on_book,,fill=still_customer))+geom_boxplot()
ggplot(bank,aes(bank$still_customer,bank$total_relationship_count,fill=still_customer))+
  geom_boxplot()
ggplot(bank,aes(bank$income_category,bank$credit_limit,fill=still_customer))+
  geom_bar(stat = "identity")
ggplot(bank,aes(bank$education_level,bank$credit_limit,fill=still_customer))+
  geom_bar(stat = "identity")
ggplot(bank,aes(still_customer,bank$total_revolving_bal))+
  geom_boxplot()
ggplot(bank,aes(bank$still_customer,bank$avg_open_to_buy))+geom_boxplot()
ggplot(bank,aes(bank$still_customer,bank$total_trans_amt,fill=bank$still_customer))
ggplot(bank,aes(bank$still_customer,bank$total_trans_ct,fill=bank$still_customer))+geom_boxplot()
ggplot(bank,aes(bank$still_customer,bank$avg_utilization_ratio,fill=bank$still_customer))+geom_boxplot()
library(corrplot)
model1=glm(bank$still_customer~.,family = (binomial),bank)
summary(model1)
model2=glm(bank$still_customer~gender+dependent_count+income_category+total_relationship_count+
             months_inactive_12_mon+contacts_count_12_mon+total_revolving_bal+total_trans_amt+
             total_trans_ct+total_ct_chng_q4_q1,family=(binomial),bank)

model4=glm(bank$still_customer~dependent_count+income_category+total_relationship_count+
             months_inactive_12_mon+contacts_count_12_mon+total_revolving_bal+
             total_trans_amt+total_trans_ct+total_ct_chng_q4_q1+
             bank$months_on_book,family=(binomial),bank)
summary(model4)
p4=predict(model4,bank)
pr4=ifelse(p4>0.5,1,0)
head(pr4)
table(pr4,bank$still_customer)
