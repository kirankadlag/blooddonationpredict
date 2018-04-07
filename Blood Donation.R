setwd("C:/Users/Sonali/Desktop/MS Subjects/Predictive Analysis/Blood Donation Project")
b.train = read.csv("Training Data.csv")
b.test1 = read.csv("Test Data.csv")

b.test = read.csv("Test Data.csv")

summary(b.train)
names(b.train)
names(b.test)

par(mfrow=c(2,2))
hist(b.train$Months.since.Last.Donation, main='Number of Month from last donation', 
     xlab='Number of Months')
hist(b.train$Number.of.Donations, main='Number of donation made by person till date',
     xlab='number of donations')
hist(b.train$Total.Volume.Donated..c.c.. , main='Volume of blood donated' ,
      xlab='Volume')
hist(b.train$Months.since.First.Donation , main='Number of months since first 
      donation' , xlab='Months')

par(mfrow=c(1,2))

boxplot(b.train$Months.since.Last.Donation, main='Number of Month from last donation', 
     xlab='Number of Months')
boxplot(b.train$Number.of.Donations, main='Number of donation made by person till date',
     xlab='number of donations')
boxplot(b.train$Total.Volume.Donated..c.c.. , main='Volume of blood donated' ,
     xlab='Volume')
boxplot(b.train$Months.since.First.Donation , main='Number of months since first 
      donation' , xlab='Months')

#================================
head(b.train)


cor(b.train)
cor(b.test)

plot(b.train[-c(1,6)])

model1 = glm(b.train$Made.Donation.in.March.2007 ~ b.train$X + 
               b.train$Months.since.Last.Donation + b.train$Number.of.Donations + 
               b.train$Total.Volume.Donated..c.c.. + b.train$Months.since.First.Donation
             ,data = b.train, family = binomial)
summary(model1)
plot(model1)
BIC(model1)
BIC(model2)
BIC(model3)

install.packages("rsq")
library('rsq')
rsq(model1)
rsq(model2)
rsq(model3)

# evaluate model fit in comparison with null model
model1Chi = model1$null.deviance - model1$deviance
chi1 = model1$df.null - model1$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.mod1 = 1 - pchisq(model1Chi, chi1)
chisq.mod1

#====================================================
# evaluate model fit in comparison with null model
model2Chi = model2$null.deviance - model2$deviance
chi2 = model2$df.null - model2$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.mod2 = 1 - pchisq(model2Chi, chi2)
chisq.mod2

#======================================================
# evaluate model fit in comparison with null model
model3Chi = model3$null.deviance - model3$deviance
chi3 = model3$df.null - model3$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.mod3 = 1 - pchisq(model3Chi, chi3)
chisq.mod3

#======================================
# evaluate model fit in comparison with null model
model1Chi = model1$null.deviance - model1$deviance
chi1 = model1$df.null - model1$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.mod1 = 1 - pchisq(model1Chi, chi1)
chisq.mod1
#====================

model3 = glm(b.train$Made.Donation.in.March.2007 ~ 
               log(b.train$Months.since.Last.Donation+1)
             + log(b.train$Number.of.Donations) +
               (b.train$Months.since.First.Donation), data = b.train, family=binomial)
summary(model3)
plot(model3)



#===================================

b.test$Made.Donation.in.March.2007 = NA
b.total = rbind(b.train, b.test)
table(b.test$Months.since.Last.Donation)
nrow(b.test)
nrow(b.train)
nrow(b.total)
head(b.total)
tail(b.total)



library(dplyr)
summary(b.total)
aa = 14 + 1.5*IQR(b.total$Months.since.Last.Donation)

summary(b.total$Number.of.Donations)
bb = 7 + 1.5*IQR(b.total$Number.of.Donations)  

b.total1 = subset(b.total, b.total$Months.since.Last.Donation < aa)
b.total2 = subset(b.total1, b.total1$Number.of.Donations < bb)
summary(b.total2)
nrow(b.total2)

b.total3 = b.total2[,-6]

model2 = glm(b.total2$Made.Donation.in.March.2007 ~ b.total2$Months.since.Last.Donation 
                     + b.total2$Number.of.Donations + b.total2$Months.since.First.Donation
                     ,data = b.total2, family = binomial())
summary(model2)
par(mfrow=c(1,2))
plot(model2)


predictTestData = predict(model2, newdata =  b.test1, type = "response")

write.csv(cbind(b.test1$X, predictTestData),file="PredictBlood1.csv")

#==============================

