CountryData; Democray Index, DrugUse Correlation Tests

CountryData = read_excel(file.choose())

# Assign new names for the columns
DemocracyIndex <- (CountryData$Democracy Index)
DrugUse <- (CountryData$PopulationDrugDisorderPercantage)

#Check summary of the data(median, mean etc.)
summary(CountryData)

#Calculate Standard deviation and variance
sdDemocracy<- sd(DemocracyIndex)
sdDrugUse<- sd(DrugUse)
varDemocracy<- var(DemocracyIndex)
varDrugUse<- var(DrugUse)

#Plot; Scatter,boxplot,histogram,qq-plot
plot(DemocracyIndex)
plot(DrugUse)
plot(DemocracyIndex, DrugUse)
boxplot(DemocracyIndex)
boxplot(DrugUse)
boxplot(DemocracyIndex, DrugUse)
hist(DemocracyIndex)
hist(DrugUse)
qqnorm(DemocracyIndex)
qqnorm(DrugUse)
qqline(DemocracyIndex)
qqline(DrugUse)

#Hypothesis test I: Null hypothesis
chisq.test(DemocracyIndex, DrugUse)

#Hypothesis test II:Goal is to find if lower “DemocracyIndex” (0-6) #Countries have similar mean of “DrugUse” compared to higher Democracy #Index (6-10) Countries.
LowDemocracy<-CountryData[CountryData$DemocracyIndex<=5.99,]
HighDemocracy<-CountryData[CountryData$DemocracyIndex>=6,]
Low<-LowDemocracy[,"PopulationDrugDisorderPercentage"]
High<-HighDemocracy[,"PopulationDrugDisorderPercentage"]
t.test(High,Low, alternative = "two.sided", mu=0,conf.int=0.95)

#Shapiro-Wilk Normality Test
shapiro.test(DemocracyIndex)
shapiro.test(DrugUse)

#Anderson-Darling test
ad.test(DemocracyIndex)
ad.test(DrugUse)

#Maximum Likelihood Estimator

#DemocracyIndex
# Checking for normal
norMLE<-fitdist(DemocracyIndex,"norm",method="mle")
# Checking for gamma
gamMLE<-fitdist(DemocracyIndex,"norm",method="mle")]
# Checking for exponential
expMLE<-fitdist(DemocracyIndex,"norm",method="mle")
# Checking for Weibull
weiMLE<-fitdist(DemocracyIndex,"norm",method="mle")

#DrugUse
# Checking for normal
nor_MLE<-fitdist(DrugUse,"norm",method="mle")
# Checking for gamma
gam_MLE<-fitdist(DrugUse,"norm",method="mle")]
# Checking for exponential
exp_MLE<-fitdist(DrugUse,"norm",method="mle")
# Checking for Weibull
wei_MLE<-fitdist(DrugUse,"norm",method="mle")

#Pearson Correlation test
res <- cor.test(DemocracyIndex, DrugUse, method = "pearson")
info(res)

#Kendall's Rank Correlation test
res2 <- cor.test(DemocracyIndex, DrugUse, method="kendall")
info(res2)

#Linear Regression
model<-lm(DrugUse ~ DemocracyIndex)
summary(model)