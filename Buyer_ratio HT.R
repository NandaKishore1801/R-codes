
BuyerRatio=read.csv(file.choose())
View(BuyerRatio)

#Transpose data
rownames(BuyerRatio) = BuyerRatio$Observed.Values
BuyerRatio$Observed.Values = NULL
head(BuyerRatio)
BuyerRatio_transpose <- as.data.frame(t(as.matrix(BuyerRatio)))
BuyerRatio_transpose
BR = BuyerRatio_transpose
View(BR)

#EDA
mean(BR$Males)
median(BR$Males)

#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(BR$Males)


var(BR$Males)
sd(BR$Males)


#Measures of skewness
library(moments)

#Measures of skewness
skewness(BR$Males)


#Measures of Kurtosis 
kurtosis(BR$Males)

#Graphical Representation
boxplot(BR$Males,horizontal = TRUE)  #Boxplot
hist(BR$Males)  #Histogram
barplot(BR$Males)  #Barplot

#EDA for Females
mean(BR$Females)
median(BR$Females)

#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(BR$Females)


var(BR$Females)
sd(BR$Females)

#Measures of skewness
skewness(BR$Females)


#Measures of Kurtosis 
kurtosis(BR$Females)

#Graphical Representation
boxplot(BR$Females,horizontal = TRUE)  #Boxplot
hist(BR$Females)  #Histogram
barplot(BR$Females)  #Barplot


##### Normality Test##################
library(nortest)
ad.test(BR$Males)      ###Anderson-Darling test (Males)
ad.test(BR$Females)      ###Anderson-Darling test (Females)

########### 1 sample t-test ##########
t.test(BR$Males, mu = 0, alternative = "two.sided") # (Males)
t.test(BR$Females, mu = 0, alternative = "two.sided") # (Females)

#########Chi Square################
attach(BR)
table(Males,Females)
chisq.test(table(Males,Females))

################# Mood's Median Test #################
#install.packages("RVAideMemoire")
library(RVAideMemoire)
attach(BR)
mood.medtest(Males ~ Females,exact = FALSE)

# Significance test
attach(BR)
prop.test(x=c(1,2),n=c(3,4),conf.level = 0.95,correct = FALSE,alternative = "two.sided") # two.sided p-value = 0.6592
prop.test(x=c(1,2),n=c(3,4),conf.level = 0.95,correct = FALSE,alternative = "less") # less p-value = 0.3296

##### Insights #####
# both ("two.sided" and "less") p-value are greater than 0.05 accept null hypothesis 
# equal proportions 
