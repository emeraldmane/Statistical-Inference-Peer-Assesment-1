library(datasets)
library(ggplot2)
library(psych)
library(gridExtra)
library(xtable)
data(ToothGrowth)
attach(ToothGrowth)

# Columns in the data set
head(ToothGrowth)

# Statistical Summary of the data set
summary(ToothGrowth)

# Frequency of the data eliminating missing Values
table(supp,dose)

# Summary statistics by grouping as variables
dose <- as.factor(dose)
describe(len)

# Correlation between Supplement on Tooth Growth
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity",) +
  facet_grid(. ~ supp) +
  xlab("Dose in mg") +
  ylab("Tooth length") +
  guides(fill=guide_legend(title="Supplement type"))

# Effect of Dosage on Tooth Growth
suppplot<-ggplot(aes(x = supp, y = len), data = ToothGrowth) + geom_boxplot(aes(fill = supp))
doseplot<-ggplot(aes(x = factor(dose), y =len), data = ToothGrowth) + 
  geom_boxplot(aes(fill = factor(dose)))

grid.arrange(suppplot,doseplot)


# Mean length for the two supplements
round(with(ToothGrowth, sapply(split(len, supp), mean)), 3)


# Mean length for the two supplements for various doses
aggregate(len, list(supp, dose), mean)

# Standard Deviation for the two supplements for various doses
aggregate(len, list(supp, dose), sd)

# Regression to see the effect of supplements on growth
fit <- lm(len ~ dose + supp, data=ToothGrowth)
summary(fit)

# T-Test to find correlation between Orange Juice vs Vitamin Supplement

WOdose <- t.test(ToothGrowth$len~ToothGrowth$supp , var.equal = T)
WOdose.conf <- WOdose$conf[1:2]
WOdose.pvalue <- WOdose$p.value

WOdose.conf
WOdose.pvalue

dose_0.5 <- ToothGrowth[ToothGrowth$dose == 0.5,]
dose_1 <- ToothGrowth[ToothGrowth$dose == 1,]
dose_2 <- ToothGrowth[ToothGrowth$dose == 2,]

Testdose_0.5 <- t.test(dose_0.5$len~dose_0.5$supp , var.equal = T)
Testdose_1 <- t.test(dose_1$len~dose_1$supp , var.equal = T)
Testdose_2 <- t.test(dose_2$len~dose_2$supp , var.equal = T)

conf_0.5 <- Testdose_0.5$conf[1:2]
conf_1 <- Testdose_1$conf[1:2]
conf_2 <- Testdose_2$conf[1:2]

pvalue_0.5 <- Testdose_0.5$p.value
pvalue_1 <- Testdose_1$p.value
pvalue_2 <- Testdose_2$p.value

pvalue_0.5
conf_0.5

pvalue_1
conf_1

pvalue_2
conf_2


