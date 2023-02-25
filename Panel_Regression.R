library(foreign)
library(gplots)
House.Covid <- House.Covid
View(House.Covid)
plotmeans(Sale_median ~ County, main="Heterogeineity across countries", data=House.Covid)
ols <-lm(Sale_median ~ `Total Case`, data=House.Covid)
summary(ols)

yhat <- ols$fitted
plot(House.Covid$`Total Case`, House.Covid$Sale_median, pch=19, xlab="x1", ylab="y") 
abline(lm(House.Covid$Sale_median~House.Covid$`Total Case`),lwd=3, col="red")


fixed.dum <-lm(Sale_median ~ `Total Case` + factor(County) - 1, data=House.Covid)
summary(fixed.dum)

yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~House.Covid$`Total Case`|House.Covid$County, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(House.Covid$Sale_median~House.Covid$`Total Case`),lwd=3, col="red")


library(plm)
plm(House.Covid$Sale_median~House.Covid$`Total Case`)


View(House.Covid)
summary(House.Covid$`Total Case`)
summary(House.Covid$Year)

# Include the data from the 2019, 2020, 2021
# 2020 jan - march should be 0
# China <- plm(DID = factor of confirm covid cases and actual number, x1 = Property type, x2 = )

# Introduction - problem
# database
# model

#Merge interest and inflation rate to the data by monthly


