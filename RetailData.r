#pd<-read.csv(file.choose(), header = T)
#load("finalreportrdata.Rdata")
load("finalreport.Rdata")
str(pd)
#removing the commas from the numeric variables
pd$Net.Revenue<-as.numeric(gsub(",", "", pd$Net.Revenue))
pd$Net.Qty.Sold<-as.numeric(gsub(",", "", pd$Net.Qty.Sold))
pd$ASP.Pricing<-as.numeric(gsub(",", "", pd$ASP.Pricing))
#converting ProfitMargin into numberic variable
pd$Profitability.Margin<-as.numeric(pd$Profitability.Margin)
#creating NetProfit variable
pd$NetProfit<-pd$Net.Revenue*pd$Profitability.Margin/100
#eliminating negative revenue rows
pd2<-pd[pd$Net.Revenue>=0,]
#finding and removing the observation with Not Available as customer region
which(pd2$Cust..Geography=="NOT AVAILABLE") #[1] 439
pd3<-pd2[-c(439),]
#exploring Distribution center
table(pd$Disty..Region)
dev.off()
plot(pd$Disty..Region, main="Distribution center")

#exploring Product grouping
table(pd$Product.Grouping)
plot(pd$Product.Grouping, main="Product")
length(table(pd$Product.Grouping)) #[1] 46

#exploring customer geography/countries
table(pd$Cust..Geography)
plot(pd$Cust..Geography, main="Countries")
length(table(pd$Cust..Geography)) #[1] 31

#exploring net revenue
library(psych)
describe(pd$Net.Revenue)
plot(density(pd3$Net.Revenue), main="Revenue", xlab="Revenue", ylab="Density", lwd=3)
plot(sqrt(pd3$Net.Revenue), main="Revenue", xlab="Revenue", ylab="Density", lwd=3)
plot(log(pd3$Net.Revenue), main="Revenue", xlab="Revenue", ylab="Density", lwd=3, type="l")

#testing the sampling
set.seed(123)
n<-50
rev_samp<-sample(pd3$Net.Revenue, n)
mean(pd3$Net.Revenue)-mean(rev_samp)#[1] 499201.4
median(pd3$Net.Revenue)-median(rev_samp)#[1] 6261.73

#exploring quantity.sold / Quantity of the product grouping
hist(pd3$Net.Qty.Sold, breaks = 50, main="Histogram of Quantity sold", xlab="Number of the item", ylab="Frequency", col="steelblue")

#exploring average selling price
hist(pd3$ASP.Pricing, breaks = 500, xlim = c(0, 1000), main = "Histogram of AVS price", col = "steelblue", xlab="Price", ylab="Frequency")

#exploring profitability margin
hist(pd3$Profitability.Margin, breaks = 50, col=c("steelblue", "pink"), freq=F, main="Histogram of Profit margin")
lines(density(pd3$Profitability.Margin), col="red", lwd=3)

#exploring net profit
plot(density(pd3$NetProfit), main="Net profit", xlab="Net profit", lwd=3, col = "blue", xlim=c(0, 1000000))

#Creating and assignin Country region to Customer geography
pd3$CountryRegions[pd3$Cust..Geography=="CANADA"|pd3$Cust..Geography=="UNITED STATES"]<-"CAN-USA"
sa<-c("ARGENTINA", "BRAZIL", "COLOMBIA", "VENEZUELA", "CHILE", "BOLIVIA", "PERU", "URUGUAY", "TRINIDAD & TOBAGO")
pd3$CountryRegions[pd3$Cust..Geography %in% sa]<-"SOUTHAMERICA"
nac<-c("PANAMA", "NICARAGUA", "MEXICO", "HONDURAS", "GUATEMALA", "EL SALVADOR", "COSTA RICA")
others<-c("BAHAMAS", "JAMAICA", "DOMINICAN REPUBLIC", "CAYMAN ISLANDS", "PUERTO RICO", "BARBADOS", "NETHERLANDS ANTILLES", "JAPAN", "CHINA", "AUSTRALIA", "UNITED ARAB EMIRATES")
pd3$CountryRegions[pd3$Cust..Geography %in% nac]<-"NORTHAMnoCU"
pd3$CountryRegions[pd3$Cust..Geography %in% others]<-"OTHERS"
#checking CountryRegions variable
table(pd3$CountryRegions)
#changing the names of variables into easy names
names(pd3)<-c("Year", "Center", "Product", "Country", "Revenue", "Quantity", "AveragePrice", "ProfitMargin", "NetProfit", "CountryRegion")

#exploring CountryRegion
barplot(table(pd3$CountryRegion), main = "Countries divided into regions")

#Chi-square test checking if CountryRegion and Product variables are independent
prodcregion<-table(pd3$CountryRegion, pd3$Product)
chisq.test(prodcregion)

#comparing NetProfit of distribution centers
la<-pd3$NetProfit[pd3$Center=="LATIN AMERICA"]
na<-pd3$NetProfit[pd3$Center=="NORTH AMERICA"]
#wilcox test
wilcox.test(na, la, alternative="greater")#W = 34459, p-value = 3.234e-14
#t.test on the centers
t.test(la, na, conf.level = .99)
#Plot for comparing the NetProfit of the centers
par(mfrow=c(2,1))
hist(la, col="lightgray", breaks=50, main="Center Latin America: Net profit")
abline(v=mean(la), col="red", lwd=2, lty=2)
hist(na, col="lightgray", breaks=50, main="Center North America: Net profit") 
abline(v=mean(na), col="red", lwd=2, lty=2)

aggregate(pd3$NetProfit ~pd3$CountryRegion, FUN=mean) 
# Box Plot for comparing 4 regions/segments
dev.off()
boxplot(NetProfit~CountryRegion, data=pd3, col=2:4, xlab="Country regions", main="Net profit by country regions")
#ANOVA 
regionprofit.aov <- aov(NetProfit~CountryRegion, data=pd3)
regionprofit.aov
summary(regionprofit.aov) # Null rejected.
regprof.tk<-TukeyHSD(regionprofit.aov)
round(regprof.tk$CountryRegion,2)
par(mfrow=c(2,2))
plot(regionprofit.aov) 

#comparing net profits of four country regions
canusa<-pd3[pd3$CountryRegion=="CAN-USA",]
NAnoCU<-pd3[pd3$CountryRegion=="NORTHAMnoCU",]
others<-pd3[pd3$CountryRegion=="OTHERS",]
sa<-pd3[pd3$CountryRegion=="SOUTHAMERICA",]
dev.off()
plot(log(canusa$NetProfit), main="Comparison of four regions")
lines(log(NAnoCU$NetProfit), col="blue")
lines(log(sa$NetProfit), col="red")
lines(log(others$NetProfit), col="green", lwd=2)

#Correlation matrix of numeric variables
library(car)
library(corrplot)
cormat<-round(cor(pd3[,5:9]), 2)
corrplot(cormat, method="circle", addCoef.col="grey")
pairs(~Revenue+Quantity+AveragePrice+ProfitMargin+NetProfit, data=pd3)
#correlation plots
dev.off()
scatterplotMatrix(~Quantity+AveragePrice+ProfitMargin+NetProfit|CountryRegion, data=pd3, main="Correlations by regions")
scatterplotMatrix(~Quantity+AveragePrice+ProfitMargin+NetProfit|Center, data=pd3, main="Correlations by centers")

# Correlation
dev.off()
plot(pd3$AveragePrice, pd3$NetProfit, main="Correlation between Price and Profit")
abline(lm(pd3$NetProfit ~ pd3$AveragePrice), lwd=3, col="steelblue")

# No correlation can be seen from the plot. Prove it with numeric value #for the correlation
cor(pd3$AveragePrice, pd3$NetProfit) # ~ -0.03 very close to zero.

#correlation test
cor.test(pd3$AveragePrice, pd3$NetProfit)

#Adjusted r-square from the regression that has been done is .44 so we
#remove price which is not significant to see if it would be
#a better model

mod4<-lm(NetProfit ~ Center+Quantity, data = pd3)
summary(mod4) # now Adjusted R-squared is even less which is .43
# Therefore we don't remove the price from model

#regression which includes Revenue and ProfitMarging
mod4<-lm(NetProfit ~ Center+Revenue+Quantity+AveragePrice+ProfitMargin, data = pd3)
summary(mod4)

# Perform Regression Diagnostic
# we are looking for a no pattern residual for the errors in the regression model
plot(predict(mod4), residuals(mod4))

#Regression Analysis dropping Revenue and ProfitMargin and sqrt data transformation
reglog <-lm(sqrt(NetProfit) ~ sqrt(Quantity) + sqrt(AveragePrice)+Center+CountryRegion+Product, data = pd3)
summary(reglog)

plot(predict(reglog), residuals(reglog))
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod4)
vif(mod5) 
sqrt(vif(mod5)) > 2 # if any variable is true, we would need to drop it 