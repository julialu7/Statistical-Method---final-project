# Final Project
# Code Reviewer's name: Blinda Tian

police_employment <- read.csv("~/Desktop/police_employment.csv",stringsAsFactors=FALSE)
jc_2000 <- read.csv("~/Desktop/2000.csv", stringsAsFactors = FALSE)
jc_2001 <- read.csv("~/Desktop/2001.csv", stringsAsFactors = FALSE)
jc_2002 <- read.csv("~/Desktop/2002.csv", stringsAsFactors = FALSE)
jc_2003 <- read.csv("~/Desktop/2003.csv", stringsAsFactors = FALSE)
jc_2004 <- read.csv("~/Desktop/2004.csv", stringsAsFactors = FALSE)
jc_2005 <- read.csv("~/Desktop/2005.csv", stringsAsFactors = FALSE)
jc_2006 <- read.csv("~/Desktop/2006.csv", stringsAsFactors = FALSE)
jc_2007 <- read.csv("~/Desktop/2007.csv", stringsAsFactors = FALSE)
jc_2008 <- read.csv("~/Desktop/2008.csv", stringsAsFactors = FALSE)
jc_2009 <- read.csv("~/Desktop/2009.csv", stringsAsFactors = FALSE)
jc_2010 <- read.csv("~/Desktop/2010.csv", stringsAsFactors = FALSE)
jc_2011 <- read.csv("~/Desktop/2011.csv", stringsAsFactors = FALSE)
jc_2012 <- read.csv("~/Desktop/2012.csv", stringsAsFactors = FALSE)
jc_2013 <- read.csv("~/Desktop/2013.csv", stringsAsFactors = FALSE)
jc_2014 <- read.csv("~/Desktop/2014.csv", stringsAsFactors = FALSE)
jc_2015 <- read.csv("~/Desktop/2015.csv", stringsAsFactors = FALSE)
jc_2016 <- read.csv("~/Desktop/2016.csv", stringsAsFactors = FALSE)
jc_2017 <- read.csv("~/Desktop/2017.csv", stringsAsFactors = FALSE)
jc_2018 <- read.csv("~/Desktop/2018.csv", stringsAsFactors = FALSE)
jc_2019 <- read.csv("~/Desktop/2019.csv", stringsAsFactors = FALSE)


# NA = Crime in the United States 2000 reported no arrest counts for this State. 					
# Note: In this table the arrest rate is defined as the number of arrests of persons under age 18 for every 100,000 persons ages 10-17. Juvenile arrests (arrests of youth under age 18) reported at the State level in Crime in the United States cannot be disaggregated into more detailed age categories so that the arrests of persons under age 10 can be excluded in the rate calculation. 					

# Internet citation: OJJDP Statistical Briefing Book. Online. Available: https://www.ojjdp.gov/ojstatbb/crime/qa05103.asp?qaDate=2001. Released on  May 31, 2003.					
# Adapted from Snyder, H. (2003).  <a href=https://www.ojjdp.gov/ojstatbb/publications/StatBBAbstract.asp?BibID=201370>Juvenile Arrests 2001</a>. Washington, D.C.: Office of Juvenile Justice and Delinquency Prevention. 					

all_jc <- read.csv("~/Desktop/2000_2019_jcrate.csv",stringsAsFactors=FALSE)
dim(all_jc)

# change na and "" to 0
#for (i in 1:10) {
#  all_jc[,i][is.na(all_jc[,i])] <- 0
#  all_jc[,i][all_jc[,i]==""] <- 0
#}

df <- merge(police_employment,all_jc, by=c("State", "data_year"))
df$violent_crime_rate <- class(df$violent_crime_rate)
View(df)
write.csv(df,file = "df.csv")
new_df <- read.csv("~/Desktop/df.csv",stringsAsFactors=FALSE)
View(new_df)
dim(new_df)
class(new_df$Drug.abuse)
class(new_df$Weapons)
new_df$Drug.abuse[which(is.na(new_df$Drug.abuse))] <- 0

# Deal with outliers NA
new_df <- new_df[-which(new_df$jarrest_rate_alloffenses == 0),]

# Regression of all juvenile arrest on all police
regression_allarr <- lm(new_df$jarrest_rate_alloffenses~new_df$allpolice_rate_per_100000)
summary(regression_allarr)
plot(new_df$allpolice_rate_per_1000,new_df$jarrest_rate_alloffenses)
abline(regression_allarr)

# Regression of violent juvenile arrest on all police
regression_violent <- lm(new_df$violent_crime_rate~new_df$allpolice_rate_per_100000)
summary(regression_violent)
plot(new_df$allpolice_rate_per_1000,new_df$violent_crime_rate)
abline(regression_violent)
par(mfrow=c(2,2))
plot(regression_violent)
plot(regression_violent, which = 4)

# Two of all juvenile arrests
# correlation between two independent variables
cor(new_df$officer_rate_per100000,new_df$civillian_rate_per100000)

regression_model <- lm(new_df$jarrest_rate_alloffenses~new_df$officer_rate_per100000+new_df$civillian_rate_per100000+new_df$officer_rate_per100000:new_df$civillian_rate_per100000)
summary(regression_model)
plot(new_df$officer_rate_per100000+new_df$civillian_rate_per100000,new_df$jarrest_rate_alloffenses)
abline(regression_model)

# multiple r-squared:0.03843
first <- lm(new_df$jarrest_rate_alloffenses~new_df$officer_rate_per100000)
summary(first)
plot(new_df$officer_rate_per100000, new_df$jarrest_rate_alloffenses)
abline(first)

# multiple r-squared:0.08034
second <- lm(new_df$jarrest_rate_alloffenses~new_df$civillian_rate_per100000)    
summary(second)
plot(new_df$civillian_rate_per100000, new_df$jarrest_rate_alloffenses)
abline(second)

#Drug abuse rate:0.08519 and Weapon rate:0.1362
summary(lm(new_df$Drug.abuse~new_df$officer_rate_per100000+new_df$civillian_rate_per100000))
summary(lm(new_df$Weapons~new_df$allpolice_rate_per_100000))

# Two of all violent juvenile arrests, R-squared: 0.202 
regression_model <- lm(new_df$violent_crime_rate~new_df$officer_rate_per100000+new_df$civillian_rate_per100000+new_df$officer_rate_per100000:new_df$civillian_rate_per100000)
summary(regression_model)
plot(new_df$officer_rate_per100000+new_df$civillian_rate_per100000,new_df$violent_crime_rate)
abline(regression_model)

# R-squared:  0.1888
first <- lm(new_df$violent_crime_rate~new_df$officer_rate_per100000)
summary(first)
plot(new_df$officer_rate_per100000, new_df$violent_crime_rate)
abline(first)

# R-squared:  0.05206
second <- lm(new_df$violent_crime_rate~new_df$civillian_rate_per100000)
summary(second)
plot(new_df$civillian_rate_per100000, new_df$violent_crime_rate)
abline(second)

# control for fixed time
df_2000 <- new_df[which(new_df$data_year==2000),]
summary(lm(df_2000$violent_crime_rate~df_2000$officer_rate_per100000))
cor(df_2000$officer_rate_per100000,df_2000$civillian_rate_per100000)
summary(lm(df_2000$jarrest_rate_alloffenses~df_2000$officer_rate_per100000+df_2000$civillian_rate_per100000))
summary(lm(df_2000$violent_crime_rate~df_2000$officer_rate_per100000+df_2000$civillian_rate_per100000))


# Old Regression model
# Regression
regression <- lm(df$jarrest_rate_alloffenses~df$allpolice_rate_per_1000)
summary(regression)
plot(df$allpolice_rate_per_1000,df$jarrest_rate_alloffenses)
abline(regression)

# Two
regression_model <- lm(df$jarrest_rate_alloffenses~df$officer_rate_per_1000+df$civilian_rate_per_1000)
summary(regression_model)
plot(df$officer_rate_per_1000+df$civilian_rate_per_1000,df$jarrest_rate_alloffenses)
abline(regression_model)

first <- lm(df$jarrest_rate_alloffenses~df$officer_rate_per_1000)
summary(first)
plot(df$officer_rate_per_1000, df$jarrest_rate_alloffenses)
abline(first)

second <- lm(df$jarrest_rate_alloffenses~df$civilian_rate_per_1000)
summary(second)
plot(df$civilian_rate_per_1000, df$jarrest_rate_alloffenses)
abline(second)

# reverse direction
test1 <- lm(df$officer_rate_per100000~df$jarrest_rate_alloffenses) # more arrest => less police => no make sense
summary(test1)
plot(df$jarrest_rate_alloffenses, df$officer_rate_per100000)
abline(test1)

# R0squared too small
test2 <- lm(df$civillian_rate_per100000~df$jarrest_rate_alloffenses)
summary(test2)
plot(df$jarrest_rate_alloffenses, df$civillian_rate_per100000)
abline(test2)

# insignificant negative between juvenile offenses and all police rate
test3 <- lm(df$allpolice_rate_per_100000~df$jarrest_rate_alloffenses)
summary(test3)
plot(df$jarrest_rate_alloffenses, df$allpolice_rate_per_100000)
abline(test3)

# whole US 1980-2019 pe-arrest rate 
us8019 <- read.csv("~/Desktop/countryly_pe_jcrime80-19.csv",stringsAsFactors=FALSE )

lm_us8019 <- lm(us8019$Total_juvenile_arrest_rate~us8019$all_police_rate) # more police => less crime => not significant
summary(lm_us8019)
plot(us8019$all_police_rate, us8019$Total_juvenile_arrest_rate)
abline(lm_us8019)

# draw line chart
plot(us8019$Year,us8019$Total_juvenile_arrest_rate, type = "o", col="red",ylim = range(0:8350),ylab = "",xlab = "Year", main = "Juvenile arrest rate of all offenses vs. police employment rate trend: 1980 - 2019")
lines(us8019$Year,us8019$all_police_rate, type = "o", col="blue")
legend("topright",cex=1.5, legend=c("Juvenile Arrest Rate","Police Employment Rate"), fill=c("red","blue"))
plot(us8019$Year,us8019$all_police_rate, type = "o", col="blue",xlab = "Year",ylab = "All police employee rate", main = "All police employee rate trend: 1980 - 2019")

# two
# because two independent variables are highly correlated => plus interaction terms

acf(lm2_8019$residuals)
acf(us8019$all_officer_rate,us8019$all_civilians_rate)
cor(us8019$all_officer_rate,us8019$all_civilians_rate) # close to 1 => highly correlated
lm2_8019 <- lm(us8019$Total_juvenile_arrest_rate~us8019$all_officer_rate+us8019$all_civilians_rate+us8019$all_officer_rate:us8019$all_civilians_rate)
summary(lm2_8019)
par(mfrow=c(2,2))
plot(lm2_8019)
# Testify assumptions of regression model
# adding log of arrest rate to fix the nonlinearity issue
lm2_8019 <- lm(log(us8019$Total_juvenile_arrest_rate)~us8019$all_officer_rate+us8019$all_civilians_rate+us8019$all_officer_rate:us8019$all_civilians_rate)
summary(lm2_8019)
par(mfrow=c(2,2))
plot(lm2_8019)
par(mfrow=c(1,1))
mean(lm2_8019$residuals)
acf(lm2_8019$residuals)
cor.test(us8019$all_officer_rate,lm2_8019$residuals)
cor.test(us8019$all_civilians_rate,lm2_8019$residuals)

# Tried to fix the heteroscedasticity by using Box-Cox transformation of Y variable
arrestBCMod <- caret::BoxCoxTrans(us8019$Total_juvenile_arrest_rate)
us8019 <- cbind(us8019,arrest_new=predict(arrestBCMod,us8019$Total_juvenile_arrest_rate))
head(us8019)
new_lm2_8019 <- lm(us8019$arrest_new~us8019$all_officer_rate+us8019$all_civilians_rate+us8019$all_officer_rate:us8019$all_civilians_rate)
summary(new_lm2_8019)
par(mfrow=c(2,2))
plot(new_lm2_8019)
par(mfrow=c(1,1))
plot(lm2_8019, which = 4)
plot(us8019$all_officer_rate, us8019$Total_juvenile_arrest_rate)
abline(lm2_8019)
par(mfrow=c(1,1))


# exclude reverse causal effect
par(mfrow=c(2,2))

plot(lm(us8019$all_officer_rate~us8019$Total_juvenile_arrest_rate)) # more arrest&less police&insignificant
summary(lm(us8019$all_officer_rate~us8019$Total_juvenile_arrest_rate))
summary(lm(us8019$all_civilians_rate~us8019$Total_juvenile_arrest_rate)) # more arrest&less police&insignificant
summary(lm(us8019$all_police_rate~us8019$Total_juvenile_arrest_rate)) # more arrest&less police&insignificant


# ask for confidence intervals for the model coefficients
confint(lm2_8019, conf.level=0.95)

# Varlid regression model 
# The Y-values (or the errors, "e") are independent - knowledge of study design or data collection
# The Y-values can be expressed as a linear function of the X variable
# Variation of observations around the regression line (the residual SE) is constant (homoscedasticity)
# For given value of X, Y values (or the error) are normally distributed.




lm2_80191 <- lm(us8019$Total_juvenile_arrest_rate~us8019$all_officer_rate)
summary(lm2_80191)
lm2_80192 <- lm(us8019$Total_juvenile_arrest_rate~us8019$all_civilians_rate)
summary(lm2_80192)




