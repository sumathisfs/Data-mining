
# Purpose : This code will load and merge house price data, build, train and compare multiple regression models


#Loading data 
library(stats4)
library(bbmle)
library(Matrix)
hp <- read.csv("metro_sales_updated.csv")
vc <-read.csv("ViolentCrimeRates.csv")

# Removing unwanted columns from Metro 
hp$SOLD.DATE=NULL
hp$ZIP=NULL
hp$LATITUDE=NULL
hp$LONGITUDE=NULL
# Removing unwanted columns from Violent Crimepar(mfrow=c(1, 2)) 
vc$Aggravated.Assault.Rate.Per.100.000.People=NULL
vc$Robbery.Rate.Per.100.000.People=NULL
vc$Rape.Rate.Per.100.000.People=NULL
vc$Murder.Rate.Per.100.000.People=NULL
vc$Violent.Crime.Rate.Per.100.000.People=NULL
vc$Population=NULL

#Renaming Column to match on both the dataset
names(vc)[1] <-"CITY"

# Merging house price and crime datasets and storing into new one
mhp <- merge(hp,vc,by=c("CITY"))

#Omitting NA records
mhp <-na.omit(mhp)

#Factoring property type
mhp$PROPERTY.TYPE <- factor(mhp$PROPERTY.TYPE)
mhp$Latitude=NULL
mhp$Longitude=NULL

#setting seed
set.seed(1234)

#splitting data set into training 70% and test 30%
mhp_train <- sample(nrow(mhp), 0.7*nrow(mhp))
mhp.train <- mhp[mhp_train,]
mhp.test <- mhp[-mhp_train,]

# Setting up variables for Linear Regression
y <- "PRICE"
x <- c("SQUARE.FEET","BEDS","BATHS","YEAR.BUILT","Violent.Crimes")
formula <- paste(y,paste(x,collapse = "+"),sep = "~")

#building model based on the formula
ln_mod <- lm(formula,data=mhp.train)

#summary ln_mod
summary(ln_mod)
coefficients(ln_mod)

#Setting plot parameters
par(mfrow=c(2,2))

#plotting ln_mod
plot(ln_mod)

#Log likelihood of linear reg model
logLik(ln_mod)

#AIC of linear regression model
AIC(ln_mod)

# building Polynomial model of Degree 2
poly2_mod <- lm(mhp.train$PRICE ~ poly(mhp.train$SQUARE.FEET, degree=2, raw=TRUE) +
                  poly(mhp.train$BEDS, degree=2, raw=TRUE) +
                  poly(mhp.train$BATHS, degree=2, raw=TRUE) +
                  poly(mhp.train$YEAR.BUILT, degree=2, raw=TRUE) +
                  poly(mhp.train$Violent.Crimes, degree=2, raw=TRUE))

#summary poly2_mod
summary(poly2_mod)

#Log likelihood of polynomial regression model of degree 2
logLik(poly2_mod)

#AIC of polynomial regression model of degree 2
AIC(poly2_mod)

#plotting poly2_mod
plot(poly2_mod,pch=18,col="green")

# building Polynomial model of Degree 3
poly3_mod <- lm(mhp.train$PRICE ~ poly(mhp.train$SQUARE.FEET, degree=3, raw=TRUE) +
     poly(mhp.train$BEDS, degree=3, raw=TRUE) +
     poly(mhp.train$BATHS, degree=3, raw=TRUE) +
     poly(mhp.train$YEAR.BUILT, degree=3, raw=TRUE) +
     poly(mhp.train$Violent.Crimes, degree=3, raw=TRUE))

#summary poly3_mod
summary(poly3_mod)

#Log likelihood of polynomial regression model of degree 3
logLik(poly3_mod)

#AIC of polynomial regression model of degree 3
AIC(poly3_mod)

#plotting poly3_mod
plot(poly3_mod,pch=18,col="blue")


