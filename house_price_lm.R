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
x <- c("SQUARE.FEET","BEDS","BATHS","LOT.SIZE","YEAR.BUILT","Violent.Crimes")
formula <- paste(y,paste(x,collapse = "+"),sep = "~")

#building model based on the formula
model <- lm(formula,data=mhp.train)

#summary model
summary(model)
coefficients(model)

#plotting model
plot(model)


# # likelihood
# mhp.train$PRED <- predict(model,newdata=mhp.train,type="response")
# mhp.test$PRED <- predict(model,newdata=mhp.test,type="response")
# loglikelihood <- function(y,py) {sum(y*log(py)+(1-y)*log(1-py))}
# y_tst0 <- mhp.test$PRED
# py_tst=na.omit(mhp.test$PRED)
# y_tst=na.omit(mhp.test$PRED+y_tst0) - py_tst
# pnull.test <- mean(y_tst)
# null.dev.test  <- -2*loglikelihood(y_tst,pnull.test)
# resid.dev.test <- -2*loglikelihood(y_tst,py_tst)
# print(null.dev.test)

# sigma <- summary(model)$sigma
#  
# # value of the likelihood with the "classical" sigma hat
# sum(log(dnorm(x = y, mean = predict(model), sd = sigma)))
# 
# # value of the likelihood with the ML sigma hat
# sigma.ML <- sigma*sqrt((n-dim(model.matrix(model))[2])/n) 
# sum(log(dnorm(x = y, mean = predict(model), sd = sigma.ML)))
logLik(model)

AIC(model)