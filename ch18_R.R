

## ------------------------------------------------------------------------
house <- read.table('Housing_Prices_GE17.txt', sep = '\t', header = TRUE)


## ------------------------------------------------------------------------
str(house)


## ------------------------------------------------------------------------
par(mfrow = c(1,2))
plot(house$Living.Area, house$Price, xlab = 'Living Area', ylab = 'Price')
plot(factor(house$Bedrooms), house$Price, xlab = 'Bedrooms', ylab = 'Price')

par(mfrow = c(1,2))
plot(factor(house$Bathrooms), house$Price, xlab = 'Bathrooms', ylab = 'Price')
plot(factor(house$Fireplaces), house$Price, xlab = 'Fireplaces', ylab = 'Price')


## ------------------------------------------------------------------------
par(mfrow = c(1,2))
plot(house$Age, house$Price, ylab = 'Price', xlab = 'Age')
house$Age.new <- house$Age + 1
plot(log10(house$Age.new), house$Price, ylab = 'Price', xlab = 'Log10(Age.new)')


## ------------------------------------------------------------------------
imod <- lm(Price ~ Living.Area + Bedrooms + Bathrooms + Fireplaces + log10(Age.new), data = house)


## ------------------------------------------------------------------------
par(mfrow = c(1,1))
plot(imod$fitted.values, imod$residuals, xlab = 'Fitted Values', ylab = 'Residuals')
abline(0, 0)


## ------------------------------------------------------------------------
par(mfrow = c(1,2))
hist(imod$residuals, main = '', xlab = 'Residuals')
qqnorm(imod$residuals)
qqline(imod$residuals)


## ------------------------------------------------------------------------
summary(imod)


## ------------------------------------------------------------------------
summary(lm(Price ~ Bedrooms, data = house))$coeff


## ------------------------------------------------------------------------
summary(lm(Price ~ log10(Age.new), data = house))$coeff


## ------------------------------------------------------------------------
confint(imod)
confint(imod, level = 0.99)


## ------------------------------------------------------------------------
data.pred <- data.frame(Living.Area = 3000, Bedrooms = 4, Bathrooms = 2.5, 
                        Fireplaces = 1, Age.new = 4+1)


## ------------------------------------------------------------------------
predict(imod, newdata = data.pred, interval = 'prediction')

