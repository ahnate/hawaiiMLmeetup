x = rnorm(5, mean = 5, sd = 5)
hist(x)
curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE)
x = rnorm(30, mean = 5, sd = 5)
hist(x)
x = rnorm(1000, mean = 5, sd = 5)
hist(x)

house <- read.csv("ml_house_data_set.csv")

hist(house$total_sqft)

house$total_sqft <- log(house$total_sqft)
house$sale_price <- log(house$sale_price)

plot(house$total_sqft, house$sale_price)

fit <- lm(sale_price ~ total_sqft, data = house)

summary(fit)

abline(fit)

cor(house$total_sqft, house$sale_price)

boxplot(sale_price ~ has_pool, data = house)

x = house$sale_price[house$has_pool == "True"]
y = house$sale_price[house$has_pool == "False"]
t.test(x, y)

fit$residuals
mse <- mean(residuals(fit)^2)
sqrt(mse)

fit <- lm(sale_price ~ ., data = house)
library(caret)
varImp(fit)

fit <- lm(sale_price ~ poly(total_sqft, 2), data = house)
plot(house$total_sqft, house$sale_price)
points(house$total_sqft, predict(fit), col = "red")
