library(datasets)
library(corrplot)
library(xtable)
attach(mtcars)
?mtcars

# We know that the mtcars$am variable is binomial - manual and automatic
mtcarsCor <- cor(mtcars)
sort(mtcarsCor[,1])
mtcars$am <- factor(mtcars$am)
levels(am)

# It appears that mtcars$am = 0 is the automatic transmission, while mtcars$am = 1 is manual
levels(mtcars$am) <- c("Automatic", "Manual")

#Exploratory Data Analysis
# Generate assumptions and ensure that those assumptions are met
head(mtcars)
par(mfrow = c(2,1))
plot(am, xlab = "Transmissions studied", ylab = "Number of cars", col = c("green", "blue"))
boxplot(mpg ~ am, data = mtcars, outline = TRUE, col = "gray", xlab = "Transmission Type", 
        ylab = "MPG", main = "MPG by Transmission Type of Cars Studied")

# It appears from the data that we can assume that cars with an automatic transmission
# overall have a lower MPG rating.  This is what we are attempting to determine.
# There are more automatic transmissions included in the study than there are manual transmissions

plot(mpg)
# From a simple scatter plot of the mpg, it would appear that there is a pattern in how the
# data is distributed, but after an inspection of the ordering of the dataset, it is
# determined that the pattern is just a function of how the cars are listed in the dataset

plot(density(mpg), xlab = "MPG", ylab = "Density", main = "Density plot of MPG")
abline(v = mean(mpg), col = "red", lwd = 2)
# The density plot seems to indicate that the data is not quite normally distributed.  There
# appear to be a few outliers around the 30 to 40 mpg range that is skewing the data.  Those
# data will need to be looked at closer to see if we can rule them out or remove them from 
# the analysis altogether.

# We run the Shapiro-Wilk test for normality
test <- shapiro.test(mpg)
test$p.value
# From the Shapiro-Wilk test, we fail to reject the null hypothesis that the data is normally
# distributed

qqnorm(mpg)
qqline(mpg, col = "red", lwd = 2)
# However, we can see from the QQ plot that the mpg data does not appear to be 
# normallyl distributed.  Again, it appears that there are 3 or 4 cars in the 30+ mpg range
# that are skewing the data

# We look at the data for mpg >= 30
mtcars[mpg >= 30, ]
# From this we see that all transmission types of mpg >= 30 are manual.  Since there is no
# automatic transmission included in this list, we cannot remove these data points from
# the dataset and we run the analysis with the data as it is

# Determine the expected values (mean) of each transmission type
transMean <- with(mtcars, tapply(mpg, am, FUN = mean))
transMeanDiff <- transMean[1] - transMean[2]
transMeanDiff
# We can see that automatic transmissions get on average 7.24 mpg less than manual transmisions


# We need to do a t-test to determine if the 7.24 mpg average difference is significant enough 
# to confidently state that automatic transmissions overall get less gas mileage than manual
# transmissions.  The null hypothesis is that the mean values are the same, the alternative
# hypothesis is that they are different.

autoSub <- subset(mtcars, am == "Automatic")
manualSub <- subset(mtcars, am == "Manual")

amTTest <- t.test(autoSub$mpg, manualSub$mpg)
amTTest$p.value
c(amTTest$conf.int[1], amTTest$conf.int[2])
# From the p-value, we can confidently reject the null hypothesis and determine that the
# mean values are different.

# We now need to determine if there is correlation between the different variables in the 
# dataset
sort(mtcarsCor[,1])
corrplot(mtcarsCor, method = "number", type = "upper", 
         title = "Correlation Plot of MTCARS Variables",
         diag = FALSE)
# Notice along the first row that cyl, disp, hp, wt, and carb are all negatively correlated
# with mpg, while drat(rear axle ratio), qsec(1/4 mile time), vs, am, and gear are positively 
# correlated.  This indicates that we should see an increase in mpg as the values for drat, 
# qsec, vs, am, and gear increase and, conversely, we should see a decrease in mpg as values
# for cyl, disp, hp, wt, and carb increase

# Now we fit the model
# First we fit just the am variable to see if our assumptions hold
fit <- lm(mpg ~ am - 1, data = mtcars)
fit0 <- lm(mpg ~ am, data = mtcars)
summary(fit0)
str(summary(fit))
summary(fit)$r.squared
# As we can see, our assumptions do hold, but the Multiple R-squared value is 0.3598, which
# indicates that the model only explains about 36% of the data.  A multivariate approach
# should get us a better representation of the dataset.  In other words, an increase in
# mpg is dependent upon much more than just the transmission.  We need to understand how those
# varibles affect the mpg as well.

fit2 <- lm(mpg ~ ., data = mtcars)
summary(fit2)$coef
summary(fit2)$r.squared

# From using all of the variables, we see that the Multiple R-squared value grows to 0.869,
# which means that this model represents about 87% of the data, but notice that the p-values
# for the data are all fairly high and none of them represent a good t-score.  We will need 
# to find the most descriptive variables that provide the best representation of the data.
# To do this, we use the step method in r where we step through 10000 different linear
# models to find the best variables to use.

fit3 <- step(lm(data = mtcars, mpg ~ .), direction = "both", trace = 0, steps = 10000)
summary(fit3)$coef[4]

fit3Plots <- plot(fit3)
fit3Resid <- resid(fit3)
?cor
dev.off()
par(mfrow = c(1, 2))
plot(density(fit3Resid), main = "Density of Residuals")
abline(v = mean(fit3Resid), col = "red", lwd = 2)
plot(density(mtcars$mpg), main = "Density of Datapoints")
abline(v = mean(mtcars$mpg), col = "red", lwd = 2)

# From using the step model, we see that using the wt, qsec, and am variables, we get an 
# accurate linear model that represents roughly 85% of the data.  We compare that to 
# our original model using an ANOVA:
anovaCompare <- anova(fit, fit3)
test <- xtable(anovaCompare)
test[[6]][2]
str(test)
anovaCompare[[6]][2]
str(anovaCompare)

# Notice the p-value in the second model of 1.55e-09.  This indicates that we can confidently
# reject the null hypothesis and conclude that the second model is superior.  This indicates
# that there were other factors in the model that confounded the transmission selection and
# influenced it - namely the wt and qsec variables.  From the multi-variate linear regression
# we can say that on average, a manual transmission gets about 2.94 better mpg than 
# automatic transmissions.