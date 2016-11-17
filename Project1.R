library(datasets)
attach(mtcars)

# We know that the mtcars$am variable is binomial - manual and automatic
mtcars$am <- factor(mtcars$am)
levels(am)

# It appears that mtcars$am = 0 is the automatic transmission, while mtcars$am = 1 is manual
levels(mtcars$am) <- c("Automatic", "Manual")

#Exploratory Data Analysis
# Generate assumptions and ensure that those assumptions are met
head(mtcars)

plot(am, xlab = "Transmissions studied", ylab = "Number of cars", col = c("green", "blue"))
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
shapiro.test(mpg)
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

boxplot(mpg ~ am, data = mtcars, outline = TRUE, col = "gray", xlab = "Transmission Type", 
        ylab = "MPG", main = "MPG by Transmission Type of Cars Studied")

# It appears from the data that we can assume that cars with an automatic transmission
# overall have a lower MPG rating.  This is what we are attempting to determine.

# Determine the expected values (mean) of each transmission type
transMean <- with(mtcars, tapply(mpg, am, FUN = mean))
transMeanDiff <- transMean[1] - transMean[2]


