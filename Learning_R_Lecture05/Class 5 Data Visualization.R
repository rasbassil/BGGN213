# Class 5 Data Visualization

x <- rnorm(1000)

#How many things are in x
length(x)

mean(x)
sd(x)

#Min, 1st quartile, median, mean, 3rd quartile, max
summary(x)

boxplot(x)

hist(x)

#Shows where your data is on a histogram, where your actual data points are
rug(x)

hist(x, breaks=3)

hist(x, breaks=30)

