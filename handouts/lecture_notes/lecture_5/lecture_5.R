## This file contains all code to generate Lecture notes 5.

# Install packages
# install.packages(c("AER", "foreign"))

# Load packages
library(AER)

# Check packages loaded
search()

## R as a calculator ------------------------------

#+ Binary operations
1 + 2; 2*3; 2^3; 5/2;
5 %% 2  # get x mod y
5 %/% 2 # get the integer division

# Use built-in functions
log(exp(sin(pi/2)^2) * exp(cos(pi/3)^2))

## Vector operations ------------------------------

# Create a vector with c()
x <- c(0.3, 1.5, 7.3, 2)
y <- c(3, 2, 1)
z <- c(x, y)
z

# Vectors with different data types
student.names <- c("John", "Mary", "Bob", "Ann")
student.male <- c(TRUE, FALSE, TRUE, FALSE)
student.age <- c(20, 19, 21, 20)

class(student.names)
class(student.male)
class(student.age)

students <- c(student.names, student.male, student.age)
students

# Create a sequence
even <- seq(from = 2, to = 20, by = 2)
even
years <- 1995:2005
years

# Create repetition
ones <- rep(1, times = 10)
ones
rep13 <- rep(1:3, times = 3, each = 2)
rep13

# Draw a random vector
x <- runif(10); x
length(x)

2 * x + 3
log(x)

y <- runif(5)
x + y

# Selecting elements in a vector
x[1:5]
x[c(1, length(x))]
x[-4]
x[x > 0.5]

student.names
student.age
# Give elements names
names(student.age) <- student.names
student.age
student.age[c("John", "Bob")]

# Create a matrix
A <- matrix(1:12, nrow = 3, ncol = 4); A
matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)

# Create a matrix by combining vectors
a <- 1:4; b <- 2:5; c <- 3:6
cbind(a, b, c)
rbind(a, b, c)

# Give names to rows and columns
rownames(A) <- paste("X", 1:3, sep = "")
colnames(A) <- paste("Y", 1:4, sep = "")
A

# Selecting elements in a matrix
A[1, 3]
A["X1", "Y3"]
A[1:3, c(2, 4)]
A[, 2]
A[3, ]

t(A)

A %*% B

A <- matrix(rnorm(9), nrow = 3)
B <- solve(A)
A %*% B

A <- cbind(c(3, 2, -1),
           c(2, -2, 0.5),
           c(-1, 4, -1))
B <- c(1, -2, 0)
solve(A, B)

diag(1:3)

diag(3)

array(1:18, dim = c(3, 3, 2))

mylist <- list(chr = c("a", "b", "c", "d"),
	       num = 1:10,
	       boo = c(TRUE, FALSE, FALSE, TRUE))
mylist

mylist$chr
mylist[[2]][3:6]
mylist[["boo"]][-1]

mydata <- data.frame(X = 1:5, Y = letters[1:5], Z = rep(c(TRUE, FALSE), length = 5)); mydata

A <- matrix(sample.int(100, size = 20), nrow = 5)
A.df <- as.data.frame(A); A.df

names(A.df) <- paste("VAR", 1:4, sep = "_"); A.df

mydata <- read.table("mydata.txt", header = TRUE, sep = "")
head(mydata)
# tail(mydata)

tail(read.csv("mydata.csv", header = TRUE))

mean(mydata$Weight)

str(mydata)
summary(mydata)

data(mtcars)
head(mtcars)
# str(mtcars)

barplot(sort(mtcars$mpg, decreasing = TRUE),
        col = "blue",
        main = "The mpg among car models",
        xlab = "car models", ylab = "mpg")

plot(mtcars$wt, mtcars$mpg,
     main = "The scatterplot between mpg and displacement",
     xlab = "Car weights (lbs/1000)",
     ylab = "Miles per gallon",
     pch = 19, col = "red")


## Empirical exercise 3.1 ------------------------------

library(foreign)
cpsdat <- read.dta("data/cps92_08.dta")
head(cpsdat)

# extract the data for average hourly earnings in 1992 and 2008
ahe.92 <- cpsdat$ahe[cpsdat$year == 1992]
ahe.08 <- cpsdat$ahe[cpsdat$year == 2008]
mean.ahe.92 <- mean(ahe.92); mean.ahe.92
mean.ahe.08 <- mean(ahe.08); mean.ahe.08

# the sample variance
sd.ahe.92 <- sd(ahe.92)
sd.ahe.08 <- sd(ahe.08)

n.92 <- length(ahe.92)
n.08 <- length(ahe.08)

# the standard error
se.ahe.92 <- sd.ahe.92 / sqrt(n.92)
se.ahe.08 <- sd.ahe.08 / sqrt(n.08)

# 95% confidence interval
# the 95% critical value from a normal distribution
cv.95 <- qnorm(0.975)

lower.lim.92 <- mean.ahe.92 - cv.95 * se.ahe.92
lower.lim.08 <- mean.ahe.08 - cv.95 * se.ahe.08

upper.lim.92 <- mean.ahe.92 + cv.95 * se.ahe.92
upper.lim.08 <- mean.ahe.08 + cv.95 * se.ahe.08

# Use aggregate() to compute the means in both years
ahe.means <- aggregate(ahe ~ year, FUN = mean, data = cpsdat)
ahe.means

# t test for ahe in 1992
t.ahe.92 <- t.test(ahe.92); t.ahe.92$conf.int
# t test for ahe in 2008
t.ahe.08 <- t.test(ahe.08); t.ahe.08$conf.int
# test for the change between 1992 and 2008
t.ahe.diff <- t.test(ahe.08, ahe.92); t.ahe.diff

# CPI in 1992 and 2008
cpi.92 <- 140.3
cpi.08 <- 215.2
# Inflation adjustment
inflator <- cpi.08 / cpi.92
cpsdat$ahe.adj <- with(cpsdat, ifelse(year == 1992, ahe * inflator, ahe))

ahe.92.adj <- with(cpsdat, ahe.adj[year == 1992])
mean.ahe.92.adj <- mean(ahe.92.adj)
t.ahe.92.adj <- t.test(ahe.92.adj)
t.ahe.diff.adj <- t.test(ahe.08, ahe.92.adj)

# select data in 2008
cps08 <- subset(cpsdat, year == 2008, select = c(year, ahe, bachelor))

# calculate means
ahe.educ.08 <- aggregate(ahe ~ bachelor, FUN = mean, data = cps08)

# select ahe and filter by bachelor
ahe.high.08 <- with(cps08, ahe[bachelor == 0])
ahe.bach.08 <- with(cps08, ahe[bachelor == 1])

# construct confindence interval
t.ahe.high.08 <- t.test(ahe.high.08)
t.ahe.bach.08 <- t.test(ahe.bach.08)
t.ahe.gap.08  <- t.test(ahe.bach.08, ahe.high.08)

boxplot(ahe ~ bachelor, data = cps08,
        main = "Average Hourly Earnings by Education",
        col = c("red", "orange"),
        xlab = "Bachelor degres = 1, high school = 0",
        ylab = "US$ 2008")
