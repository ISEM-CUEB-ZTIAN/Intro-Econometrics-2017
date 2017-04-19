library(AER)
library(foreign)
classdata <- read.dta("caschool.dta")

# scatterplot
oldpar <- par(mfrow = c(2, 1))

plot(classdata$str, classdata$testscr, col = "red",
    main = "student-teacher ratio vs test scores",
     xlab = "Student-teacher ratio", ylab = "Test scores")

plot(classdata$el_pct, classdata$testscr, col = "blue",
     main = "English learners vs test scores",
     xlab = "Percentage of English learners",
     ylab = "Test scores")

par(oldpar)

mod1 <- lm(testscr ~ str + el_pct, data = classdata)
(sum.mod1 <- summary(mod1))

# get the compoenents
b <- coef(mod1) # coefficients
y.hat <- predict(mod1) # predicted value of y
u.hat <- resid(mod1) # residuals
SER <- sum.mod1$sigma # standard error of regression
R2 <- sum.mod1$r.squared # R squared
AR2 <- sum.mod1$adj.r.squared # adjusted R squared

(vcov.hm <- vcov(mod1)) # homoskedasticity-only covariance matrix
se.hm <- sqrt(diag(vcov.hm)) # homoskedasticity-only standard error

(vcov.ht <- vcovHC(mod1, type = "HC1")) # HCCM
se.ht <- sqrt(diag(vcov.ht)) # heterskedasticity-robust se

TSS <- with(classdata, sum((testscr - mean(testscr))^2))
ESS <- sum((y.hat - mean(y.hat))^2)
SSR <- sum(u.hat^2)

TSS == ESS + SSR

abs(TSS - ESS - SSR) < 1.0e-9

# step 1
m1 <- lm(str ~ el_pct, data = classdata)
# step 2
m2 <- lm(testscr ~ el_pct, data = classdata)
# step 3
m3 <- lm(resid(m2) ~ resid(m1) - 1)

abs(coef(m3) - b[2]) < 1.0e-10

small <- ifelse(classdata$str < 18, 1, 0)
middle <- ifelse(classdata$str >= 18 & classdata$str < 20, 1, 0)
large <- ifelse(classdata$str >= 20, 1, 0)

classsize <- ifelse(classdata$str < 18, "small",
             ifelse(classdata$str >= 18 & classdata$str < 20, "medium", "large"))
classsize <- as.factor(classsize)

mod3 <- lm(testscr ~ small + middle + large, data = classdata)
summary(mod3)

mod3.a <- lm(testscr ~ small + middle, data = classdata)
summary(mod3.a)

mod4 <- lm(testscr ~ small + middle + large - 1, data = classdata)
summary(mod4)

mod5 <- lm(testscr ~ classsize, data = classdata)
summary(mod5)
