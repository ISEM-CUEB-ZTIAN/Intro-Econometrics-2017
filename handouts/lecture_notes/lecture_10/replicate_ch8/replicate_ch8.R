library(AER)
# read data
library(foreign)
classdata <- read.dta("./data/caschool.dta")

var2use <- c("testscr", "str", "el_pct", "meal_pct", "avginc")
df2use <- classdata[var2use]
sumdf <- summary(df2use)

varlabs <- c(
    "Average test scores",
    "Student-teacher ratio",
    "Percent of English learners",
    "Percent of students eligible for subsidized lunch",
    "Average district income"
)

library(stargazer)
stargazer(df2use, title = "Descriptive Statistics of All Variables",
          covariate.labels = varlabs,
          summary.stat = c("max", "mean", "median", "min", "sd"),
          digits = 2,
          label = "tab:destab"
)

plot(testscr ~ str, data = df2use,
     main = "The scatterplot of test scores against student-teacher ratios",
     xlab = "Student-teacher ratio", ylab = "Test scores",
     bty = "l", col = "blue")

hiel <- ifelse(df2use$el_pct > 10, TRUE, FALSE)
table(hiel)

fm1 <- testscr ~ str + el_pct + meal_pct
fm2 <- testscr ~ str + el_pct + meal_pct + log(avginc)
fm3 <- testscr ~ hiel*str
fm4 <- testscr ~ hiel*str + meal_pct + log(avginc)
fm5 <- testscr ~ str + I(str^2) + I(str^3) + hiel + meal_pct + log(avginc)
fm6 <- testscr ~ hiel*str + hiel*I(str^2) + hiel*I(str^3) + meal_pct + log(avginc)
fm7 <- testscr ~ str + I(str^2) + I(str^3) + el_pct + meal_pct + log(avginc)

fm.ls <- mget(paste("fm", 1:7, sep = ""))
allols <- function(x) lm(x, data = df2use)
ols.all <- lapply(fm.ls, allols)

coef.all <- lapply(ols.all, coef)
hccm.all <- lapply(ols.all, vcovHC, type = "HC1")
seht.all <- lapply(hccm.all, function(x) sqrt(diag(x)))

indep.labels <- c("$STR$", "$STR^2$", "$STR^3$",
                  "Percent of English learner",
                  "High Percent of English learner",
                  "$HiEL \\times STR$", "$HiEL \\times STR^2$",
                  "$HiEL \\times STR^3$", "Percent of eligible for free lunch",
                  "Average district income")

stargazer(ols.all, title = "Nonlinear regression models of test scores",
          coef = coef.all, se = seht.all,
          covariate.labels = indep.labels,
          dep.var.caption = "Dependent variable: Average test scores",
          dep.var.labels.include = FALSE,
          no.space = TRUE, df = FALSE,
          order = c(2, 4, 5, 3, 1, 8, 9, 10, 6, 7, 11),
          float.env = "sidewaystable",
          label = "tab:tab83")

testSTR23 <- function(ols.res, vcov.hc){
    test <- linearHypothesis(ols.res, c("I(str^2) = 0", "I(str^3) = 0"),
                             vcov. = vcov.hc)
    fstat <- test[2, 3]
    pval <- test[2, 4]
    return(list(Fstat = fstat, Pval = pval, Test = test))
}

F5 <- testSTR23(ols.all[[5]], hccm.all[[5]])
F6 <- testSTR23(ols.all[[6]], hccm.all[[6]])
F7 <- testSTR23(ols.all[[7]], hccm.all[[7]])

str.sim <- with(df2use, seq(min(str), max(str), by = 0.05))
n.sim <- length(str.sim)
means <- lapply(df2use, mean)
means$hiel <- ifelse(mean(hiel) > 0.5, TRUE, FALSE)
newdf <- data.frame(str = str.sim,
                    el_pct = rep(means$el_pct, n.sim),
                    meal_pct = rep(means$meal_pct, n.sim),
                    avginc = rep(means$avginc, n.sim),
                    hiel = rep(means$hiel, n.sim))

yhat.2 <- predict(ols.all[[2]], newdata = newdf)
yhat.5 <- predict(ols.all[[5]], newdata = newdf)
yhat.7 <- predict(ols.all[[7]], newdata = newdf)

plot(testscr ~ str, data = df2use,
     xlab = "Student-teacher ratio", ylab = "Test scores",
     bty = "l", col = "gray")
lines(yhat.2 ~ str.sim, col = "black", lwd = 1.5)
lines(yhat.5 ~ str.sim, col = "red", lwd = 1.5)
lines(yhat.7 ~ str.sim, col = "blue", lty = 2, lwd = 1.5)
legend("topright", c("Linear regression (2)",
                     "Cubic regression (5)",
                     "Cubic regression (7)"),
       col = c("black", "red", "blue"),
       lty = c(1, 1, 2))

F6.inter <- linearHypothesis(ols.all[[6]],
                             c("hielTRUE:str=0", "hielTRUE:I(str^2)=0",
                               "hielTRUE:I(str^3)=0"), vcov. = hccm.all[[6]])

# plot Figure 8.11
df2use.a <- df2use[hiel, ]
df2use.b <- df2use[!hiel, ]

newdf$hiel <- TRUE
yhat.6.T <- predict(ols.all[[6]], newdata = newdf)

newdf$hiel <- FALSE
yhat.6.F <- predict(ols.all[[6]], newdata = newdf)

plot(testscr ~ str, data = df2use.a,
     xlab = "Student-teacher ratio", ylab = "Test scores",
     bty = "l", col = "gray")
points(testscr ~ str, data = df2use.b, col = "orange")
lines(yhat.6.T ~ str.sim, col = "blue", lwd = 1.5)
lines(yhat.6.F ~ str.sim, col = "red", lwd = 1.5, lty = 2)
legend("topright", legend = c("High EL", "Low EL"),
       pch = c(1, 1), col = c("gray", "orange"))
legend(18, 615, legend = c("Regression with HiEL=1",
                                 "Regression with HiEL = 0"),
       col = c("blue", "red"), lty = c(1, 2))
