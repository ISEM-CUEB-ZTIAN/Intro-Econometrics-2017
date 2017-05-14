library(AER)
library(foreign)
teachingdata <- read.dta("TeachingRatings.dta")
summary(teachingdata)

# simple regression
mod1 <- lm(course_eval ~ beauty, data = teachingdata)
summary(mod1)

# multiple regression
mod2 <- lm(course_eval ~ beauty + intro + onecredit + female
           + minority + nnenglish, data = teachingdata)
summary(mod2)

# FWL regressions
mod2.a <- lm(course_eval ~ intro + onecredit + female
             + minority + nnenglish, data = teachingdata)
mod2.b <- lm(beauty ~ intro + onecredit + female
             + minority + nnenglish, data = teachingdata)
mod2.c <- lm(resid(mod2.a) ~ resid(mod2.b) - 1)
summary(mod2.c)

# prediction
smith <- data.frame(minority = 1, female = 0,
                    beauty = mean(teachingdata$beauty),
                    nnenglish = 0, intro = 0, onecredit = 0)
smith.hat <- predict(mod2, smith)

b1.mod1 <- coef(mod1)[2]
b1.mod2 <- coef(mod2)[2]
b1.mod2c <- coef(mod2.c)[1]

library(stargazer)
stargazer(mod1, mod2,
          title = "The OLS Estimation of the Simple and Multiple Regressions",
          label = "tab:results_ab",
          dep.var.labels = "course_eval",
          digits = 4, no.space = TRUE)
