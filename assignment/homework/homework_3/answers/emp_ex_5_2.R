library(foreign)
teachingdata <- read.dta("./data/TeachingRatings.dta")

head(teachingdata)

str(teachingdata)

summary(teachingdata)

library(stargazer)
stargazer(teachingdata,
  title = "Summary Statistics", label = "tab:sum-stats")

teaching.formula <- course_eval ~ beauty
plot(teaching.formula, data = teachingdata,
   main = "The Scatterplot of Course Evaluation on Professor's Beauty",
   xlab="Beauty", ylab = "Course evaluation", col = "blue")

# run a regression of course evaluation on professor's beauty
teaching.ols <- lm(teaching.formula, data = teachingdata)

# create the latex table
stargazer(teaching.ols,
  covariate.labels = c("Prof. Beauty"),
  dep.var.labels = c("Course Evaluations"),
  title = "The OLS Estimation of the Regression of Course Evaluation on Beauty",
  label = "tab:ols-1", single.row = TRUE, omit.stat = c("adj.rsq", "f")
)

beauty.watson <- mean(teachingdata$beauty)
beauty.stock <- mean(teachingdata$beauty) + sd(teachingdata$beauty)
ave.courseval <- mean(teachingdata$course_eval)
# do prediction step by step
b0 <- teaching.ols$coef[1]
b1 <- teaching.ols$coef[2]
courseval.predict <- b0 + b1 * c(beauty.watson, beauty.stock)
names(courseval.predict) <- c("waston", "stock")

beauty.sd <- sd(teachingdata$beauty)
courseval.sd <- sd(teachingdata$course_eval)
delta.courseval <- b1 * beauty.sd

rsq <- summary(teaching.ols)$r.squared
