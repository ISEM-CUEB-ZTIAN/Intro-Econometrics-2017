library(AER)
library(foreign)
classdata <- read.dta("caschool.dta")

df2use <- classdata[c("testscr", "str")]
mod1 <- lm(testscr ~ str, data = df2use)
summary(mod1)

class(mod1)
str(mod1, max.level=1, give.attr = FALSE)

b <- coef(mod1)
b

b1 <- b[2]

V <- vcov(mod1)
se_b1 <- sqrt(V[2, 2]); se_b1

htV <- vcovHC(mod1, type = "HC1")
se_b1_rb <- sqrt(htV[2, 2]); se_b1_rb

(t_b1_rb <- b1 / se_b1_rb)

(c.5 <- qnorm(0.975))

(pval <- 2 * pnorm(-abs(t_b1_rb)))

coeftest(mod1)

# coeftest returns a matrix
t_tst <- coeftest(mod1, vcov. = htV); t_tst

## Get the t-statistic for STR
t_b1 <- t_tst["str", "t value"]; t_b1

# confidence interval with the default homoskedasticity-only SE
confint(mod1, "str")

get_confint_rb <- function(lm_obj, param, vcov_ = vcov(lm_obj),
                           level = 0.05){
    ## This function generates a two-sided confidence interval for a
    ## parameter in the linear regression model with a specified
    ## covariance matrix.  The inputs The output

    ## get all the parameters' names and select one based on param
    all_param <- names(coef(lm_obj))
    which_param <- grep(param, all_param)

    ## get the estimated parameter and its standard error
    bhat_param <- coef(lm_obj)[which_param]
    sd_param <- sqrt(vcov_[which_param, which_param])

    ## get the critical value
    cv <- qnorm(1 - level/2)

    ## calculate the confidence interval
    lower <- bhat_param - cv * sd_param
    upper <- bhat_param + cv * sd_param

    conf_interval <- c(lower, upper)
    names(conf_interval) <- c("lower", "upper")
    return(conf_interval)
}

get_confint_rb(mod1, param = "str", vcov = htV)

smallclass <- ifelse(df2use$str < 20, 1, 0)

mod2 <- lm(testscr ~ smallclass, data = df2use)
coeftest(mod2, vcov. = vcovHC(mod2, type = "HC1"))

plot(smallclass, df2use$testscr,
     xlab = "class size", ylab = "test score")

smallclass_jittered <- jitter(smallclass, amount = 0.05)
plot(smallclass_jittered, df2use$testscr, xlim = c(-0.5, 1.5),
     xlab = "class size", ylab = "test score")
abline(coef(mod2)[1], coef(mod2)[2], col = "blue")
