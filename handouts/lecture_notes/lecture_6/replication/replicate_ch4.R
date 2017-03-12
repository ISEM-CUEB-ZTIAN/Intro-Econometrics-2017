# This file contains the R commands for replicating the application of
# California school district test scores.

library(AER)

# Read the Excel file
library(gdata)
classdata <- read.xls("./data/caschool.xlsx", sheet = "caschool")

# Read the stata file
# library(foreign)
# classdata <- read.dta("./data/caschool.dta")

head(classdata)

df <- classdata[c("testscr", "str")]
summary(df)

# Replicate the summary statistics in Table 4.1
summary4.1 <- function(df){
    ave <- sapply(df, mean)
    std <- sapply(df, sd)
    perctile <- sapply(df, function(x)
        quantile(x, probs=c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)))
    return(rbind(ave, std, perctile))
}

sumtab <- t(summary4.1(df))

library(xtable)
xtable(sumtab)

# Scatterplot
plot(df$str, df$testscr, col = "blue", pch =16, cex = 0.7, bty = "l",
     main = "Scatterplot of Test Score vs. Student-Teacher Ratio",
     xlab = "Student-teacher ratio", ylab = "Test scores")

# correlation coefficient
cor(df$str, df$testscr)

# OLS
mod1 <- lm(testscr ~ str, data = df)
summary(mod1)

abline(coef(mod1)[1], coef(mod1)[2], col="red")
text(23.5, 655, "TestScore = 698.9 - 2.28 STR", cex.lab = 0.9, font.lab = 3)

