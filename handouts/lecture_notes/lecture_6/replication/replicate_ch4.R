library(AER)

library(foreign)
classdata <- read.dta("./data/caschool.dta")

head(classdata[c("observat", "district", "testscr", "str")])

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
library(xtable)
sumtab <- xtable(t(summary4.1(df)))

print(sumtab, type = "latex")

print(sumtab, type = "html")

plot(df$str, df$testscr, col = "blue", pch =16, cex = 0.7, bty = "l",
     main = "Scatterplot of Test Score vs. Student-Teacher Ratio",
     xlab = "Student-teacher ratio", ylab = "Test scores")

mod1 <- lm(testscr ~ str, data = df)
summary(mod1)

plot(df$str, df$testscr, col = "blue", pch =16, cex = 0.7, bty = "l",
     xlab = "Student-teacher ratio", ylab = "Test scores")
intercept <- coef(mod1)[1]
slope <- coef(mod1)[2]
abline(intercept, slope, col="red")
texteq <- paste("TestScore = ", round(intercept, 1), " + ",
                round(slope, 2), "STR", sep = "")
text(23.5, 655, texteq, cex.lab = 0.9, font.lab = 3)
