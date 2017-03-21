## ---- include=FALSE, results='hide'--------------------------------------
  library(AER)

## ----readdata, echo=TRUE, results='hide'---------------------------------
    library(foreign)
    classdata <- read.dta("caschool.dta")

## ---- echo=TRUE----------------------------------------------------------
  head(classdata[c("observat", "district", "testscr", "str")])

## ------------------------------------------------------------------------
  df <- classdata[c("testscr", "str")]
  summary(df)

## ---- echo=TRUE, results='hide'------------------------------------------
  # Replicate the summary statistics in Table 4.1
  summary4.1 <- function(df) {
    ave <- sapply(df, mean)
    std <- sapply(df, sd)
    perctile <- sapply(df, function(x)
    quantile(x, probs = c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)))
    return(rbind(ave, std, perctile))
  }
  library(xtable)
  sumtab <- xtable(t(summary4.1(df)))

## ---- results='asis', include=TRUE---------------------------------------
  # print as a latex table
  print(sumtab, type = "latex")

## ---- results='asis', include=FALSE--------------------------------------
  # print out as an html table
  print(sumtab, type = "html")

## ---- fig.align='center'-------------------------------------------------
  # generate a scatterplot
  plot(df$str, df$testscr, col = "blue", pch =16, cex = 0.7, bty = "l",
       main = "Scatterplot of Test Score vs. Student-Teacher Ratio",
       xlab = "Student-teacher ratio", ylab = "Test scores")

## ------------------------------------------------------------------------
  # calculate correlation coefficient
  cor(df$str, df$testscr)

## ------------------------------------------------------------------------
  mod1 <- lm(testscr ~ str, data = df)
  summary(mod1)

## ---- fig.align='center'-------------------------------------------------
    plot(df$str, df$testscr, 
         col = "blue", pch =16, cex = 0.7, bty = "l",
         xlab = "Student-teacher ratio", ylab = "Test scores")
    # add a straight line with an intercept a and slop b
    abline(coef(mod1)[1], coef(mod1)[2], col="red")
    # add a text on the plot
    text(23, 660, "TestScore = 698.9 - 2.28 STR", 
         cex.lab = 0.9, font.lab = 3)

