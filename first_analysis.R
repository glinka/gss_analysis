gss.data <- read.table(file="sub-data-backup.txt", header = TRUE, sep = ",")

gss.data$SEX <- factor(gss.data$SEX, labels=c("M","F"))
male.ages <- gss.data$AGE[gss.data$SEX == "M"]

library(plyr)

combos <- combn(ncol(gss.data),2)

adply(combos, 2, function(x) {
  test <- chisq.test(gss.data[, x[1]], gss.data[, x[2]])

  out <- data.frame("Row" = colnames(gss.data)[x[1]]
                    , "Column" = colnames(gss.data[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
                    )
  return(out)

})


