library(ggplot2)
library(plyr)

gss.data <- read.table(file="./sub-data.txt", header = TRUE, sep = ",")
gss.data <- gss.data[,c("AGE","SEX","DEGREE","CONINC","HRS2","AGEWED")]

# trim the data and/or factorize
gss.data$SEX <- factor(gss.data$SEX, labels=c("M","F"))

gss.data$DEGREE <- factor(gss.data$DEGREE, levels=c(1:4), labels=c("HIGH SCHOOL","JUNIOR COLLEGE", "BACHELOR", "GRADUATE"))
gss.data <- gss.data[complete.cases(gss.data$DEGREE),]

gss.data <- gss.data[gss.data$CONINC != 0 ,]
gss.data <- gss.data[gss.data$CONINC != 999999 ,]
gss.data <- gss.data[gss.data$CONINC != 999998 ,]

gss.data <- gss.data[gss.data$AGE != 0 ,]
gss.data <- gss.data[gss.data$AGE != 98 ,]
gss.data <- gss.data[gss.data$AGE != 99 ,]

gss.data <- gss.data[gss.data$HRS2 != 0 ,]
gss.data <- gss.data[gss.data$HRS2 != 98 ,]
gss.data <- gss.data[gss.data$HRS2 != 99 ,]

gss.data <- gss.data[gss.data$AGEWED != 0 ,]
gss.data <- gss.data[gss.data$AGEWED != 98 ,]
gss.data <- gss.data[gss.data$AGEWED != 99 ,]



age.sex.hist.fig <- ggplot(gss.data, aes(x=AGE, fill=SEX)) + geom_histogram(binwidth=5, alpha=0.5)
ggsave("../figs/age-sex-hist.png", plot=age.sex.hist)

avg.income.by.degree <- ddply(gss.data, "DEGREE", summarise, group.mean=mean(CONINC))
avg.income.by.degree.fig <- ggplot(gss.data, aes(x=CONINC, fill=DEGREE)) + geom_histogram(binwidth=10000, alpha=0.5) +
    geom_vline(data=avg.income.by.degree, aes(xintercept=group.mean, color=DEGREE))
ggsave("../figs/avg.income.by.degree.png", plot=avg.income.by.degree.fig)

# perform pca and project onto first two pcs
gss.pca <- prcomp(gss.data[,c("AGE","CONINC","HRS2","AGEWED")], center=TRUE, scale=TRUE)
projs <- t(gss.pca$rotation[,c(1,2)])%*%t(gss.pca$x)
projs <- data.frame(pc1 = projs[1,], pc2 = projs[2,], degree=gss.data$DEGREE, sex=gss.data$SEX)

pc1.pc2.deg.fig <- ggplot(projs, aes(x=pc1, y=pc2)) +
    geom_point(aes(color=degree))
ggsave("../figs/pc1.pc2.deg.png", plot=pc1.pc2.deg.fig)


pc1.pc2.sex.fig <- ggplot(projs, aes(x=pc1, y=pc2)) +
    geom_point(aes(color=sex))
ggsave("../figs/pc1.pc2.sex.png", plot=pc1.pc2.sex.fig)


# attempt at chisq tests
## combos <- combn(ncol(gss.data),2)

## adply(combos, 2, function(x) {
##   test <- chisq.test(gss.data[, x[1]], gss.data[, x[2]])

##   out <- data.frame("Row" = colnames(gss.data)[x[1]]
##                     , "Column" = colnames(gss.data[x[2]])
##                     , "Chi.Square" = round(test$statistic,3)
##                     ,  "df"= test$parameter
##                     ,  "p.value" = round(test$p.value, 3)
##                     )
##   return(out)

## })


