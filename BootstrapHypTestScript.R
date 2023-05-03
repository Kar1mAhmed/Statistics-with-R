### BOOTSTRAP HYPOTHESIS TEST ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="/Users/OldMarin/Desktop/VIDEOS/R Videos Project/Bootstrap Hyp Tests/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
levels(d$feed)
# how many observations in each diet?
table(d$feed)

# let's look at a boxplot of weight gain by those 2 diets
boxplot(d$weight~d$feed, las=1, ylab="weight (g)", 
        xlab="feed",main="Weight by Feed")

# calculate the difference in sample means
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, mean))
# lets calculate the absolute diff in means
test.stat1 <- abs(mean(d$weight[d$feed=="casein"]) - mean(d$weight[d$feed=="meatmeal"]))  #diff in means
test.stat1
# and, a fanceir way to do that...
abs( diff( with(d, tapply(weight, feed, mean)) ) )

# and, the same for the medians
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, median))
# lets calculate the absolute diff in medians
test.stat2 <- abs(median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
test.stat2
# and, a fanceir way to do that...
abs( diff( with(d, tapply(weight, feed, median)) ) )

## let's take a look at the 3 "Classic" hyp tests we could 
# consider (each of which comes with their own limitations...)

# let's look at the Independent 2-sample t-test
t.test(d$weight~d$feed, paired=F, var.eq=F)  # tests Ho: means are equal
# let's look at the Wilcoxon aka Mann-Whitney U 
wilcox.test(d$weight~d$feed, paired=F)  # tests Ho: medians are equal
# let's look at the Kolmogorov-Smirnov 2-sample test
ks.test(d$weight[d$feed=="casein"], d$weight[d$feed=="meatmeal"], paired=F)     # tests Ho: distributions are same

########################
##  BOOTSTRAPPING... ###
########################

# let's bootstrap...
set.seed(112358)   # for reproducibility
n <- length(d$feed)  # the number of observations to sample
n
B <- 10000  # the number of bootstrap samples
variable <- d$weight  # the variable we will resample from

# now, get those bootstrap samples (without loops!)
BootstrapSamples <- matrix( sample(variable, size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)
# let's take a moment to discuss what that code is doing...
dim(BootstrapSamples)

# now, calculate the means (Yc and Ym) for each of the bootstrap samples
#  (the inefficeint, but transparent way...best to start simple, and once
#   working well, then make code more efficent)
# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)
# run through a loop, each time calculating the bootstrap test.stat
#  NOTE: could make this faster by writing a "function" and then
#        using "apply" to apply it to columns of the "BootSamples"
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat1[i] <- abs( mean(BootstrapSamples[1:12,i]) - 
                               mean(BootstrapSamples[13:23,i]) )
  # calculate the boot-test-stat2 and save it
  Boot.test.stat2[i] <- abs( median(BootstrapSamples[1:12,i]) - 
                               median(BootstrapSamples[13:23,i])  )
}

# before going too far, let's remind ourselves of the OBSERVED TEST STATS
test.stat1; test.stat2
# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
round(Boot.test.stat1[1:20], 1)
round(Boot.test.stat2[1:20], 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1 >= test.stat1)[1:20]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean( Boot.test.stat1 >= test.stat1)

# let's calculate the p-value for test statistic 2 (abs diff in medians)
mean( Boot.test.stat2 >= test.stat2)

# now, recall the difference between "statistical significance" and 
# "scientific significance"
### in a "real-world" what would you want to conclude here
table(d$feed)

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
plot(density(Boot.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Bootstrap Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

###########################
### Code to run the analysis, using a test stat of diff in 90th percentiles
###########################

# lets calculate the absolute diff in 90th percentiles
test.stat3 <- abs(quantile(d$weight[d$feed=="casein"], prob=0.9) - quantile(d$weight[d$feed=="meatmeal"], prob=0.9))  #diff in medians
test.stat3

# initialize a vector to save the bootstrap test stats in
Boot.test.stat3 <- rep(0,B)

# run thru a loop calculating the bootstrap test statistics
for (i in 1:B){
# calculate the boot-test-stat3 and save it
Boot.test.stat3[i] <- abs( quantile(BootstrapSamples[1:12,i], prob=0.9) - 
                             quantile(BootstrapSamples[13:23,i], prob=0.9) )
}

# and, calculate the p-value
mean( Boot.test.stat3 >= test.stat3)


