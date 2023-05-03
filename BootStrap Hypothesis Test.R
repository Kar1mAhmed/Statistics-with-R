##### Main sample #####
d <- read.csv(file="E:\\Coding\\R code\\ChickData.csv") 
View(d)

#getting the difference between the casein mean and meat_meal mean from the orginal sample
org_mean_diff <- abs( mean(d$weight[d$feed == "casein"]) - mean(d$weight[d$feed == "meatmeal"]) )
org_median_diff <- abs( median(d$weight[d$feed == "casein"]) - median(d$weight[d$feed == "meatmeal"]) )

####BOOTSTRAP#####

set.seed(1020)
sample_size <- length(d$feed)
weights <- d$weight
B_size <- 10000

B <- matrix(0 , nrow = sample_size , ncol = B_size)

# forming 10000 bootstrap samples from or original sample

for(i in 1:B_size){
  B[,i] = sample(weights , size = sample_size , replace = TRUE) # if replace = false it's permutation test
}

Boot_means <- Boot_medians <- rep(0 , B_size)

#### getting the mean difference for all BootStrap samples 
for(i in 1:B_size){
  Boot_means[i] = abs( mean(B[1:12,i]) - mean(B[13:23,i]) )
  Boot_medians[i] = abs( median(B[1:12,i]) - median(B[13:23,i]) )
}

#test how many permutation sample have mean bigger or equal to the original sample mean
# H0: casein and meat_meal have no difference    //if more than alpha% the per mean are smaller than org_mean H0 is true
# HA: casein and meat_meal have difference  //if less than alpha% of the per_means are greater than org_mean HA is true

mean(Boot_means >= org_mean_diff) # 9.2% are more than or equal the original mean
mean(Boot_medians >= org_median_diff) # 6.3 % are more than or equal the original median

#  H0 will be accepted if alpha = 5%  
#  and it will be rejected if alpha = 10%
