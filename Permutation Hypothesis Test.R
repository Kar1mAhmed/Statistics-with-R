
######### MAIN DATA SET ##########
d <- read.csv(file="E:\\Coding\\R code\\ChickData.csv") 

table(d$feed) # checking the feed types

boxplot(d$weight~d$feed,las=1 , ylab = "weight" , xlab = "feed type" ,main = "weight by feed") 

#getting the mean difference between the two feed types
org_mean <- abs(mean(d$weight[d$feed == "casein"]) - mean(d$weight[d$feed == "meatmeal"])) 

#getting the median difference between the two feed types
org_medain <- abs(median(d$weight[d$feed == "casein"]) - median(d$weight[d$feed == "meatmeal"]))

##### Permutation Test #######
set.seed(1956)
P <- 10000 # number of permutation samples we want
n <- length(d$feed)
all_weights <- d$weight

per_samples <- matrix(0 , nrow = n , ncol = P)

#getting the permutation samples using loop
for(i in 1:P){
  per_samples[,i] = sample(all_weights , size = n , replace = FALSE) 
}


#getting the means and medians of all permutation samples
per_means <- per_medians <- rep(0,P)

for(i in 1:P){
  per_means[i] = abs( mean(per_samples[1:12,i]) - mean(per_samples[13:23 ,i])) 
  per_medians[i] = abs( median(per_samples[1:12,i]) - median(per_samples[13:23 ,i])) 
}

#test how many permutation sample have mean bigger or equal to the original sample mean
# H0: casein and meat_meal have no difference    //if more than alpha% the per mean are smaller than org_mean H0 is true
# HA: casein and meat_meal have difference  //if less than alpha% of the per_means are greater than org_mean HA is true

mean(per_means >= org_mean) # 9.9% are more than or equal the original mean
mean(per_medians >= org_medain) # 5.2 % are more than or equal the original median

#  H0 will be accepted if alpha = 5%  
#  and it will be rejected if alpha = 10%

