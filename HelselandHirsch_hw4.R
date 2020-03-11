#Question 3.1.

x <- c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0, 5.0, 0.6, 1.2, 0.3, 0.2, 0.5, 0.5, 10, 0.2, 0.2, 1.7, 3.0 )
y <- x[order(x)]
plot(y)

#Finding the non-parametric intervals

#According to the table on page 367 of the book "Performance evaluation of computer and communication systems" (https://perfeval.epfl.ch/), the intervals for a dataset of 18 samples, the lower and upper bounds are the 5th and 14th numbers in the distribution (in ascending order). That gives us the Rl (Rl =  x' + 1) and Ru (Ru =  n ??? x' = x).

lowerg <- y[5]
upperg <- y[14]
print(paste(y[5], "to", y[14], "mg/L"))

#Maybe create a generic function to calculate all this no matter what data set we are using, notice the input of the function
# is the data set we are studying and the alpha.

#Try:

Symmetric_meadian <- function(x,alpha){
  
  n <-  length(x)
  tval <- qt(1-alpha/2,n-1) 
  ln_x <- log(x)
  var_x <- var(ln_x)
  mean <- mean(ln_x)
  lower <- exp(xmean-tval*sqrt(var_x/n))
  upper <- exp(xmean+tval*sqrt(var_x/n))
  interval <- c(lower,upper)
  print(paste(lower, "to", upper, "mg/L"))
  return(interval)
}



#In the cae of your code 
# #Finding the parametric intervals
# n = 18
# alpha = 0.05
# t.value <- qt(1-alpha/2, n-1)
# t.value2 <- qt(1-alpha/2, n-1)
# ln.grano <- log(x)
# var.grano <- var(ln.grano)
# mean.ln.grano <- mean(ln.grano)
# 
# lower <- exp(mean.ln.grano-t.value*sqrt(var.grano/n))
# lower
# 
# upper <- exp(mean.ln.grano+t.value2*sqrt(var.grano/n))
# upper
# 
# print(paste(lower, "to", upper, "mg/L"))


#The non-parametric 95% interval estimates for the median is from 0.4 to 3 mg/L, while the parametric ranges from 0.5 to 1.8 mg/L. As the dataset is small and non-normal it is better to use non-parametric, because the non-parametric provides a wider range for the confidence interval than the parametric.


#Question 3.4.


library(tidyverse)

Con_river <- read_csv("C:/Users/gusta/Desktop/PhD/Classes/ES207/Conecuh_River_apxc2.csv", col_names =TRUE, cols(
  
  Year = col_double(),
  `Flow (cfs)` = col_double()))

#As it is a small dataset (20 samples), the most appropriate choice will be a non-parametric 95% interval estimate.

#Intervals for the median

Flows <- Con_river$`Flow (cfs)`
Flows.x <- Flows[order(Flows)]

Flows

plot(Flows.x, xlab = "Samples", ylab = "River flow (cfs)", main = "River flow")

#According to the table on page 367 of the book "Performance evaluation of computer and communication systems" (https://perfeval.epfl.ch/), the intervals for a dataset of 20 samples, the lower and upper bounds are the 6th and 15th numbers in the distribution (in ascending order). That gives us the Rl (Rl =  x' + 1) and Ru (Ru =  n ??? x' = x).

Flows.x[6]
Flows.x[15]
print(paste(Flows.x[6], "to", Flows.x[15], "cfs"))

#Intervals for the mean (although the sample is small it is a binomial distribution)

#The same in this case you can have this as a function: 
#This function calculates the symmetric interval estimate for the mean, so you have a generic function that you can use again.

Symmetric_mean<- function(x,alpha){
  
  n <-  length(x)
  tval <- qt(1-alpha/2,n-1) 
  xvar <- var(x)
  xmean <- mean(x)
  lower<- xmean-tval*sqrt(xvar/n)
  upper <-xmean+tval*sqrt(xvar/n)
  interval <- c(lower,upper)
  return(interval)
}


# Symmetric_mean_int(Conecuh$`Flow (cfs)`,0.05)
# 
# n =20
# alpha = 0.05
# t.value <- qt(1-alpha/2, n-1)
# varian <- var(Con_river$`Flow (cfs)`, na.rm = TRUE)
# lowflow <- mean(Con_river$`Flow (cfs)`, na.rm = TRUE) - t.value*(sqrt(varian/20))
# upperflow <- mean(Con_river$`Flow (cfs)`, na.rm = TRUE) + t.value*(sqrt(varian/20))
# print(paste(lowflow,"to", upperflow, "cfs"))


#The lower and upper bounds for the non-parametric 95% interval estimates for the median are from 524 to 894 cfs. The lower and upper bounds for the 95% interval estimates for the mean are from 557 to 809 cfs. 

