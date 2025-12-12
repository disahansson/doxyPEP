
library(openxlsx)
library(dplyr)
library(mc2d)
#library(rjags)
#library(prevalence)
##

data.path 		    <- "./data" 
script.path 	    <- "./code"



Survey_res = read.xlsx(paste(data.path,"/Res_survey_both_Q.xlsx",sep="")) 

expert_data <- data.frame(
  expert_id = 1:nrow(Survey_res),  # 
  lower = Survey_res$Q1_Lower_bound/100,   # 2.5% CI (minimum expected uptake)
  most_likely = Survey_res$Q1_Best_estimate/100,  # Most likely estimate
  upper =Survey_res$Q1_Upper_bound/100   # 97.5% CI (maximum expected uptake)
)




# hist(Survey_res$Q1_Best_estimate,breaks=10, xlab = "%", main="Percentage PrEP-users to take doxyPEP", ylim = c(0,20))
summary(Survey_res$Q1_Best_estimate)
summary(Survey_res$Q1_Lower_bound)
summary(Survey_res$Q1_Upper_bound)

summary(Survey_res$Q2_Mode)
summary(Survey_res$Q2_Upper_bound )
# hist(Survey_res$Q2_Mode,breaks=15,xlab = "Doses /6 months", main="DoxyPEP usage", ylim = c(0,20))


# plot(Survey_res$Q1_Best_estimate,Survey_res$Q2_Mode )

#############################################
# 
#               Question 1
#
#############################################




mu = median(Survey_res$Q1_Best_estimate/100)
ci = c(median(Survey_res$Q1_Lower_bound/100), median(Survey_res$Q1_Upper_bound/100))

get.beta <- function(alpha) {
  alpha * (1-mu)/mu
}

quantiles <- c(0.25, 0.75)

cost <- function(alpha) {
  qs <- qbeta(quantiles, alpha, get.beta(alpha))
  sum(abs(qs-ci))
}

opt <- optim(1, cost, method="BFGS")
estimated.alpha <- opt$par
estimated.beta  <- get.beta(estimated.alpha)
estimated.ci    <- qbeta(quantiles, estimated.alpha, estimated.beta)

# or 

# UPPER=1000000
# opt <- optimize(cost,lower=0,upper=UPPER )
# estimated.alpha <- opt$minimum
# estimated.beta  <- get.beta(estimated.alpha)

# hist(Survey_res$Q1_Best_estimate/100, freq = FALSE, xlab="Percentage PrEP-users to take doxyPEP", 
#      main = "observed best guess vs estimated distributions",
#      breaks = 10,ylim = c(0,6), xlim = c(0, quantile(Survey_res$Q1_Best_estimate/100, 0.99)))
# curve(dbeta(x, shape1 = estimated.alpha, shape2 = estimated.beta), from = 0, col = "red", add = TRUE, lwd = 2)
# curve(dunif(x, min=0,max=1), from = 0, col = "blue", add = TRUE, lwd = 2)
# 
# curve(dpert(x, min = 0, mode = mu, max = 1, shape = 4, log = FALSE), from = 0, col = "purple", add = TRUE, lwd = 2)
# legend("topright", c("Uniform","Beta","BetaPert"), lty=1,col=c("blue","red","purple"),lwd=3)



uptake_fraction_beta_parms <- c(alpha=estimated.alpha,beta=estimated.beta)



#############################################
# 
#               Question 2
#
#############################################



expert_data_Q2 = Survey_res %>% select(Q2_Mode,Q2_Upper_bound)
expert_data_Q2_m = t(as.matrix(expert_data_Q2))
v2 <- expert_data_Q2_m#matrix(c(20,30,50,55,60,70),3,2)
p2 <- c(0.5,0.95)

mu2 = median(2*expert_data_Q2$Q2_Mode  )
upper2 = median(2*expert_data_Q2$Q2_Upper_bound  )




# mean=shape/rate <=> rate=shape/mean
# mode = (shape-1)/rate  <=>   rate = (shape-1)/mode
# variance shape/scale^2
get.rate <- function(shape) {
  return((shape-1)/mu2)
 # return((shape)/mu2)
}

quantiles <- c(0.95)

cost <- function(shape) {
  qs <- qgamma(quantiles, shape, rate=get.rate(shape))
  sum(abs(qs-upper2))
}

set.seed(235911)
guess = runif(1,1,10)
guess
opt <- optimize(cost, c(0,1000)) #optim(guess, cost, method="BFGS")

estimated.shape <- opt$minimum #opt$par
estimated.rate  <- get.rate(estimated.shape)
estimated.ci    <- qgamma(quantiles, shape=estimated.shape, rate=estimated.rate)


# hist(2*expert_data_Q2$Q2_Mode, freq = FALSE, xlab="Doses per year", 
#      main = "observed best guess vs estimated distributions",
#      breaks = 20, ylim = c(0,0.05))
# curve(dpert(x, min = 1, mode = mu2, max = upper2, shape = 4, log = FALSE), from = 0, col = "purple", add = TRUE, lwd = 2)
# 
# curve(dgamma(x, shape= estimated.shape, rate=estimated.rate,log = FALSE), from = 0, col = "red", add = TRUE, lwd = 2)
# legend("topright", c("Gamma","BetaPert"), lty=1,col=c("red","purple"),lwd=3)
# 

