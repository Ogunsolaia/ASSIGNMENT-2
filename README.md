
ASSIGNMENT

set.seed(201478668) # setting seed for replicability and debugging

#(1) ASSIGNMENT 1 :Expected sample size estimation for BOP2 design with decision rules parameters (lamda and gamma) and samples sizes (n1, n2) under the null and alternative hypothesis

    
    BOP2_design <- function(lambda, gamma, n1, n2, theta) {
    M <- 1000 # Number of samples to be simulated
    Simulated_Samples <- rep(NA, M) # creating and empty vector to store simulated samples

    for (i in 1:M) {
     # Simulate theta from its prior, and then the stage 1 data conditional
     # on this theta.

     y1 <- rbinom(1, n1, theta) # stage 1 data conditioned on the null hypothesis


     # The posterior distribution parameters
     a1 <- 0.5 + y1
     b1 <- 0.5 + n1 - y1

     # Computing the probability of futility

     prob_futility <- pbeta(0.5, a1, b1)

     # Threshold to determine whether to proceed or not based on the decision rule parameters

     c1 <- 1 - lambda * (n1 / n2)^gamma

     # The final total sample size is stored in Simulated_Samples

     if (prob_futility < c1) {
       Simulated_Samples[i] <- n1
     } else {
       Simulated_Samples[i] <- n2
     }
   }

    return(mean(Simulated_Samples)) # return the expected sample size
 }

#Estimating the sample size under the null hypothesis theta = 0.5
#lamda = 0.7, gamma = 0.5, n1 = 50, n2 = 100

    BOP2_design(0.7, 0.5, 50, 100, 0.5)


#Estimating the sample size under the alternative hypothesis theta = 0.7
#lamda = 0.7, gamma = 0.5, n1 = 50, n2 = 100

    BOP2_design(0.7, 0.5, 50, 100, 0.7)


########################################################################

# (2) ASSIGNMENT 2: Type I and Type II Error calculation
# (i) under the null 

    TypeI_Error <- function(N, n1, theta, c) {

    # N is the number of samples to be simulated
    # n1 is the sample size at the first stage
    # theta is the parameter value under the null
    # c is the is critical value

     y <- rbinom(n = N, size = n1, prob = theta) # To generate N binomial random numbers

    type1_error <- mean(y >= c) # The type 1 eRror is stored in type1_error

    return(type1_error)
  }

#To compute the type I error under the null hypothesis 
#where theta = 0.5, n1=40, N=10^4, c = 25)
  
    TypeI_Error(10^4, 40, 0.5, 25)  

#(ii) Under the alternate hypothesis

    TypeII_Error<-function(N, n1, theta, c) {
  
    #N is the number of samples to be simulated
    #n1 is the sample size at the first stage
    #theta is the parameter value under the alternative hypothesis 
    #c is the is critical value
  
    y<-rbinom(n=N, size=n1, prob=theta)    #To generate N binomial random numbers 
  
    Power<- mean(y>=c) #to compute the power of the test
  
    return (1-Power)
 }

#To compute the type I error under the alternative hypothesis 
#where theta = 0.7, n1=40, N=10^4, c = 25)

    TypeII_Error(10^4, 40, 0.7, 25) 
