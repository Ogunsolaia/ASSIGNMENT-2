source("./R/Assignment2.R")

#Estimating the sample size under the null hypothesis theta = 0.5
#lamda = 0.7, gamma = 0.5, n1 = 50, n2 = 100


BOP2_design(10^4, 0.7, 0.5, 50, 100, 0.5)

#Estimating the sample size under the alternative hypothesis theta = 0.7
#lamda = 0.7, gamma = 0.5, n1 = 50, n2 = 100

BOP2_design(10^4, 0.7, 0.5, 50, 100, 0.7)


########################################################################

#To compute the type I and tyep II error under the null and alternative hypothesis respectively
#where theta1 = 0.5, theta2 = 0.7, n1=40, N=10^4, c = 25

TypeI_and_II_Error(10^4, 40, 0.5, 0.7, 25)



