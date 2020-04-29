
library(ggplot2)
#Function to translate IR levels into survival 

ir_to_survival = function(Kmax, z, n, z_50, sigma, nsim){ 
  
  f_z = rnorm(nsim, mean = z, sd = sigma) #Generate random Normal distribution around mean IR level, of nsim replicates. 
  f_z = ifelse(f_z < 0, 0, f_z) #Prevent Insecticide Resistance being less than zero
  
  Y = (Kmax * (f_z^n)) / (z_50 + (f_z ^ n))  #Calculate Survival (Equation 6)
  Y = ifelse(Y < 0, 0, Y) #Prevent survival being less than zero.
  return(mean(Y))
  }


##Checking code
ir_to_survival(Kmax = 1, z = 500, n=1, z_50 = 100, sigma = 5, nsim = 1000)  #Currently only giving ~0.5 when n = 1

