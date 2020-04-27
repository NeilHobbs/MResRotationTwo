#Function to translate IR levels into survival
IR_Survival_Func = function(Kmax, z, n, z_50, sigma,nsim){ 
  
  f_z = rnorm(nsim, mean = z, sd = sigma) #Generate random distribution around mean IR level, of nsim replicates. 
  #Should sigma be added for the standard deviation?
  
  for(i in 1:nsim){ ##for loop can be added if situation arises where z values are less than z
    if(f_z[i] < 0)
    {f_z[i] == 0}
  }
  
  Y = (Kmax * (f_z^n)) / (z_50 + (f_z ^ n))  #Calculate Survival (Equation 6)
  for(i in 1:nsim){ ##for loop can be added if situation arises where surival values are less than 0
    if(Y[i] < 0)
    {Y[i] == 0}
  }
  return(mean(Y))
  #return(f_z)
}

IR_Survival_Func(1, 100, 1, 100, 5, 1000)  #Currently only giving ~0.5 when n = 1

