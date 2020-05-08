##Create a function that inputs a z value and a specific survival value to give a z_50 value to
#make creating z scales reproducible. 
#Uses similar coding architecture as the survival_to_ir function

#Where:
#z.value = is the specific z value of interest
#desired.survival = the corresponding survival that the z.value should have
#Example WHO cut off is 10% survival; and we want a scale such that z=100 gives desired.survival = 0.1,
#we can from there calculate the z_50 on this scale for use in all other functions. 



find_z50_value = function(z.value, desired.survival, Kmax, n, sigma, nsim, min_value, max_value, precision){
  
    while((find_z50 = ((min_value + max_value)/2))){
    if((max_value - min_value) < precision)
    {return(find_z50)} #When precision level reached return the corresponding z_50 score
    else(
      if(ir_to_survival(Kmax = Kmax, z=z.value, n=n, z_50=find_z50, sigma = sigma, nsim = nsim) > desired.survival)
      {
        min_value = find_z50} 
      else(max_value = find_z50))
  }
}


##Testing1: z.value =900, desired.survival=0.5; should return z_50 at 900
find_z50_value(z.value = 900, desired.survival = 0.5, Kmax=1, n=1, sigma=0, nsim=1000, min_value = 0, max_value = 5000,
               precision=0.001)
#Which it does.

#Test2: z.value = 100, desired.survival=0.1, should return z_50 at 900
find_z50_value(z.value = 100, desired.survival = 0.1, Kmax=1, n=1, sigma=0, nsim=1000, min_value = 0, max_value = 5000,
               precision=0.001)
#Which it does