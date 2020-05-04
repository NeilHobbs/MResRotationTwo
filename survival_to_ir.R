
survival_to_ir = function(Kmax, n, z_50, req_surv, precision, sigma, nsim, min_value, max_value){
  while((test_value = ((min_value + max_value)/2))){
    if((max_value - min_value) < precision)
      {return(test_value)} #When precision level reached return test_value(IR (z))
    else(
      if(ir_to_survival(Kmax = Kmax, z=test_value, n=n, z_50=z_50, sigma = sigma, nsim = nsim) < req_surv)
      {
        min_value = test_value} 
      else(max_value = test_value))
   }
}



#Run inverse function to confirm; using z value from survival_to_ir.
#If ir_to_survival is ~0.10 then correct
ir_to_survival(Kmax=1, z=Test1, n=1, z_50=100, sigma=0, nsim=1) 

##Run survival_to_ir with default settings
survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = 0.1, precision = 0.01, sigma = 0.1, nsim = 1000,
               min_value = 0, max_value = 5000)

#Create vectors of SD and Survivals values
sd.values = c(0.1, 0.5, 1, 5, 20, 25)
surv.values = c(0.05, 0.1, 0.2, 0.5, 0.8)

#with original default parameters
for(i in 1:length(surv.values)){
  for(j in 1:length(sd.values)){
     print(survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = surv.values[i], precision = 0.01, sigma = sd.values[j], nsim = 1000,
                                 min_value = 0, max_value = 5000))
    }
}



#confirm z_50=900 with 10% survival gives IR z value of 100
survival_to_ir(Kmax=1, n=1, z_50 = 900, req_surv = 0.1, precision = 0.01, sigma = 5, nsim = 1000,
               min_value = 0, max_value = 5000)

##With z_50 at 900 so survival is 10% at z=100
get_z_from_sd_surv = function(survival, stan.dev){
  
  
}

#Next task make a graph of ir_to_survival outputs
