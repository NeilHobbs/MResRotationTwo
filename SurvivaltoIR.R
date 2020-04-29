
survival_to_ir = function(Kmax, n, z_50, req_surv, precision){
  
  min_value = 0 
  max_value = 500 #side note: a max value of 500 can only allow for a max survival of 0.8333. Will be needed to increased 
  
  while((test_value = ((min_value + max_value)/2))){
    if((max_value - min_value) < precision)
      {return(test_value)} #When precision level reached return test_value(IR (z))
    else(
      if(ir_to_survival(Kmax = Kmax, z=test_value, n=n, z_50=z_50, sigma = 0, nsim = 1) < req_surv)
      {#can put sigma and nsim in function parameters?
      #but better to put sigma = 0, nsim=1 to prevent any  issues  in the function.Not liking NA or being ignored!
        min_value = test_value} 
      else(max_value = test_value))
   }
}

##Run survival_to_ir with default settings
Test1 = survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = 1, precision = 0.01)

#Run inverse function to confirm; using z value from survival_to_ir.
#If ir_tp=o_survival is ~0.10 then correct
ir_to_survival(Kmax=1, z=Test1, n=1, z_50=100, sigma=0, nsim=1) 

