
survival_to_ir = function(Kmax, n, z_50, req_surv, precision){
  
  min_value = 0
  max_value = 500
  
  while((test_value = ((min_value + max_value)/2))){
    if((max_value - min_value) < precision){return(test_value)} else(
    if( ir_to_survival(Kmax = Kmax, z=test_value, n=n, z_50=z_50, sigma = 0, nsim = 1) < req_surv){
      
        min_value = test_value} else(max_value = test_value))
      }
  return(test_value)
}


survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = 0.1, precision = 0.1)


