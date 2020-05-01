#calculate z values for a range of SD values and survival values

#Current Method is nested for loop:
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

##but this method only prints the z values. Need to be able to save so as to make into a table.


##consider starting with a matrix?