#calculate z values for a range of SD values and survival values

#Current Method is nested for loop:
#Create vectors of SD and Survivals values

library(dplyr)
library(tidyr)


set_sd_values = function(){} ##function to allow the input of own required sd range
set_surv_values = function(){} ##function to allow the input of own required survival range


table_z_values = function(){} #inputs: z_50; surv_vals;sd_vals, potentially max_vale

sd.values = rep(c(0.1, 0.5, 1, 5, 20, 25), times = 5)
surv.values = c(rep(0.05, 6), rep(0.1, 6), rep(0.2, 6), rep(0.5, 6), rep(0.8, 6))

##Need to turn into a function, whereby unique 

df = data.frame(surv.values, sd.values)%>%
  rowwise()%>%
  mutate(z.values = survival_to_ir(Kmax=1, n=1, z_50 = 900, req_surv = surv.values, precision = 0.01, sigma = sd.values, nsim = 1000,
                                   min_value = 0, max_value = 5000))%>%
    spread(key = surv.values, value = z.values)

##Would be useful to return a tidy dataframe before the spread is done.





library(curl)

install.packages("kableExtra", dependencies = TRUE)

library(knitr)


#Orignal method: using nested loop, with original default parameters
for(i in 1:length(surv.values)){
  for(j in 1:length(sd.values)){
    print(survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = surv.values[i], precision = 0.01, sigma = sd.values[j], nsim = 1000,
                         min_value = 0, max_value = 5000))
  }
}

##but this method only prints the z values. Need to be able to save so as to make into a table.


##consider starting with a matrix?

apply(survival_to_ir(Kmax=1, n=1, z_50 = 100, req_surv = surv.values, precision = 0.01, sigma = sd.values, nsim = 1000,
                             min_value = 0, max_value = 5000))


