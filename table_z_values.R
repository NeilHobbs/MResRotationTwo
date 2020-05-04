#calculate z values for a range of SD values and survival values

#Current Method is nested for loop:
#Create vectors of SD and Survivals values

##Packages required
library(dplyr)
library(tidyr)
library(knitr)

#create intial vectors of SD(sigma) and survival; this method looks clunky and easy to make mistakes
sd.values = rep(c(0.1, 0.5, 1, 5, 20, 25), times = 5)
surv.values = c(rep(0.05, 6), rep(0.1, 6), rep(0.2, 6), rep(0.5, 6), rep(0.8, 6))

##Need to turn into a function, whereby unique sd and survival values can be input
table_z_values = function(z_50, Kmax, n, surv.values, sd.values, precision, nsim, min_value, max_value){
df = data.frame(surv.values, sd.values)%>%
  rowwise()%>%
  mutate(z.values = survival_to_ir(Kmax=Kmax, n=n, z_50 = z_50, req_surv = surv.values, precision = precision, sigma = sd.values, nsim = nsim,
                                   min_value = min_value, max_value = max_value))%>%
    spread(key = surv.values, value = z.values)
return(kable(df))
}


table_z_values(z_50=900, Kmax=1, n=1, surv.values=surv.values, sd.values = sd.values, 
                  precision = 0.01, nsim=1000, min_value = 0, max_value = 5000)


