##varying n in survival equations
#This function demonstrates the impact of changing n in the ir_to_survival function, and demonstrates that 
# the function only performs correctly when n = 1. As this is the only time that when z_50=900, and n=1, that 
# survival is 50%.

library(dplyr)
library(tidyr)
library(knitr)

michaelis.menton.slope.values = rep(seq(0, 2, by = 0.1), 6)
sd.population.resistance.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))

table_michaelis_menton_slope_survival = function(michaelis.menton.slope.values, 
                               sd.population.resistance.values,
                               maximum.bioassay.survival.proportion,
                               half.population.bioassay.survival.resistance, 
                               mean.population.resistance,
                               nsim){
  
    df = data.frame(michaelis.menton.slope.values, sd.population.resistance.values)%>%
    rowwise()%>%
    mutate(Y.values = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                     mean.population.resistance = mean.population.resistance, 
                                     michaelis.menton.slope = michaelis.menton.slope.values, 
                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
                                     sd.population.resistance = sd.population.resistance.values, 
                                     nsim = nsim))%>%
    mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
    
    return(kable(df))
}


##run with z=900 and z_50=900
table_michaelis_menton_slope_survival(michaelis.menton.slope.values = michaelis.menton.slope.values, 
                                      sd.population.resistance.values=sd.population.resistance.values, 
                                      maximum.bioassay.survival.proportion = 1, 
                                      half.population.bioassay.survival.resistance = 900, 
                                      mean.population.resistance = 900,
                                      nsim = 100)
##To me easier to see what is going on with the graph



