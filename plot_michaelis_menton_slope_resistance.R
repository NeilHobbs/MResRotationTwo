##This function demonstrates the impact of what happens when you change n in the survival_to_ir function,
## and demonstrates how the function only appears to perform correctly when n = 1. As when z_50=900 and
# req_surv = 0.5 (50%), this is the only time the corresponding z value matches the z_50 value. 
library(dplyr)
library(ggplot2)

michaelis.menton.slope.values = rep(seq(0, 2, by = 0.1), 6)
sd.population.resistance.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))


plot_michaelis_menton_slope_resistance = function(maximum.bioassay.survival.proportion,
                                                  michaelis.menton.slope.values,
                                                  half.population.bioassay.survival.resistance,
                                                  bioassay.survival, 
                                                  estimate.precision, 
                                                  sd.population.resistance.values, 
                                                  nsim, 
                                                  minimum.resistance.value,
                                                  maximum.resistance.value){
  
  df = data.frame(michaelis.menton.slope.values, sd.population.resistance.values)%>%
    rowwise()%>%
    mutate(resistance.values = bioassay_survival_to_resistance(
              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion, 
              michaelis.menton.slope = michaelis.menton.slope.values, 
              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
              bioassay.survival = bioassay.survival,
              estimate.precision = estimate.precision,
              sd.population.resistance = sd.population.resistance.values, 
              nsim = nsim, 
              minimum.resistance.value = minimum.resistance.value, 
              maximum.resistance.value = maximum.resistance.value))%>%
    mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
  
  ggplot(df, aes(x=michaelis.menton.slope.values, y=resistance.values)) +
    geom_point(aes(color = sd.population.resistance.values)) +
    theme_classic()+
    ylab("Insecticide Resistance Intensity") +
    xlab("Michaelis Menton Equation Slope") +
    ggtitle(paste("Bioassay Survival=",bioassay.survival,
    "& Resistance for 50% Survival=",half.population.bioassay.survival.resistance))
}

plot_michaelis_menton_slope_resistance(maximum.bioassay.survival.proportion = 1,
                                       michaelis.menton.slope.values = michaelis.menton.slope.values,
                                       half.population.bioassay.survival.resistance = 900,
                                       bioassay.survival = 0.5, 
                                       estimate.precision = 0.01, 
                                       sd.population.resistance.values = sd.population.resistance.values, 
                                       nsim = 1000, 
                                       minimum.resistance.value = 0,
                                       maximum.resistance.value = 5000)


