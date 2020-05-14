##Plot survival_to_ir function
#This function plots the survival_to_ir function 
#minY = the minimal survival
##maxY = the maximum survival
##divisions = the resolution to which the graph can plot


library(ggplot2)
library(dplyr)


plot_bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion,
                                                michaelis.menton.slope, 
                                                half.population.bioassay.survival.resistance, 
                                                estimate.precision, 
                                                sd.population.resistance, 
                                                nsim, 
                                                minimum.resistance.value, 
                                                maximum.resistance.value,
                                                minimum.bioassay.survival, 
                                                maximum.bioassay.survival, 
                                                divisions){
  
  df=data.frame(bioassay.survival.values=seq(minimum.bioassay.survival, maximum.bioassay.survival,
                                             by = divisions))%>%
    rowwise%>%
    mutate(resistance.values = bioassay_survival_to_resistance(
      maximum.bioassay.survival.proportion=maximum.bioassay.survival.proportion,
      michaelis.menton.slope=michaelis.menton.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      bioassay.survival=bioassay.survival.values, 
      estimate.precision=estimate.precision, 
      sd.population.resistance=sd.population.resistance,
      nsim=nsim,
      minimum.resistance.value=minimum.resistance.value, 
      maximum.resistance.value=maximum.resistance.value)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
  ggplot(df, aes(x=resistance.values, y = bioassay.survival.values)) +
    geom_point(colour = "red") +
    xlab("Insecticide Resistance Intensity") +
    ylab("Bioassay Survival Proportion") +
    theme_classic()
}


plot_bioassay_survival_to_resistance(
                    maximum.bioassay.survival.proportion = 1,
                    michaelis.menton.slope=1,
                    half.population.bioassay.survival.resistance = 900, 
                    estimate.precision = 0.01, 
                    sd.population.resistance = 25,
                    nsim = 1000,
                    minimum.resistance.value = 0,
                    maximum.resistance.value = 25000,
                    minimum.bioassay.survival = 0, 
                    maximum.bioassay.survival = 1, 
                    divisions = 0.01)



