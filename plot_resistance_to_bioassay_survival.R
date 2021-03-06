##Graph of Insecticide Resistance and Survival (ir_to_survival)
##This function uses the ir_to_survival function to plot the log of insecticide resistance (x axis) against
##survival in a bioassay (y axis)
##warning_surv = the survival at which it is recommended to look for resistance mechanisms (currently 2%, WHO)
##cutoff_surv = the survival at which switching insecticide is recommended (currently 10%, WHO)

#required packages by function
library(ggplot2)
library(dplyr)

plot_resistance_to_bioassay_survival = function(maximum.bioassay.survival.proportion, 
                                                michaelis.menton.slope, 
                                                half.population.bioassay.survival.resistance, 
                                                nsim, 
                                                minimum.resistance, 
                                                maximum.resistance, 
                                                sd.population.resistance){
  
  df=data.frame(resistance.values=seq(minimum.resistance, maximum.resistance, by = 1))%>%
    rowwise%>%
    mutate(bioassay.survival.proportion = resistance_to_bioassay_survival(
      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion, 
      mean.population.resistance = resistance.values, 
      michaelis.menton.slope=michaelis.menton.slope, 
      half.population.bioassay.survival.resistance=half.population.bioassay.survival.resistance, 
      sd.population.resistance = sd.population.resistance,
      nsim=nsim)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
  ggplot(df, aes(x=resistance.values, y = bioassay.survival.proportion)) + ##logging to increase ease of readibility
    geom_point(colour = "blue") +
    xlab("Insecticide Resistance") + ##applying log(z) makes it more readable at the lower z levels.
    ylab("Bioassay Survival Proportion")+ #note: this label may need to be changed depending on what we calibrate against
    theme_classic()
}


#sd.values = c(rep(0.1, 10001), rep(0.5, 10001), rep(1, 10001), rep(5, 10001), rep(20, 10001), rep(25, 10001))

ir.values = seq(0, 5000, by = 1)   
##I was finding that including sigma ranges (0.1 to 25) were not having any influence on the final plot (overlapping)
plot_resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1, 
                    michaelis.menton.slope = 1, 
                    half.population.bioassay.survival.resistance = 900, 
                    nsim = 1000, 
                    minimum.resistance = 0, 
                    maximum.resistance = 5000, 
                    sd.population.resistance = 1)

