library(dplyr)
library(ggplot2)


michaelis.menton.slope.values = rep(seq(0, 2, by = 0.1), 6)
sd.population.resistance.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))

plot_michaelis_menton_slope_survival = function(michaelis.menton.slope.values, 
                              sd.population.resistance.values, 
                              maximum.bioassay.survival,
                              half.population.bioassay.survival.resistance, 
                              mean.population.resistance,
                              nsim){
  df = data.frame(michaelis.menton.slope.values, sd.population.resistance.values)%>%
    rowwise()%>%
    mutate(bioassay.survival.proportion = resistance_to_bioassay_survival(
        maximum.bioassay.survival = maximum.bioassay.survival, 
        mean.population.resistance = mean.population.resistance, 
        michaelis.menton.slope = michaelis.menton.slope.values, 
        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
        sd.population.resistance = sd.population.resistance.values,
        nsim = nsim))%>%
    mutate(sd.population.resistance.values = as.factor(sd.population.resistance.values))
  
  ggplot(df, aes(x=michaelis.menton.slope.values, y=bioassay.survival.proportion)) +
    geom_point(aes(color = sd.population.resistance.values)) +
    theme_classic()+
    ylab("Bioassay Survival Proportion") +
    xlab("Michaelis Menton Equation Slope") +
    ggtitle(paste("Mean population resistance=",mean.population.resistance,
                  "& resistance for 50% survival=",half.population.bioassay.survival.resistance))
}


plot_michaelis_menton_slope_survival(michaelis.menton.slope.values = michaelis.menton.slope.values, 
                   sd.population.resistance.values = sd.population.resistance.values, 
                   maximum.bioassay.survival = 1,
                   half.population.bioassay.survival.resistance = 900, 
                   mean.population.resistance = 900,
                   nsim = 1000)

