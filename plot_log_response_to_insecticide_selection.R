#Plot to give years to 10% Survival (assuming 10 generations per year)
plot_log_response_to_insecticide_selection = function(
  exposure.scaling.factor,
  nsim, 
  minimum.insecticide.resistance.hertitability, 
  maximum.insecticide.resistance.hertitability,
  minimum.male.insecticide.exposure,
  maximum.male.insecticide.exposure, 
  minimum.female.insecticide.exposure, 
  maximum.female.insecticide.exposure
){
  
  output = (response_to_insecticide_selection(
    exposure.scaling.factor = exposure.scaling.factor,
    nsim = nsim, 
    minimum.insecticide.resistance.hertitability = minimum.insecticide.resistance.hertitability, 
    maximum.insecticide.resistance.hertitability = maximum.insecticide.resistance.hertitability,
    minimum.male.insecticide.exposure = minimum.male.insecticide.exposure,
    maximum.male.insecticide.exposure = maximum.male.insecticide.exposure, 
    minimum.female.insecticide.exposure = minimum.female.insecticide.exposure, 
    maximum.female.insecticide.exposure = maximum.female.insecticide.exposure))
  
  ##Scale to 10 generations per year and log10
  output= log10(10/R)
  
  #Coerce into a dataframe to allow for plotting
  output=data.frame(output) 
  
  #Histogram of years (logged)
 plot.output =  ggplot(data = output, aes(x=output)) +
    geom_histogram(fill = "grey", colour = "black") +
    xlab("log10(Years to 10% Survival)") +
    theme_classic()
  
  return(plot.output)
}

#Check code works
plot_log_response_to_insecticide_selection(
  exposure.scaling.factor = 10,
  nsim = 1000, 
  minimum.insecticide.resistance.hertitability = 0.05, 
  maximum.insecticide.resistance.hertitability = 0.3,
  minimum.male.insecticide.exposure = 0,
  maximum.male.insecticide.exposure = 1, 
  minimum.female.insecticide.exposure = 0.4, 
  maximum.female.insecticide.exposure = 0.5
)



