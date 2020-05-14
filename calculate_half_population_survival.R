##Function that enables calculation of a new 

#Function to convert bioassay survival to mean population insecticide resistance


calculate_half_population_survival = function(desired.resistance,
                                       desired.survival.proportion,
                                           maximum.bioassay.survival.proportion,
                                           michaelis.menton.slope, 
                                           estimate.precision, 
                                           sd.population.resistance,
                                           nsim,
                                           minimum.resistance.value, 
                                           maximum.resistance.value){
  while((half.population.survival.value = ((minimum.resistance.value + maximum.resistance.value)/2))){
    if((maximum.resistance.value - minimum.resistance.value) < estimate.precision)
    {return(half.population.survival.value)} #When precision level reached return population resistance
    else(
      if(resistance_to_bioassay_survival(
        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
        mean.population.resistance = desired.resistance, 
        michaelis.menton.slope = michaelis.menton.slope, 
        half.population.bioassay.survival.resistance = half.population.survival.value, 
        sd.population.resistance = sd.population.resistance, 
        nsim = nsim) > desired.survival.proportion) #check if survival 
      {
        minimum.resistance.value = half.population.survival.value} 
      else(maximum.resistance.value = half.population.survival.value))
  }
}



calculate_half_population_survival(desired.resistance = 100,
                                  desired.survival.proportion = 0.1,
                                  maximum.bioassay.survival.proportion = 1,
                                  michaelis.menton.slope = 1, 
                                  estimate.precision = 0.01, 
                                  sd.population.resistance = 0,
                                  nsim = 1000,
                                  minimum.resistance.value = 0, 
                                  maximum.resistance.value = 5000)


