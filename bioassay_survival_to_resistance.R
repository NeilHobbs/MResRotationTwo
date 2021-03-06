#Function to convert bioassay survival to mean population insecticide resistance


bioassay_survival_to_resistance = function(maximum.bioassay.survival.proportion,
                                           michaelis.menton.slope, 
                                           half.population.bioassay.survival.resistance, 
                                           bioassay.survival, 
                                           estimate.precision, 
                                           sd.population.resistance,
                                           nsim,
                                           minimum.resistance.value, 
                                           maximum.resistance.value){
  while((test.population.resistance = ((minimum.resistance.value + maximum.resistance.value)/2))){
    if((maximum.resistance.value - minimum.resistance.value) < estimate.precision)
      {return(test.population.resistance)} #When precision level reached return population resistance
    else(
      if(resistance_to_bioassay_survival(
        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
        mean.population.resistance = test.population.resistance, 
        michaelis.menton.slope = michaelis.menton.slope, 
        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance, 
        sd.population.resistance = sd.population.resistance, 
                        nsim = nsim) < bioassay.survival) #check if survival 
      {
        minimum.resistance.value = test.population.resistance} 
      else(maximum.resistance.value = test.population.resistance))
   }
}



bioassay_survival_to_resistance(maximum.bioassay.survival.proportion = 1,
                                 michaelis.menton.slope =1, 
                                 half.population.bioassay.survival.resistance=900, 
                                 bioassay.survival = 0.5, 
                                 estimate.precision = 0.01, 
                                 sd.population.resistance = 5,
                                 nsim = 1000,
                                 minimum.resistance.value = 0, 
                                 maximum.resistance.value = 10000)


