#Function to turn insecticide selection into selection response, using the
#Breeder's Equation

#h_squared is heritability of the trait
#m = 
#x = insecticide exposure
#B = factor converting exposure into selection differential 
#R = change in mean IR 
#nrep = number of repitions of uniform distribution

library(ggplot2)
library(gridExtra)
library(dplyr)

response_to_insecticide_selection = function(exposure.scaling.factor,
                                             nsim, 
                                             minimum.insecticide.resistance.hertitability, 
                                             maximum.insecticide.resistance.hertitability,
                                             minimum.male.insecticide.exposure,
                                             maximum.male.insecticide.exposure, 
                                             minimum.female.insecticide.exposure, 
                                             maximum.female.insecticide.exposure){
  
  insecticide.resistance.hertitability = runif(nsim, 
                                               min = minimum.insecticide.resistance.hertitability, 
                                               max=maximum.insecticide.resistance.hertitability)
  
  male.insecticide.exposure.proportion = runif(nsim,
                                               min = minimum.male.insecticide.exposure, 
                                               max=maximum.male.insecticide.exposure)
  
  female.insecticide.exposure.proportion= runif(nsim, 
                                                min = minimum.female.insecticide.exposure, 
                                                max=maximum.female.insecticide.exposure)
  
  resistance.selection.reponse = exposure.scaling.factor*((insecticide.resistance.hertitability) * female.insecticide.exposure.proportion * (1 + male.insecticide.exposure.proportion)/2) #Breeders Equation
  
  return(resistance.selection.reponse) ##Return insecticide selection response as a data frame
}


R = response_to_insecticide_selection(exposure.scaling.factor = 10,
                                      nsim = 1000, 
                                      minimum.insecticide.resistance.hertitability = 0.05, 
                                      maximum.insecticide.resistance.hertitability = 0.3,
                                      minimum.male.insecticide.exposure = 0,
                                      maximum.male.insecticide.exposure = 1, 
                                      minimum.female.insecticide.exposure = 0.4, 
                                      maximum.female.insecticide.exposure = 0.9)



#Plot to give years to 10% Survival (assuming 10 generations per year)
plot_function = function(B){
  
  R = selection_response(B=B, nrep=1000, h.min= 0.05, h.max = 0.3,
                         m.min=0, m.max=1, x.min=0.4, x.max=0.9)
  
  R = 10/R 
  
  ggplot(data = R, aes(x=R)) +
    geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
    #scale_x_continuous(breaks = seq(-100, 100, 100))+
    xlab("Years to 10% Survival") +
    theme_classic()  +
    ggtitle(paste0("B =", B))
}

#Finding suitable B value to centre around 10 Years
B8 = plot_function(8)
B9 = plot_function(9)
B10 = plot_function(10)
B11 = plot_function(11)

##
grid.arrange(B8, B9, B10, B11)

log_plot_function = function(B){
  
  R = selection_response(B=B, nrep=10000, h.min= 0.05, h.max = 0.3,
                         m.min=0, m.max=1, x.min=0.4, x.max=0.9)
  
  R = log10(10/R)
  
  ggplot(data = R, aes(x=R)) +
    geom_histogram(binwidth = 0.1, fill = "grey", colour = "black") +
    #scale_x_continuous(breaks = seq(-30, 30, 60))+
    xlab("log(Years to 10% Survival)") +
    theme_classic()  +
    ggtitle(paste0("B =", B))
}

##Centre around 10 years (log10(10) = 1)


log12 = log_plot_function(12)
log9 = log_plot_function(9)
log10 = log_plot_function(10)
log11 = log_plot_function(11)

grid.arrange(log9, log10, log11, log12)

##logging the values certainly doesn't seem to help!
