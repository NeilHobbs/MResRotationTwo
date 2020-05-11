#Function to turn insecticide selection into selection response, using the
#Breeder's Equation

#h_squared is heritability of the trait
#m = 
#x = insecticide exposure
#B = factor converting exposure into selection differential 
#R = change in mean IR

library(ggplot2)

selection_response = function(B, nrep, h.min, h.max,
                              m.min, m.max, x.min, x.max){
  
  h_squared = runif(nrep, min = h.min, max=h.max)
  m = runif(nrep, min = m.min, max=m.max)
  x = runif(nrep, min = x.min, max=x.max)
  
  R = B*((h_squared) * x * (1 + m)/2) #Breeders Equation
  
  return(data.frame(R)) ##Return insecticide selection response
}


R = (selection_response(B=12.7, nrep=1000, h.min= 0.05, h.max = 0.3,
                        m.min=0, m.max=1, x.min=0.4, x.max=0.9))

R = 10/R

##Check median value
median(R$R)
##Centering around median = 10, B=~12.7


#Plot to give years to 10% Survival (assuming 10 generations per year)
ggplot(data = R, aes(x=R)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  xlab("Years to 10% Survival") +
  theme_classic()


##






