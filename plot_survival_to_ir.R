##Plot survival_to_ir function
library(ggplot2)
library(dplyr)


plot_survival_to_ir = function(Kmax, n, z_50, precision, sigma, nsim, min_value, max_value,
                               minY, maxY, divisions){
  
  df=data.frame(Y.values=seq(minY, maxY, by = divisions))%>%
    rowwise%>%
    mutate(ir.values = survival_to_ir(Kmax=Kmax, n =n, z_50=z_50, req_surv = Y.values, precision =precision,
                                     sigma = sigma, 
                                     nsim = nsim, 
                                     min_value = min_value,
                                     max_value = max_value)) ##plotting with sigma as 0.1 or 25 made no difference to plots
  
  ggplot(df, aes(x=ir.values, y = Y.values)) +
    geom_point(colour = "red") +
    xlab("Insecticide Resistance Intensity (z)") +
    ylab("Survival in CDC Bottle Bioassay")+
    theme_classic()
}

plot_survival_to_ir(Kmax = 1, n=1, z_50 = 900, precision = 0.01, sigma = 25, nsim = 1000, min_value = 0,
                    max_value = 25000, minY=0, maxY = 1, divisions = 0.01)



