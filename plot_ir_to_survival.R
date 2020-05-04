##Graph of Insecticide Resistance and Survival (ir_to_survival)
library(ggplot2)
library(dplyr)


#sd.values = c(rep(0.1, 10001), rep(0.5, 10001), rep(1, 10001), rep(5, 10001), rep(20, 10001), rep(25, 10001))
         
ir.values = seq(0, 25000, by = 1)   
##


plot_ir_to_survival = function(Kmax, n, z_50, nsim, min.ir, max.ir, sigma){

  df=data.frame(ir.values=seq(min.ir, max.ir, by = 1))%>%
  rowwise%>%
  mutate(Y.values = ir_to_survival(Kmax = Kmax, z = ir.values, n=n, z_50=z_50, sigma = sigma, nsim=nsim)) ##plotting with sigma as 0.1 or 25 made no difference to plots

ggplot(df, aes(x=ir.values, y = Y.values)) +
  geom_point(colour = "blue") +
  xlab("Insecticide Resistance Intensity (z)") +
  ylab("Survival in CDC Bottle Bioassay")+
  theme_classic()
}

##I was finding that including sigma ranges (0.1 to 25) were not having any influence on the final plot
plot_ir_to_survival(Kmax = 1, n=1, z_50 = 900, nsim = 1000, min.ir = 0, max.ir = 25000, sigma=1)
