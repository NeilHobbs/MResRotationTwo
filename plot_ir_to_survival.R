##Graph of Insecticide Resistance and Survival (ir_to_survival)
##This function uses the ir_to_survival function to plot the log of insecticide resistance (x axis) against
##survival in a bioassay (y axis)
##warning_surv = the survival at which it is recommended to look for resistance mechanisms (currently 2%, WHO)
##cutoff_surv = the survival at which switching insecticide is recommended (currently 10%, WHO)

#required packages by function
library(ggplot2)
library(dplyr)

plot_ir_to_survival = function(Kmax, n, z_50, nsim, min.ir, max.ir, sigma, warning_surv, cutoff_surv){

  df=data.frame(ir.values=seq(min.ir, max.ir, by = 1))%>%
  rowwise%>%
  mutate(Y.values = ir_to_survival(Kmax = Kmax, z = ir.values, n=n, z_50=z_50, sigma = sigma, nsim=nsim)) ##plotting with sigma as 0.1 or 25 made no difference to plots

ggplot(df, aes(x=log(ir.values + 1), y = Y.values)) + ##logging to increase ease of readibility
  geom_point(colour = "blue") +
  xlab("log(Insecticide Resistance Intensity (z))") + ##applying log(z) makes it more readable at the lower z levels.
  ylab("Survival in CDC Bottle Bioassay")+ #note: this label may need to be changed depending on what we calibrate against
  geom_hline(yintercept=warning_surv, colour = "orange")+ ##2% warning level as specified by WHO
  geom_hline(yintercept=cutoff_surv, colour = "red")+ ##10% survival cut-off as specified by WHO
  theme_classic()
}


#sd.values = c(rep(0.1, 10001), rep(0.5, 10001), rep(1, 10001), rep(5, 10001), rep(20, 10001), rep(25, 10001))
         
ir.values = seq(0, 5000, by = 1)   
##I was finding that including sigma ranges (0.1 to 25) were not having any influence on the final plot (overlapping)
plot_ir_to_survival(Kmax = 1, n=1, z_50 = 900, nsim = 1000, min.ir = 0, max.ir = 25000, sigma=1,
                    warning_surv = 0.02, cutoff_surv = 0.1)
