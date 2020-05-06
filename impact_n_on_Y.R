##varying n in survival equations
#This function demonstrates the impact of changing n in the ir_to_survival function, and demonstrates that 
# the function only performs correctly when n = 1. As this is the only time that when z_50=900, and n=1, that 
# survival is 50%.

library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(gridExtra)


n.values = rep(seq(0, 2, by = 0.1), 6)
sd.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))

table_impact_n_on_Y = function(n.values, sd.values, Kmax, z_50, nsim, z){
    df = data.frame(n.values, sd.values)%>%
    rowwise()%>%
    mutate(Y.values = ir_to_survival(Kmax = Kmax, z = z, n = n.values, z_50 = z_50, 
                                     sigma = sd.values, nsim = nsim))%>%
    mutate(sd.values = as.factor(sd.values))
    
    
    return(kable(df))
}


##run with z=900 and z_50=900
table_impact_n_on_Y(n.values = n.values, sd.values=sd.values, Kmax=1, z_50=900, nsim=1000, z=900)
##To me easier to see what is going on with the graph



plot_impact_n_on_Y = function(n.values, sd.values, Kmax, z_50, nsim, z){
  df = data.frame(n.values, sd.values)%>%
    rowwise()%>%
    mutate(Y.values = ir_to_survival(Kmax = Kmax, z = z, n = n.values, z_50 = z_50, 
                                     sigma = sd.values, nsim = 1000))%>%
    mutate(sd.values = as.factor(sd.values))
  
  ggplot(df, aes(x=n.values, y=Y.values)) +
    geom_point(aes(color = sd.values)) +
    theme_classic()+
    ylab("Survival") +
    xlab("n - slope of the curve") +
    ggtitle(paste("Using z=",z,", z_50=", z_50))
}


A = plot_impact_n_on_Y(n.values=n.values, sd.values=sd.values, Kmax=1, z_50=900, nsim=1000, z=20)
B = plot_impact_n_on_Y(n.values=n.values, sd.values=sd.values, Kmax=1, z_50=900, nsim=1000, z=100)
C = plot_impact_n_on_Y(n.values=n.values, sd.values=sd.values, Kmax=1, z_50=900, nsim=1000, z=900)
D = plot_impact_n_on_Y(n.values=n.values, sd.values=sd.values, Kmax=1, z_50=900, nsim=1000, z=3600)

grid.arrange(A, B, C, D)



