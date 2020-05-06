##This function demonstrates the impact of what happens when you change n in the survival_to_ir function,
## and demonstrates how the function only appears to perform correctly when n = 1. As when z_50=900 and
# req_surv = 0.5 (50%), this is the only time the corresponding z value matches the z_50 value. 


n.values = rep(seq(0, 2, by = 0.1), 6)
sd.values = c(rep(0.1, 21), rep(0.5, 21), rep(1, 21), rep(5, 21), rep(20, 21), rep(25, 21))


plot_impact_n_on_z = function(Kmax, n.values, z_50, req_surv, precision, sd.values, nsim, min_value, max_value){
  df = data.frame(n.values, sd.values)%>%
    rowwise()%>%
    mutate(z.values = survival_to_ir(Kmax, n=n.values, z_50, req_surv, precision,
                                     sigma=sd.values, nsim, min_value, max_value))%>%
    mutate(sd.values = as.factor(sd.values))
  
  ggplot(df, aes(x=n.values, y=z.values)) +
    geom_point(aes(color = sd.values)) +
    theme_classic()+
    ylab("Insecticide Resistance Intensity (z)") +
    xlab("n - slope of the curve") +
    ggtitle(paste("Using required survival=",req_surv,", z_50=", z_50))
}

plot_impact_n_on_z(Kmax=1, n.values=n.values, z_50=900, req_surv=0.5, precision=0.01,
     sd.values=sd.values, nsim=1000, min_value = 0, max_value = 5000)

