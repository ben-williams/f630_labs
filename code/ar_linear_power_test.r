# Power of linear regresion models on autoregressive data

# load ----
source('code/helper.r')
source('code/functions.r')

# global inputs -----

n <- 100
xmax <- 10
n_slope <- 11
n_ac <- 10
sig = 0.7
yint <- 0.0
nsims = 10 # note that in the lab this is 2000 - set to 10 for quicker example

# replicate the function a specified number of times, save the output so that 
  # it is not necessary to run again if you want to do something else with the 
  # data
replicate(nsims, f_run_it(n = n, 
         xmax = xmax, 
         yint = yint, 
         slope = seq(0, 0.5, length.out = n_slope), 
         sig = sig, 
         ac = seq(0, 0.9, length.out = n_ac)), 
         simplify = FALSE) -> sim_out

# calculate the power via the f_calc_power function
# plot the output

f_calc_power(sim_out) %>%  
  ggplot(aes(slope, ac, size = power)) +
  geom_point(color = 'blue') +
  scale_size_area()
  
  
