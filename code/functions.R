# function to create random value from an autocorrelated linear model

# based upon input parameters create autocorrelated data 
sim_data <- function(n, xmax, yint, slope, sig, ac) {
  
  data.frame(xvals = seq(0, xmax, length.out = n),
             rans = rnorm(n, sig)) %>% 
    mutate(lag_rans = lag(rans),
           ac_rans = ifelse(is.na(lag_rans), rans, rans + ac * lag_rans),
           yvals = yint + slope * xvals + sqrt(1 - ac * ac) * ac_rans) %>% 
    dplyr::select(xvals, yvals)
}    

# this function:
  # creates a dataframe of parameter inputs
  # runs the sim_data function for each row of the dataset via pmap (parameter mapping in the purrr package)
  # ungroups the data
  # runs a regression for each slope and ac value
  # extracts the regresion results
  # filters out the results for just the slopes "xvals"
  # creates a column of 1 and 0 for the p.value condition

f_run_it <- function(n, xmax, yint, slope, sig, ac){
  expand.grid(n = n, 
              xmax = xmax, 
              yint = yint, 
              slope = seq(0, 0.5,length.out = n_slope), 
              sig = sig, 
              ac = seq(0, 0.9, length.out = n_ac)) %>% 
    mutate(data = pmap(list(n=n, xmax=xmax, yint=yint, slope=slope, sig=sig, ac=ac), sim_data)) %>% 
    unnest(data) %>% 
    group_by(slope, ac) %>% 
    do(fit = lm(yvals ~ xvals, data = .)) %>% 
    tidy(fit) %>% 
    filter(term=='xvals') %>% 
    mutate(pow = ifelse(p.value < 0.05, 1, 0)) }

# this function takes the simulation output and calculates the power by slope and ac
f_calc_power <- function(output){
  
  output %>% 
    bind_rows %>% 
    group_by(slope, ac) %>% 
    summarise(power = sum(pow) / n())
}
