stan_e1 = function(independent.variable =    independent.variable, 
                   dependent.variable = dependent.variable, 
                   controls = controls, data = data){
  y = data[,DV]
  x = data[,CONTROLS]
  z = data[,IV]
  ### Construct Stan Data Generator #####
  dat = list(
    N =length(y),
    P =ncol(x),
    X = x,
    Y = y,
    A = z,
    K = max(y)
  )
  
  ### Generate the appropriate stan model ####
  return(dat)
}