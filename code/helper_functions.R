stan_e1 = function(independent.variable =    independent.variable, 
                   dependent.variable = dependent.variable, 
                   controls = controls, data = data){
  y = data[,dependent.variable]
  x = data[,controls]
  z = data[,independent.variable]
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
##### Stqn Code #### 
ologit_baseline_stan <-
  'data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  vector[N] A; 
  int K; 
  int<lower=0,upper=6> Y[N];
}
transformed data {
  vector[N] boot_probs = rep_vector(1.0/N, N);  
}
parameters {
  vector[P + 1] alpha;
  ordered[K - 1] c;
}
transformed parameters{
  vector[P] alphaZ = head(alpha, P);
  real alphaA = alpha[P + 1];
}
model{
  alpha ~ normal(0, 2.5);
  Y ~ ordered_logistic(X * alphaZ + A * alphaA, c);
}
generated quantities {
    int row_i;
    real ATE1 = 0;
    real ATE2 = 0;
    real ATE3 = 0;
    vector[N] Yl_a1;
    vector[N] Yl_a0;
    vector[N] Ym_a1;
    vector[N] Ym_a0;
    vector[N] Yh_a1;
    vector[N] Yh_a0;
    vector[N] eta1;
    vector[N] eta2;
  for (n in 1:N) {
    row_i = categorical_rng(boot_probs);
    eta1[n] = X[row_i] * alphaZ;
    eta2[n]=  X[row_i] * alphaZ + alphaA;
    Yl_a1[n] = bernoulli_logit_rng(c[2] - eta2[n]);
    Yl_a0[n] = bernoulli_logit_rng(c[2] - eta1[n]);
    Ym_a1[n] = bernoulli_logit_rng(c[3] - eta2[n]) - bernoulli_logit_rng(c[2] - eta2[n]); 
    Ym_a0[n] = bernoulli_logit_rng(c[3] - eta1[n]) - bernoulli_logit_rng(c[2] - eta1[n]); 
    Yh_a1[n] = 1 - bernoulli_logit_rng(c[3] - eta2[n]); 
    Yh_a0[n] = 1 - bernoulli_logit_rng(c[3] - eta1[n]);   
    ATE1 = ATE1 + (Yl_a1[n] - Yl_a0[n])/N;
    ATE2 = ATE2 + (Ym_a1[n] - Ym_a0[n])/N;
    ATE3 = ATE3 + (Yh_a1[n] - Yh_a0[n])/N;
    }
}
'
