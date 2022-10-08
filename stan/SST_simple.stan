data {
  int<lower=0> N;//number of observations
  int<lower=0> n_point;//number of point
  int<lower=1,upper=n_point> point_id[N];//vector of sample indeces
  vector[N] y;
}
parameters {
  vector[n_point] gamma;//vector of point deviation from the average 
  real<lower=0> mu;//average max SST
  real<lower=0> sigma_gamma;//standard deviation of the gamma coeffs
  real<lower=0> sigma_y;//scale paramter of the observations
}
transformed parameters {
  vector[N] y_hat;
  for (i in 1:N)
    y_hat[i] = mu + gamma[point_id[i]] ;
}
model {
  //prior on the scale coefficient
  sigma_gamma ~ cauchy(0,2.5);
  sigma_y ~ gamma(2,0.1);
  //get point level deviation
  gamma ~ normal(0, sigma_gamma);
  //likelihood
  y ~ gumbel(y_hat, sigma_y);
}
generated quantities {
//sample predicted values from the model for posterior predictive checks
  real y_rep[N];
  for(n in 1:N)
    y_rep[n] =  gumbel_rng(y_hat[n],sigma_y);
}
