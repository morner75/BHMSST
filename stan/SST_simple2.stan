data {
  int<lower=0> N;//number of observations
  int<lower=0> Nnew;//number of observations
  int<lower=0> n_point;//number of point
  int<lower=0> n_indvar;//number of predictors
  int<lower=1,upper=n_point> point_id[N];//vector of sample indeces
  vector[N] y; //response
  matrix[N,n_indvar] X; // design matrix
  matrix[Nnew,n_indvar] Xnew; 
}
parameters {
  vector[n_indvar] beta1;//vector of coefficients for location parameter
  vector[n_indvar] beta2;//vector of coefficients for scale paramenter
  //real<lower=0> tau2;//scale random effect
  //real<lower=0> sigma_y;//scale paramter of the observations
  real<lower=0> sigma_beta1;//standard deviation of the gamma coeffs
  real<lower=0> sigma_beta2;
}
transformed parameters {
  vector[N] a ;
  vector[N] b ;
  for (i in 1:N){
    a[i] = X[i]*beta1;
    b[i] = X[i]*beta2;
}
}
model {
  //prior on the scale coefficient
  sigma_beta1 ~ cauchy(0,2.5);
  sigma_beta2 ~ cauchy(0,2.5);
  //sigma_y ~ gamma(2,0.1);
  //get point level deviation
  beta1 ~ normal(0, sigma_beta1);
  beta2 ~ normal(0, sigma_beta2);
  //likelihood
  y ~ gumbel(a, exp(b));
}
generated quantities {
//sample predicted values from the model for posterior predictive checks
  real y_pred[Nnew];
  for(n in 1:Nnew)
    y_pred[n] =  gumbel_rng(Xnew[n]*beta1,exp(Xnew[n]*beta2));
}
