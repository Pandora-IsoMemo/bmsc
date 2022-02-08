data {
  int N; //the number of observations
  int K; //the number of columns in the model matrix
  int K1; //Intercept yes/no?
  int xunc;
  int betaConstr;
  int logitR;
  int ar1;
  real<lower = 0> varY;
  vector[N] y; //the response
  int<lower=0,upper=1> yL[N]; //the response (0/1);
  vector<lower = 0>[N] yUncertainty; //sd of uncertainties in Y
  matrix<lower = 0>[N,K] xUncertaintyMatrix; // sd of uncertainties in X
  matrix[N,K] X; //the model matrix
}
parameters {
  vector[K1] beta0; //the intercept
  simplex[K - K1] betaraw; //the regression parameters
  vector[K - K1] betarawS; //the regression parameters
  real logsigma; //the standard deviation
  vector[K] log_sigma_x; // prior scale
  matrix[N,K] XTRUE; //the model matrix of True values
  real<lower=-1,upper=1> ar;
}
transformed parameters{
  vector[N] sigma;
  vector[K] sigma_x; // prior scale
  vector[K - K1] beta;
  vector[K] betaAll;
  vector[N] epsAR1;
  vector[N] eps;

  if(betaConstr){
    beta = betaraw;
  } else {
    beta = betarawS;
  }
  sigma_x = exp(log_sigma_x);
  sigma = sqrt(square(exp(logsigma) * sqrt(varY)) + square(yUncertainty));
  betaAll = append_row(beta0, beta);
  if(!logitR){
  if(ar1){
    if(xunc){
      eps[1] = y[1] - XTRUE[1, ] * betaAll;
      epsAR1[1] = eps[1];
      for(i in 2:N){
        eps[i] = y[i] - XTRUE[i, ] * betaAll - epsAR1[i-1] * ar;
        epsAR1[i] = epsAR1[i-1] * ar + eps[i];
      }
    } else {
      eps[1] = y[1] - X[1, ] * betaAll;
      epsAR1[1] = eps[1];
      for(i in 2:N){
        eps[i] = y[i] - X[i, ] * betaAll - epsAR1[i-1] * ar;
        epsAR1[i] = epsAR1[i-1] * ar + eps[i];
      }
    }
  }
  }
}
model {
  logsigma ~ student_t(3, -2, 2);
  if(K1 > 0){
    beta0 ~ student_t(1, 0, 10); //prior for the intercept
  }
  beta ~ student_t(3, 0, 5); //prior for the slopes
  if(xunc){
  log_sigma_x ~ normal(0, 1);
  for (i in 1:K){
    XTRUE[,i] ~ normal(0, sigma_x[i]);
    X[,i] ~ normal(XTRUE[,i], xUncertaintyMatrix[,i] + 1E-4);
  }
  if(logitR){
    yL ~ bernoulli_logit(XTRUE * betaAll);
  } else {
    if(ar1){
      y[1] ~ normal(XTRUE[1,] * betaAll, sigma[1] / sqrt(1 - pow(ar, 2)));
      for(i in 2:N){
      y[i] ~ normal(XTRUE[i,] * betaAll + epsAR1[i-1] * ar, sigma[i]);
      }
    } else {
    y ~ normal(XTRUE * betaAll, sigma);
    }
  }
  } else {
  if(logitR){
    yL ~ bernoulli_logit(X * betaAll);
  } else {
    if(ar1){
      y[1] ~ normal(X[1,] * betaAll, sigma[1] / sqrt(1 - pow(ar, 2)));
      for(i in 2:N){
        y[i] ~ normal(X[i,] * betaAll + epsAR1[i-1] * ar, sigma[i]);
      }
    } else {
      y ~ normal(X * betaAll, sigma);
    }
  }
  }
}
generated quantities{
  vector[N] log_lik;
  vector[N] sigmasq;
  real rsq;
  for (n in 1:N) sigmasq[n] = square(sigma[n]);
  rsq = 1 - mean(sigmasq) / varY;
  if(xunc){
    if(logitR){
      for (n in 1:N) log_lik[n] = bernoulli_logit_lpmf(yL[n] | XTRUE[n, ] * betaAll) + normal_lpdf(X[n, ] |  XTRUE[n, ], xUncertaintyMatrix[n,] + 1E-4);
    } else {
      if(ar1){
        log_lik[1] = normal_lpdf(y[1] | XTRUE[1, ] * betaAll, sigma[1] / sqrt(1 - pow(ar, 2))) + normal_lpdf(X[1, ] |  XTRUE[1, ], xUncertaintyMatrix[1,] + 1E-4);
      for (n in 2:N) log_lik[n] = normal_lpdf(y[n] | XTRUE[n, ] * betaAll + epsAR1[n-1] * ar, sigma[n]) + normal_lpdf(X[n, ] |  XTRUE[n, ], xUncertaintyMatrix[n,] + 1E-4);
      } else {
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | XTRUE[n, ] * betaAll, sigma[n]) + normal_lpdf(X[n, ] |  XTRUE[n, ], xUncertaintyMatrix[n,] + 1E-4);
     }
    }
  } else {
    if(logitR){
        for (n in 1:N) log_lik[n] = bernoulli_logit_lpmf(yL[n] | X[n, ] * betaAll);
    } else {
      if(ar1){ 
        log_lik[1] = normal_lpdf(y[1] | X[1, ] * betaAll, sigma[1] / sqrt(1 - pow(ar, 2)));
      for (n in 2:N) log_lik[n] = normal_lpdf(y[n] | X[n, ] * betaAll + epsAR1[n-1] * ar, sigma[n]);
      } else {
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | X[n, ] * betaAll, sigma[n]);
      }
    }
  }
}
