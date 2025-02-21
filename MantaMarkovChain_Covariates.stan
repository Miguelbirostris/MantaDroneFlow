#Stan file with model specifications and parameterization. Must be saved in working directory for models to run in the main script.



// estimating parameters of a Markov chain

data {

  int<lower=1> T; // length of the time series
  int<lower=1> N; // number of behaviors
  int<lower=1, upper=N> behaviors[T];
  int<lower=1> index[T]; // index for individual time series
  
  int<lower=1> ncov; // number of covariates
  matrix[T, ncov+1] xmat; // matrix of covariates

}

parameters {

  // intercept and slope parameters
  matrix[ncov + 1, N] beta[N];
  
  // Nx1 vector
  simplex[N] initdist;

}


model {
  
  // difusse prior distribution for betas
  
  for(n in 1:N){
    for(k in 1:(ncov + 1))
     beta[n][k] ~ normal(0, 1);
  }
  

  // model specification
  behaviors[1] ~ categorical(initdist);
  
  for(t in 2:T){
    
    if(index[t] == index[t-1]){
      behaviors[t] ~ categorical(softmax(to_vector(xmat[t]*beta[behaviors[t-1]])));
    } else {
      behaviors[t] ~ categorical(initdist);
    }
  }
  
}

