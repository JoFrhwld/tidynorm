// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h>
#include <cmath>
using namespace Rcpp;

NumericVector seqC(double startNum, double endNum, int n){
  double rangeNum = endNum - startNum;
  double step = rangeNum / (n - 1);
  NumericVector out(n);

  out[0] = startNum;
  for(int i=1; i<n; ++i){
    out[i] = out[i - 1] + step;
  }
  return out;
}

// [[Rcpp::export]]
double cos_fun(double j, int k, int N){
  return cos((M_PI*k*(2*j+1))/(2*N));
}

double sin_fun(double j, int k, int N){
  return sin((M_PI*k*(2*j+1))/(2*N));
}

// [[Rcpp::export]]
arma::mat cos_bank(int jj, int kk){
  arma::mat bank = arma::mat(jj, kk);
  double o = 1/sqrt(2);

  for(int j = 0; j < jj; ++j){
    for(int k = 0; k < kk; ++k){
      bank(j,k) = cos_fun(j, k, jj)*2;
    }
  }

  bank.col(0) = bank.col(0) * o;

  return bank;
}

// [[Rcpp::export]]
arma::mat dct_fun(arma::vec y, int kk){

  arma::mat x_init = cos_bank(y.size(), kk);
  arma::uvec finites = arma::find_finite(y);
  arma::vec y2 = y(arma::find_finite(y));
  int nfinite = finites.n_rows;
  arma::mat x(nfinite, kk);

  int new_row = 0;
  for(int i = 0; i < y.n_rows; ++i){
    if(std::isfinite(y(i))){
      x.row(new_row) = x_init.row(i);
      new_row +=1;
    }
  }
  if(nfinite < y.n_rows){
    x.shed_cols(nfinite, y.n_rows-1);
  }

  arma::vec coefs = arma::inv(x.t() * x) * x.t() * y2;

  return coefs.col(0);
}

// [[Rcpp::export]]
arma::mat dct_mat(arma::mat y, int kk){
  arma::mat coefs(kk, y.n_cols);

  for(int i = 0; i < y.n_cols; ++i){
    coefs.col(i) = dct_fun(y.col(i), kk);
  }

  return coefs;
}

// [[Rcpp::export]]
arma::mat idct_fun(arma::vec y, int n){
  int N = y.size();
  arma::mat basis = cos_bank(n, N);
  arma::mat x = basis * y;

  return x;
}

// [[Rcpp::export]]
arma::mat idct_mat(arma::mat y, int n){
  arma::mat x(n, y.n_cols);

  for(int i = 0; i < y.n_cols; ++i){
    x.col(i) = idct_fun(y.col(i), n);
  }

  return x;
}


// [[Rcpp::export]]
NumericVector idct_prime(NumericVector y,int n){
  int N = y.size();
  NumericVector j = seqC(0, n-1, n);
  NumericVector x(n);

  for(int k = 1; k < N; ++k){
    float midterm = (M_PI * k)/n;
    for(int i = 0; i < n; ++i){
      x[i] += (-2 * y[k] * midterm * sin_fun(j[i], k, n));
    }
  }

  return x;
}


// [[Rcpp::export]]
NumericVector idct_dprime(NumericVector y,int n){
  int N = y.size();
  NumericVector j = seqC(0, n-1, n);
  NumericVector x(n);

  for(int k = 1; k < N; ++k){
    float midterm = pow((M_PI * k)/n,2);
    for(int i = 0; i < n; ++i){
      x[i] += -2 * y[k] * midterm * cos_fun(j[i], k, n);
    }
  }

  return x;
}

// [[Rcpp::export]]
NumericVector idct_interp(NumericVector y, int n, double p){
  NumericVector x = NumericVector(wrap(idct_fun(y, n)));
  NumericVector rate = idct_prime(y, n);
  NumericVector accel = idct_dprime(y, n);
  NumericVector j = seqC(0, n-1, n);
  NumericVector j_prop(n);
  NumericVector out(2);
  for(int i = 0; i < n; ++i){
    j_prop[i] = (2.0 * j[i] + 1)/(2.0*n);
  }

  int low_idx;

  for(int i=0; i<n; ++i){
    if(j_prop[i] > p){
      low_idx = i;
      break;
    }
  }

  int high_idx = n-1-low_idx;

  for(int i=low_idx-1; i>=0; --i){
    accel[i] = accel[low_idx];
    rate[i] = rate[i+1]-accel[i+1];
    x[i] = x[i+1] - rate[i+1];
  }

  for(int i=high_idx+1; i < n; ++i){
    accel[i] = accel[high_idx];
    rate[i] = rate[i-1] + accel[i-1];
    x[i] = x[i-1] + rate[i-1];
  }
  return x;
}
