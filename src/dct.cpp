#include <Rcpp.h>
using namespace Rcpp;
#include <math.h>
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
NumericVector dct_fun(NumericVector x, int kk){
  NumericVector j_vec = seqC(0, x.size()-1, x.size());
  NumericVector y(kk);
  int j_size = y.size();

  for(int k = 0; k <= kk; ++k){
    y[k] = 0;
    for(int j; j < j_size; ++j){
      y[k] += x[j] * cos_fun(j, k, j_size);
    }
    y[k] = y[k]/j_size;
  }

  return y;

}


// [[Rcpp::export]]
NumericVector idct_fun(NumericVector y,int n){
  int N = y.size();
  // double adjust = (N*1.0)/(n*1.0);
  NumericVector j = seqC(0, n-1, n);
  NumericVector x(n);

  for(int i = 0; i < n; ++i){
    for(int k = 1; k < N; ++k){
      //x[i] = adjust;
      x[i] += y[k] * cos_fun(j[i], k, n);
    }
    x[i] = (2*x[i]) + (sqrt(2) * y[0]);
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
  NumericVector x = idct_fun(y, n);
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
