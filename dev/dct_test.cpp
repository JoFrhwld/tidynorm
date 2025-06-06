#include <Rcpp.h>
using namespace Rcpp ;

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

double cos_fun(double j, int k, int N){
  return cos((M_PI*k*(2*j+1))/(2*N));
}

// [[Rcpp::export]]
NumericVector dct_fun2(NumericVector x){
  double kk = x.length();
  NumericVector y(kk);
  double j_size = x.length();
  NumericVector j_vec = seqC(0, j_size-1, x.size());
  NumericVector k_vec = seqC(0, kk-1, kk);
  NumericMatrix cos_bank(kk, j_size);
  double coef = 0.0;
  double orth = sqrt(2);

  for(int k = 0; k < kk; k++){
    if (k > 0){
      orth = 1;
    }
    for(int j=0; j < j_size; j++){
      y[k] += (x[j] * cos_fun(j_vec[j], k, j_size))/(j_size*orth);
    }
  }

  return y;

}
