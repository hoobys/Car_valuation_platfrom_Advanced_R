#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double price_accuracy_ratio(NumericVector predicted, NumericVector actual)
{
  int n = predicted.size();
  double sum = 0.0;

  for (int i = 0; i < n; i++)
  {
    sum += std::abs(predicted[i] - actual[i]) / actual[i];
  }

  return sum / n;
}

// [[Rcpp::export]]
List segment_specific_mae(NumericVector predicted, NumericVector actual)
{
  NumericVector segments = NumericVector::create(10000, 25000, 50000, 75000, 100000);
  NumericVector mae_per_segment(segments.size());

  for (int i = 0; i < segments.size(); ++i)
  {
    double sum = 0.0;
    int count = 0;
    for (int j = 0; j < actual.size(); ++j)
    {
      if (actual[j] <= segments[i] && (i == 0 || actual[j] > segments[i - 1]))
      {
        sum += std::abs(predicted[j] - actual[j]);
        ++count;
      }
    }
    mae_per_segment[i] = (count > 0) ? sum / count : NA_REAL;
  }

  return List::create(Named("segments") = segments,
                      Named("mae") = mae_per_segment);
}