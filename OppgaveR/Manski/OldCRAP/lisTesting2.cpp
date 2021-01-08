#include <cmath>
#include <vector>
#include <Rcpp.h>
#include <list>


inline bool acute(double ax, double ay, double bx, double by){
  /* calculate the dot product and return its sign. */
  double dotprod = ax*bx + ay*by;
  if (dotprod >= 0) {
    return(true);
  } else {
    return(false);
  }
}

template <typename T> int signum(T val) {
    return (T(0) < val) - (val < T(0));
}

// [[Rcpp::export]]
bool intersection(double a, double b, bool isRed){
  /* Decide the x point of intersection. */
  //double xcoord = (x21*c1-x11*c2)/(x22*x11-x21*x12);
  //double ycoord = -c1/x11 - xcoord*x12/x11;
  
  if (signum(a) != signum(b)) {
    return(isRed);
  } else {
    if (a < b) {
      return(!isRed);
    } else {
      return(isRed);
    }
  }
  
}