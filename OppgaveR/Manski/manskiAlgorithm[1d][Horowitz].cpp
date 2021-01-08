#include <cmath>
#include <vector>
#include <Rcpp.h>

class Point { 
  public: 
    /* These values should be there from the beginning, when the struct is
  * instantiated. */
    double value;
  double weight;
  bool isRed;
  
  double redLeft;
  double blueRight;
  
  double posWeight;
  
  Point(double _value, double _weight, bool _isRed);
  Point();
  
}; 

typedef std::vector <Point> Points;

inline bool operator < (const Point pointOne, const Point pointTwo) { 
  return pointOne.value < pointTwo.value; 
}

inline bool operator > (const Point pointOne, const Point pointTwo) { 
  return pointOne.value > pointTwo.value; 
}

Point::Point(){
}

Point::Point(double _value, double _weight, bool _isRed) {
  /* The basic values of the object is defined. */
    value      = _value;
  weight     = _weight;
  isRed      = _isRed;
  posWeight     = 0;
  
  /* The quickredblue procedure will never update a point 
  * with its own colors. This is done here. */
    if (isRed){
      redLeft   = weight; 
      blueRight = 0; 
    } else {
      redLeft   = 0; 
      blueRight = weight;     
    }
}

void updateColors(Points &_points){
  long n = _points.size() - 1;
  
  /* We iterate trough all points. At each point, we update its redLeft
  * etc values by adding what was known at the previous step of the iteration
  */
    
    for (long i = (n-1); i >= 0; i--){
      _points[n].redLeft += _points[i].redLeft;
    } 
  
  for (long i = (n-1); i >= 0; i--){
    _points[i].blueRight += _points[i+1].blueRight;
    if (_points[i+1].isRed){
      _points[i].redLeft   = _points[i+1].redLeft - _points[i+1].weight;
    } else {
      _points[i].redLeft   = _points[i+1].redLeft;
    }
  }
}

void updateWeights(Points &_points){
  long n = _points.size();
  for (long i = 0; i < n; i++){
    _points[i].posWeight = _points[i].blueRight + _points[i].redLeft;
  }
}


// [[Rcpp::export]]
Rcpp::NumericMatrix redBlue(Rcpp::NumericVector values,Rcpp::NumericVector weights,
                            Rcpp::IntegerVector isReds,bool isSorted){
  
  long n = values.size();
  Points points(n+2);
  for (int i = 1; i < (n+1); i++){
    points[i] = Point(values[i-1],weights[i-1],isReds[i-1]);
  }
  
  points[n+1]   = Point( 1.0/0.0, 0, false);
  points[0]     = Point(-1.0/0.0, 0, true);
  
  if (!isSorted) std::sort(points.begin(),points.end());
  
  updateColors(points);
  updateWeights(points);
  
  Rcpp::NumericMatrix valuesAndWeights(3,n+2);
  for (int i = 0; i < n+2; i++){
    valuesAndWeights(0,i) = points[i].value;
    valuesAndWeights(1,i) = points[i].isRed;
    valuesAndWeights(2,i) = points[i].posWeight;
  }
  
  return (valuesAndWeights);
}
/*
  Points updatePoints(Points &points,const Point newPoint){
    long n = points.size();
    int index;
    
    if (newPoint.isRed){
      index = 
    } else {
      
    }
  }
*/
  
  
  
  