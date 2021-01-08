#include <cmath>
#include <vector>
#include <iostream>
#include <algorithm>
#include <list>
#include <Rcpp.h>


class Point { 
public: 
  double value;
  long weight;
  long posWeight;
  double redLeft;
  double blueRight;
  bool isRed;
  Point(double _value, long _weight, bool _isRed);
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

Point::Point(double _value, long _weight, bool _isRed) {
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

template <typename T> int signum(T val) {
    return (T(0) < val) - (val < T(0));
}

void updateWeights(Points &_points){
  long n = _points.size();
  for (long i = 0; i < n; i++){
    _points[i].posWeight = _points[i].blueRight + _points[i].redLeft;
  }
}

long getWeightMax(Points points){
    long tempweight = 0;
    long n = points.size();
    for(int i = 0; i < n; i++){
        tempweight = std::max(tempweight,points[i].posWeight);
    }
    return(tempweight);
}

inline bool getColor(double const slopeCurrent,double const slope,
                     bool const isRed){
  bool whichColor;
  
  if (slopeCurrent > slope) whichColor = isRed;
  else whichColor = !isRed;

  return (whichColor);     
}



Points redBlue(std::vector < double > values,
               std::vector < long > weights, 
               std::vector < bool   >   isReds){
              
  long n = values.size();
  Points points(n+2);
  
  for (int i = 1; i < (n+1); i++){
    points[i] = Point(values[i-1],weights[i-1],isReds[i-1]);
  }
  
  points[n+1]   = Point( 1.0/0.0, 0, false);
  points[0]     = Point(-1.0/0.0, 0, true);

  std::sort(points.begin(),points.end());
  updateColors(points);
  updateWeights(points);
  
  return (points);
}


// [[Rcpp::export]]
Rcpp::List redBlue2(std::vector < double > xx1,
                             std::vector < double > xx2, 
                             std::vector < bool > isReds,
                             std::vector < long > weights) {

  long nobs  = xx1.size();
  long currentWeight;
  long maxWeight = -10000;
  
  std::vector < double > points(nobs-1);
  std::vector < bool >   colors(nobs-1);
  std::vector < Points > storage(nobs-1);
  std::list < double > xCoord;
  std::list < double > yCoord;
  Points redBlues;
  
  /* The main part of the algorithm consists of computing redBlue for each line in
   * in the arrangement. In order to this, we will have to calculate all points of
   * intersection and all the corresponding colors. The following loop will run trough
   * this algorithm for us. */
  
  for (int lineCount = 0; lineCount < nobs; lineCount++){
    double slopeCurrent = -xx2[lineCount]/xx1[lineCount];
    double x11 = xx1[lineCount];
    double x12 = xx2[lineCount];

    for (int intersectionCount = 0; intersectionCount < lineCount; intersectionCount++){
      bool isRed = isReds[intersectionCount];      
      double slope = -xx2[intersectionCount]/xx1[intersectionCount];  
      double x21 = xx1[intersectionCount];
      double x22 = xx2[intersectionCount];      
      
      colors[intersectionCount] = getColor(slopeCurrent,slope,isRed);
      points[intersectionCount] = (x21-x11)/(x22*x11-x21*x12);
      
    }
    
    for (int intersectionCount = lineCount +1; intersectionCount < nobs; intersectionCount++){
      bool isRed = isReds[intersectionCount];      
      double slope = -xx2[intersectionCount]/xx1[intersectionCount];  
      double x21 = xx1[intersectionCount];
      double x22 = xx2[intersectionCount];      
      
      colors[intersectionCount-1] = getColor(slopeCurrent,slope,isRed);
      points[intersectionCount-1] = (x21-x11)/(x22*x11-x21*x12);   
    }
    
    /* In order to call redBlue we will erase the values. When we're done it's safe
     * to add them once again. */
    currentWeight   = weights[lineCount];
    weights.erase(weights.begin()+lineCount);
    redBlues = redBlue(points,weights,colors);
  
    
    /* We begin the updating operation. */
    long tempWeight = maxWeight;
    maxWeight = std::max(getWeightMax(redBlues),maxWeight);
    if (tempWeight < maxWeight){
        
        /* In this case, we will remove all entries from our 
         * xCoord and yCoord lists, and replace them with the new
         * and superior entries from redBlues! */
         
        xCoord.clear();
        yCoord.clear();
        
        double slope = -xx2[lineCount]/xx1[lineCount];
        double intercept = -1/xx1[lineCount];

        for (int i = 0; i < (nobs + 1); i++){
            if (redBlues[i].posWeight >= maxWeight) {
                if(redBlues[i].value >= pow(10,10)) {
                  xCoord.push_front(pow(10,10));
                  yCoord.push_front(slope*pow(10,10)+intercept);
                } else if (redBlues[i].value <= - pow(10,10)) {
                  xCoord.push_front(-pow(10,10));      
                  yCoord.push_front(slope*(-pow(10,10)+intercept));
                } else {
                  xCoord.push_front(redBlues[i].value);
                  yCoord.push_front(slope*redBlues[i].value+intercept);
                }
            }
        }
    }
    
    else if (tempWeight == maxWeight){
        
        double slope = -xx2[lineCount]/xx1[lineCount];
        double intercept = -1/xx1[lineCount];
        
        for (int i = 0; i < (nobs + 1); i++){
            if (redBlues[i].posWeight >= maxWeight) {
                if(redBlues[i].value >= pow(10,10)) {
                  xCoord.push_front(pow(10,10));
                  yCoord.push_front(slope*pow(10,10)+intercept);
                } else if (redBlues[i].value <= - pow(10,10)) {
                  xCoord.push_front(-pow(10,10));      
                  yCoord.push_front(slope*(-pow(10,10)+intercept));
                } else {
                  xCoord.push_front(redBlues[i].value);
                  yCoord.push_front(slope*redBlues[i].value+intercept);
                }
            }
        }
        
    }
    
    weights.insert(weights.begin()+lineCount,currentWeight);
  }

  
 return Rcpp::List::create(Rcpp::Named("xCoord")    = xCoord,
                           Rcpp::Named("yCoord")    = yCoord,
                           Rcpp::Named("maxWeight") = maxWeight + 1);
}
