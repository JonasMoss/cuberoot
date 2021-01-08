#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
std::vector< double > bootIndependent(std::vector < double > zz,long N){
  long n = zz.size();
  std::vector < double > boots(N);
  /* We will add N bootstrap replications to the boots vector. */
  for (int i = 0; i < N; i++) {
    boots[i] = 0;
    for (int j = 0; j < n; j++){
      boots[i] += zz[rand() % n];
    }
  }
  return (boots);
}

// [[Rcpp::export]]
std::vector< double > bootMarkov(std::vector < double > zz,long N,int sumTimes){
  long n = zz.size();
  long newIndex;
  long oldIndex;
  std::vector < double > boots(N);
  std::vector < long > indices(n);
  boots[0] = 0;
  
  /* We make an initial bootstrap and modify one element at a time. */
  
  for (int j = 0; j < n; j++){
    indices[j] = rand() % n;
    boots[0] += zz[indices[j]];
  }
  
  for (int i = 1; i < N; i++) {
    boots[i]  = boots[i-1];
    
    for (int k = 0; k < sumTimes; k++){
    oldIndex  = rand() % n;
    newIndex  = rand() % n;
    boots[i] -= zz[indices[oldIndex]];
    boots[i] += zz[newIndex];
    indices[oldIndex] = newIndex;
    }
    
  }
  
  return (boots);
}

// [[Rcpp::export]]
std::vector< std::vector < double > > bootSubs(std::vector < double > zz,long N,int lower,int upper){
  long n = zz.size();
  int counts = upper - lower + 1;
  std::vector < std::vector < double > > boots(counts, std::vector< double >(N));
  
  for (int i = 0; i < N; i++) {
      
      boots[0][i] = zz[rand() % n];
      
      for (int j = 1; j < counts; j++){
        boots[j][i] = boots[j-1][i] + zz[rand() % n];
      }
  }
  
  return(boots);
}


// [[Rcpp::export]]
std::vector< std::vector < double > > bootUniform(std::vector < double > unifs,long N,int lower,int upper){
  long n = unifs.size();
  int counts = upper - lower + 1;
  double temp;
  std::vector < std::vector < double > > boots(counts, std::vector< double >(N));
  
  for (int i = 0; i < N; i++) {
      
      boots[0][i] = unifs[rand() % n];
      
      for (int j = 1; j < counts; j++){
        temp = unifs[rand() % n];
        if (temp > boots[j-1][i]) {
           boots[j][i] = temp;
        } else {
          boots[j][i] = boots[j-1][i];
        }
      }
  }
  
  return(boots);
}

