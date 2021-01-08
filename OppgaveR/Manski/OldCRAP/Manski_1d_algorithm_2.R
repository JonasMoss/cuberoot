objective = function(xx,yy,beta) {
  sum(yy*((xx*beta)>=-1) + (1-yy)*((xx*beta) < -1))
}

objective2 = function(xx,yy,beta) {
  sum(yy*((xx*beta)>=1) + (1-yy)*((xx*beta) < 1))
}

smoobjective = function(xx,yy,beta) {
  sum((2*yy-1)*exp(10*(xx*beta+1))/(1+exp(10*(xx*beta+1))))
}



manski = function(yy,xx,ww=rep(1,nobs)){
  # We start by massaging xx and yy into two separate lists,
  # combining the direction and values and of possible solutions.
  #
  # right: Vector containing possible solutions of the form (-inf,x>
  # left: Vector containing possible solutions of the form <x,inf)
  #
  # Both are sorted for convenience sake.

  nobs = length(xx)
  factors = 2*(yy) + (xx>=0)
  splitted = split(-1/xx,2*(yy) + (xx>=0))
  
  if (is.null(splitted$'1') & is.null(splitted$'2')) {
    right = NULL
  } else {
    right = sort(c(splitted$'1',splitted$'2'))
  }
  
  if (is.null(splitted$'0') & is.null(splitted$'3')) {
    left = NULL
  } else {
    left = sort(c(splitted$'0',splitted$'3'),decreasing=TRUE)
  }

  # This function identifies maximal subsets for beta0 = 1 and
  # beta0 = -1 at the same time. 
  
  findSolutions = function(left,right) {
    
    if(is.null(left)) {
      return(list(ks=NULL,
                  k_max=length(right),
                  solutions=c(lower=-Inf,upper=min(right))))
    } else if (is.null(right)) {
      return(list(ks=NULL,
                  k_max=length(left),
                  solutions=c(lower=max(left),upper=Inf)))
    } else {
    
    len_left   = length(left)
    len_right  = length(right)
    k          = len_left + sum(right>=left[1])
    k_max      = k
    right_of   = right[right < left[1]]
    ks         = c(k)
    index      = 1
    
    while( ((k-1)+length(right_of)) > k_max & index < len_left){
      index     = index + 1
      k         = (k-1)  + sum(right_of > left[index],na.rm=TRUE)
      right_of  = right_of[(right_of < left[index])]
      k_max     = max(k, k_max)
      ks[index] = k
    }
    
    
    solution_set = list()
    solutions = left[c(ks == k_max,rep(FALSE,len_left))]
    
    for (i in 1:length(solutions)) {
      current = solutions[i]
      solution_set[[i]] = c(lower = current, 
                            upper = min(c(right[right >= current],Inf)))
    }  
    
    return(list(ks=ks,
                k_max=k_max,
                solutions=solution_set))
    }
  }
  
  # We proceed to find the best solutions when beta = 0.
  
  len_left = length(left)
  len_right = length(right)
  if (len_left == len_right) middle = c(lower=-Inf,upper=Inf,k=len_left)
  if (len_left > len_right) middle = c(lower=0,upper=Inf,k=len_left)
  if (len_left < len_right) middle = c(lower=-Inf,upper=0,k=len_right)
  
  if (is.null(left)) {
    negative = findSolutions(NULL,rev(-right))
  } else if (is.null(right)) {
    negative = findSolutions(rev(-left),NULL)
  } else {
    negative = findSolutions(rev(-left),rev(-right))
  }
  
  return(list(positive   = findSolutions(left,right),
              zero       = middle,
              negative   = negative,
              left       = left,
              right      = right))

}

# Some illustrations. ---------------------------------------------------------
seed = 250
set.seed(seed)
nobs = 20
xx   = runif(nobs,-1,1)
yy   = (1 + 2*xx + rnorm(nobs)) >= 0

manskiscore = manski(yy,xx)
solutions_pos = manskiscore$positive$solutions[[1]]
solutions_neg = manskiscore$negative$solutions[[1]]
left = manskiscore$left
right = manskiscore$right

# One approach
ss = seq(-2+min(c(-1/xx,1/xx)),2+max(c(-1/xx,1/xx)),by=0.01)
plot(ss,sapply(ss,function(beta) objective(xx,yy,beta))
     ,col="red",type="l",lty=3,xlim=c(-15,20),ylim=c(2,13))
points(left,rep(2,length(left)),pch=2,col="red")
points(right,rep(2,length(right)),pch=3,col="blue")


#lines(ss,sapply(ss,function(beta) objective2(xx,yy,beta)),col="blue",type="l",lty=2)
#abline(h=manskiscore$zero[3],lty=4,col="green")
