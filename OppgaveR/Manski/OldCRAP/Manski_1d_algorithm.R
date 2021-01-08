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
  right = sort(c(splitted$'1',splitted$'2'))
  left = sort(c(splitted$'0',splitted$'3'),decreasing=TRUE)
  
  # When beta = 1, the items '0' and '1' are open. Hence the items with index
  # greater than or equal to the following are closed.
  
  plus_closed_index = length(splitted$'0')
  minus_closed_index = length(splitted$'1')
  
  # This function identifies maximal subsets for beta0 = 1 and
  # beta0 = -1 at the same time. 
  
  findSolutions = function(left,right) {
    
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
  
  # We proceed to find the best solutions when beta = 0.
  
  len_left = length(left)
  len_right = length(right)
  if (len_left == len_right) middle = c(lower=-Inf,upper=Inf,k=len_left)
  if (len_left > len_right) middle = c(lower=0,upper=Inf,k=len_left)
  if (len_left < len_right) middle = c(lower=-Inf,upper=0,k=len_right)
  
  return(list(positive   = findSolutions(left,right),
              zero       = middle,
              negative   = findSolutions(rev(-left),rev(-right))))
  
}
