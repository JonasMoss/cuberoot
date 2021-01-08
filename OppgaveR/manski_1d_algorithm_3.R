manski_new = function(yy,xx,ww=rep(1,nobs)){
  # We start by massaging xx and yy into two separate lists,
  # combining the direction and values and of possible solutions.
  #
  # right: Vector containing possible solutions of the form (-inf,x>
  # left: Vector containing possible solutions of the form <x,inf)
  #
  # Both are sorted for convenience sake.
  
  nobs = length(xx)
  split_candidates = split(-1/xx, (yy & xx>=0) | (!yy & xx < 0))
  split_weights    = split(ww, (yy & xx>=0) | (!yy & xx < 0))
  split_open       = split(yy, (yy & xx>=0) | (!yy & xx < 0))
  
  
  if (is.null(split_candidates$'FALSE')) {
    right         = NULL
    right_weights = NULL
    right_open    = NULL
  } else {
    right_order   = order(split_candidates$'FALSE',decreasing=TRUE)
    right         = split_candidates$'FALSE'[right_order]
    right_weights = split_weights$'FALSE'[right_order]
    right_open    = split_open$'FALSE'[right_order]
  }
  
  if (is.null(split_candidates$'TRUE')) {
    left          = NULL
    left_weights  = NULL
    left_open     = NULL
  } else {
    left_order    = order(split_candidates$'TRUE',decreasing=TRUE)
    left          = split_candidates$'TRUE'[left_order]
    left_weights  = split_weights$'TRUE'[left_order]
    left_open     = split_open$'TRUE'[left_order]
  }
  
  # This function identifies maximal subsets for beta0 = 1 and
  # beta0 = -1 at the same time. 
  
  findSolutions = function(left, right) {
    
#     if(is.null(left)) {
#       return(list(ks=NULL,
#                   k_max=length(right),
#                   solutions=c(lower=-Inf,upper=min(right))))
#     } else if (is.null(right)) {
#       return(list(ks=NULL,
#                   k_max=length(left),
#                   solutions=c(lower=max(left),upper=Inf)))
#     } else {
      
      len_left   = length(left)
      len_right  = length(right)
      weight     = sum(left_weights) + sum(right_weights[right>=left[1]])
      max_weight = weight
      weights    = c(weight)
      index      = 1
      
    # The condition in this loop works like follows: For it to be possible for
    # the next left value to be weight-maximal, there has to be sufficient
    # weight of right elements to the left of it, from which it can gain
    # weight in the next iteration even when we substract the current weight.
    # 
    # The condition on the index guarantees termination.
    
    while( ((weight-left_weights[index]) + sum(right_weights[right < left[index]])) > max_weight & index < len_left){
        index          = index + 1   
        weight         = (weight-left_weights[index]) + sum(right_weights[right > left[index] & right < left[index-1]])
        max_weight     = max(weight, max_weight)
        weights[index] = weight
    }
      
      
      solution_set = list()
      solutions = left[c(weights == max_weight,rep(FALSE,len_left))]
      
      for (i in 1:length(solutions)) {
        current = solutions[i]
        solution_set[[i]] = c(lower = current, 
                              upper = min(c(right[right >= current],Inf)))
      }  
      
      return(list(weights=weights,
                  max_weight=max_weight,
                  solutions=solution_set))
    }
  

  
  # We proceed to find the best solutions when beta = 0.
  
#   len_left = length(left)
#   len_right = length(right)
#   if (len_left == len_right) middle = c(lower=-Inf,upper=Inf,k=len_left)
#   if (len_left > len_right) middle = c(lower=0,upper=Inf,k=len_left)
#   if (len_left < len_right) middle = c(lower=-Inf,upper=0,k=len_right)
  
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

manski_new(yy,xx)
