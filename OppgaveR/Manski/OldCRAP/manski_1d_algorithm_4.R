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
    
    len_left    = length(left)
    len_right   = length(right)
    weight      = sum(left_weights) + sum(right_weights[right>=left[1]])
    max_weight  = weight
    weights     = c(weight)
    index       = 1
    weight_left = sum()
    
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
    
    if (sum(right_weights[right < left[len_left]])>max_weight){
      solution_set = c(lower = -Inf, 
                       upper = right[len_right])
      max_weight   = sum(right_weights[right < left[len_left]])
    } else {
    
 
      solution_set = list()
      solutions = left[c(weights == max_weight,rep(FALSE,len_left))]
    
      for (i in 1:length(solutions)) {
        current = solutions[i]
        solution_set[[i]] = c(lower = current, 
                            upper = min(c(right[right >= current],Inf)))
      }  
    }
    
    return(list(weights=weights,
                max_weight=max_weight,
                solutions=solution_set))
  }
  
  
  # We find the solutions by running trough both left and rev(-left)
  # and selecting the best solutions. The following conditions are
  # required as rev, - etc. don't handle NULL vectors.
  
  if (is.null(left)) {
    negative = findSolutions(NULL,rev(-right))
  } else if (is.null(right)) {
    negative = findSolutions(rev(-left),NULL)
  } else {
    negative = findSolutions(rev(-left),rev(-right))
  }
  
  positive = findSolutions(left,right)
  
  solutions        = list()
  class(solutions) = "manski"
  if (positive$max_weight == negative$max_weight) {
    solutions$POSITIVE = positive$solutions
    solutions$NEGATIVE = negative$solutions
    solutions$weight   = positive$max_weight
    return(solutions)
  } else if (positive$max_weight > negative$max_weight){
    solutions$POSITIVE = positive$solutions
    solutions$weight   = positive$max_weight
    return(solutions)
  } else {
    solutions$NEGATIVE = negative$solutions
    solutions$weight   = negative$max_weight
    return(solutions)    
  }
  
}
# 
# print.manski = function(x){
#   if (is.null(x$POSITIVE)) {
#     print ("All solutions have negative intercept.")
#   }
# }

seed = 90
set.seed(seed)
nobs = 20
xx   = runif(nobs,-1,1)
yy   = (xx + rnorm(nobs)) >= 0


solutions_pos = manski_new(yy,xx)
reds = -1/xx[((yy & xx > 0) | (!yy & xx < 0))]
blues = -1/xx[!((yy & xx > 0) | (!yy & xx < 0))]
plot(reds,rep(0,length(reds)),xlim=c(min(-1/xx),max(-1/xx)),
     col=cols[1],pch=16,ylim=c(-0.2,0.2),xlab=NA,ylab=NA,yaxt='n',bty="n",axes=0)
points(blues,rep(0,length(blues)),col=cols[2],bg=cols[2],pch=16)
lines(solutions_pos,c(0,0),col=pastel[1])

reds = 1/xx[((yy & xx > 0) | (!yy & xx < 0))]
blues = 1/xx[!((yy & xx > 0) | (!yy & xx < 0))]
plot(reds,rep(0,length(reds)),xlim=c(min(1/xx),max(1/xx)),
     col=cols[1],pch=16,ylim=c(-0.2,0.2),xlab=NA,ylab=NA,yaxt='n',bty="n",axes=0)
points(blues,rep(0,length(blues)),col=cols[2],bg=cols[2],pch=16)
lines(solutions_pos,c(0,0),col=pastel[1])

manski_new(yy,xx)
