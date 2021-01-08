

partition(xx,isReds,add=0)


quickcol = function(points,isReds,add=0){
  n        = length(xx)
  pivot    = sample(1,n)
  upperInd = xx > xx[pivot]
  lowerInd = xx < xx[pivot]
  pivotVal = sum(isReds[lowerInd]) + sum((1-isReds)[upperInd]) + 1
  lbUpper  = sum(isReds)
  ubUpper  = sum(isReds) + sum((1-isReds)[upperInd])
  lbLower  = sum((1-isReds))
  ubLower  = sum((1-isReds)) + sum((isReds)[lowerInd])  
  
}