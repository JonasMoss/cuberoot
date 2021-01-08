k <- 4
M <- (k-1)*3-1
q0 <- c(0,0,0)
f <- function(q0,index) { c(head(q0,index-1),q0[index]+1,tail(q0,k-index-1))}
  

for (index in ((0:M)%%(k-1)+1)){
  print(index)
  q0 <- f(q0,index)
  print(q0)
}
