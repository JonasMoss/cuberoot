n = 100
x1 = runif(n,0,1)
x2 = runif(n,0,1)
z = exp(1+5*x1-6*x2)/(1+exp(1+5*x1-6*x2))
y = rbinom(n,1,z)

coefs = glm(y~x1+x2,family="binomial")$coef
pred = exp(coefs[1]+coefs[2]*x1-6*x2)/(1+exp(coefs[1]+coefs[2]*x1-6*x2))

plot(sde(pred,10))
sde_lines(quantile(pred,(1:9)/10),lty=2,col="blue")
sde(pred,10)

boxes = function(q,y){
  
}