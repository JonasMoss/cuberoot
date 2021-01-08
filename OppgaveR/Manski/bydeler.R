bydeler = read.csv("bydeler.csv",sep=";",header=TRUE)
bydeler$økning = bydeler[,7]/bydeler[,1]
bydeler$gror1 = bydeler["Grorud",1]/bydeler[,1]
bydeler$gror2 = bydeler["Grorud",7]/bydeler[,7]

plot(c(rep(0,14),rep(1,14)),c(bydeler$gror1[-c(10,16)],bydeler$gror2[-c(10,16)]),
     bty="l",xlab=NA,ylab="Groruds inntekt som andel av bydels inntekt",xaxt = "n")

axis(1, at=0:1, labels=c("År 2007","År 2013"))

for ( i in c(1:9,11:15)){
 lines(0:1,c(bydeler$gror1[i],bydeler$gror2[i]))
}

mean(bydeler$gror2[-c(10,16)])

mean(bydeler$gror1[-c(10,16)])
