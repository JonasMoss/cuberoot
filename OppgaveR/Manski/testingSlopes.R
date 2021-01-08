xs = seq(-5,5,by=0.01)
sol = (x1j-x1i)/(x2j*x1i-x2i*x1j)

line = function(a,b,x){
  a*x + b
}

x11 = 5
x12 = 9
x21 = -2
x22 = 8
xcoord = (x21-x11)/(x22*x11-x12*x21)
ycoord = -1/x11 - xcoord*x12/x11

par(pty="s")
plot(xs,-1/x11-xs*x12/x11,type="l",xlim=c(-3,3),ylim=c(-3,3))
lines(xs,-1/x21-xs*x22/x21,type="l")
points(xcoord,ycoord)

sign(-x12/x11)*sign(-x22/x21) + abs(x12/x11)*abs(x22/x21)

intersection(x11,x12,x21,x22,1,1)