RungeKuttaTable<-function(Fyt=function(t,y){},y0,a,b,n){
  x<-0
  y<-0
  t<-seq(a,b,by=(b-a)/(n-1))
  y[1]<-y0
  h <- (b-a)/(n-1) #step size
  for(i in 2:n){
    k1<-Fyt(t[i-1], y[i-1])
    k2<-Fyt(t[i-1]+(h/2), y[i-1]+((h*k1)/2))
    k3<-Fyt(t[i-1]+(h/2), y[i-1]+((h*k2)/2))
    k4<-Fyt(t[i-1]+h, y[i-1]+h*k3)
    y[i]<-y[i-1]+(h/6)*(k1+2*k2+2*k3+k4)
  }
  plot(t,y,col="red",pch=20,cex=1.5)
  points(t[1],y[1],col="blue",pch=20,cex=2.5)
  recordPlot()
  
  table<-data.frame(t,y)
  return(table)
}

RungeKuttaSystemTable<-function(Fxyt=function(x,y,t){}, Gxyt=function(x,y,t){},x0,y0,a,b,n){
  x<-0
  y<-0
  t<-seq(a,b,by=(b-a)/(n-1))
  y[1]<-y0
  x[1]<-x0
  h <- (b-a)/(n-1) #step size
  for(i in 2:n){
    k1<-Fxyt(x[i-1], y[i-1],t[i-1])
    l1<-Gxyt(x[i-1], y[i-1],t[i-1])
    
    k2<-Fxyt(x[i-1]+((h*k1)/2), y[i-1]+((h*l1)/2), t[i-1]+(h/2))
    l2<-Gxyt(x[i-1]+((h*k1)/2), y[i-1]+((h*l1)/2), t[i-1]+(h/2))
    
    k3<-Fxyt(x[i-1]+((h*k2)/2), y[i-1]+((h*l2)/2), t[i-1]+(h/2))
    l3<-Gxyt(x[i-1]+((h*k2)/2), y[i-1]+((h*l2)/2), t[i-1]+(h/2))
    
    k4<-Fxyt(x[i-1]+(h*k3), y[i-1]+(h*l3), t[i-1]+h)
    l4<-Gxyt(x[i-1]+(h*k3), y[i-1]+(h*l3), t[i-1]+h)
    
    x[i]<-x[i-1]+(h/6)*(k1+2*k2+2*k3+k4)
    y[i]<-y[i-1]+(h/6)*(l1+2*l2+2*l3+l4)
  }
  plot(x,y,col="red",pch=20,cex=1.5)
  points(x[1],y[1],col="blue",pch=20,cex=2.5)
  recordPlot()
  
  table<-data.frame(t,x,y)
  return(table)
}