t<-matrix(seq(0,2*pi,0.1*pi))
x<-sin(t)
y<-(matrix(4*x+2,ncol=1))

trainadaline<-function(xin,yd,eta,tol,maxepocas,par){
  dimxin<-dim(xin)
  N<-dimxin[1]
  n<-dimxin[2]
  
  if(par==1){
    wt<-as.matrix(runif(n+1)-0.5)
    xin<-cbind(1,xin)
  }else{
    wt<-as.matrix(runif(n)-0.5)
  }
  
  nepocas<-0
  eepoca<-tol+1
  
  evec<-matrix(nrow=1,ncol = maxepocas)
  
  while ((nepocas<maxepocas)&&(eepoca>tol)) {
    ei2<-0
    xseq<-sample(N)
    for (i in 1:N) {
      irand<-xseq[i]
      xvec<-as.matrix(xin[irand,])
      yhati<-1.0*((t(xvec)%*%wt))
      ei<-yd[irand,]-yhati
      dw<-c(eta)*c(ei)*c(xin[irand,])
      wt<-wt+dw
      ei2<-ei2+ei*ei
    }
    nepocas<-nepocas+1
    evec[nepocas]<-ei2/n
    eepoca<-evec[nepocas]
  }
  retlist<-list(wt,evec[1:nepocas])
  
  return((retlist))
}

#retlist<-trainadaline(x,y,0.01,0.01,50,1)
w<-retlist[[1]]
erro<-retlist[[2]]
