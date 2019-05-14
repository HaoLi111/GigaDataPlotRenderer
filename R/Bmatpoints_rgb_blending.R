# BigMatpoints
#https://blog.csdn.net/aa13058219642/article/details/80953341
#https://en.wikipedia.org/wiki/Alpha_compositing
#https://stackoverflow.com/questions/746899/how-to-calculate-an-rgb-colour-by-specifying-an-alpha-blending-amount

BlendAddRGB = function(p1,p2,a1=.6,a2=NULL){
  if(is.null(a2)) a2=1-a1
  a=1-(1-a1)*(1-a2)
  p=1/a*(a1*p1+(1-a1)*a2*p2)
  c(p,a)
}
#BcolorRF=function(p,ly,uy)

Bmatpoints_rgb_blending = function(x,y,
                      xbase=NULL,ybase=NULL,
                      lx=NULL,ux=NULL,dx=NULL,
                      ly=NULL,uy=NULL,dy=NULL,
                      colRF = (function(y) return(y)),
                      colGF = (function(y) 0),
                      colBF = (function(y) return(1-y)),
                      mixF =BlendAddRGB,
                      a1=.5,
                      colVar='y',
                      aInit=.9){

  nx=length(x);ny = ncol(y)
  stopifnot(nx==nrow(y))
  xmin=min(x);xmax=max(x);ymin=min(y);ymax=max(y)
  if(is.null(xbase)){
    if(is.null(lx)){
      if(is.null(dx)){
        #automatically generate x
        xbase = seq(from=xmin,to=xmax,length.out=12800)
        ybase = seq(from=ymin,to=ymax,length.out=7200)
      }else{
        xbase = seq(from=xmin,to=xmax,by=dx)
        ybase = seq(from=ymin,to=ymax,by=dy)
      }
    }else{
      #create base from diff and lim
      xbase = seq(from=lx,to=ux,by=dx)
      xbase=xbase[-length(xbase)]

      ybase = seq(from=ly,to=uy,by=dy)
      ybase=ybase[-length(ybase)]
    }
  }
  if(is.null(lx)){
    lx=min(xbase)
    ux=max(xbase)
    ly=min(ybase)
    uy=max(ybase)
    dx=xbase[2]-xbase[1]
    dy=ybase[2]-ybase[1]
  }
  nr=length(ybase);nc=length(xbase)
  #black background
  b<-g<-r<-matrix(0,nr,nc)
  #a=matrix(aInit,nr,nc)
  #message(nx);message(ny)
  #message(lx);message(dx)
  for(i in 1:nx){
    #find nc for nx
    ic = ceiling((x[i]-lx)/dx)
    for(j in 1:ny){
      #transform y value to rowwise pixel data
      #do not normalize
      #find the r location corrspd to y value first
      ir = nr-ceiling((y[i,j]-ly)/dy)
      #message(ir);message(ic)
      if(ir<=nr & ir>0){
        if(ic<=nc & ic>0){
          re = mixF(c(colRF(j/ny),colGF(j/ny),colBF(j/ny)),r[ir,ic],a1=a1)
          r[ir,ic]=re[1];g[ir,ic]=re[2];b[ir,ic] = re[3]
          #a[ir,ic]=re[4]
        }
      }
    }
  }
  m=numeric(nr*nc*3)
  dim(m) = c(nr,nc,3)
  m[,,1] = r
  m[,,2] = g
  m[,,3] = b
  return(m)
}


