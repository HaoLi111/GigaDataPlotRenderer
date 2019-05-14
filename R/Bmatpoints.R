# BigMatpoints

Bmatpoints = function(x,y,
                      xbase=NULL,ybase=NULL,
                      lx=NULL,ux=NULL,dx=NULL,
                      ly=NULL,uy=NULL,dy=NULL,
                      a=.2,
                      type='yCover'){

  nx=length(x);ny = ncol(y)
  #stopifnot(nx==nrow(y))
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
    m=matrix(0,nr,nc)
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
            if(type=='freq'){
              #Render based on frequency
              p=m[ir,ic]
             if(p>1){
               m[ir,ic]=1
             }else{
               m[ir,ic]=p+a
              }
            }else if(type=='yCover'){
              #Coverage based on corresponding y value
              m[ir,ic]= j/ny
            }

          }
        }
      }
    }
    return(m)
}


