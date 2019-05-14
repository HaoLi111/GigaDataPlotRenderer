Base_xy= function(x,y,
                  xbase=NULL,ybase=NULL,
                  lx=NULL,ux=NULL,dx=NULL,
                  ly=NULL,uy=NULL,dy=NULL,
                  type = "default"){
  if(type == "default"){
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
  }else if(type=="xDep"){
    xbase = x
    lx = min(x)
    ux = max(x)
    dx = abs(x[2]-x[1])

    if(is.null(ybase)){
      if(is.null(ly)){
        if(is.null(dy)){
          #automatically generate x
          #xbase = seq(from=xmin,to=xmax,length.out=12800)
          ybase = seq(from=ymin,to=ymax,length.out=7200)
        }else{
          #xbase = seq(from=xmin,to=xmax,by=dx)
          ybase = seq(from=ymin,to=ymax,by=dy)
        }
      }else{
        #create base from diff and lim
        #xbase = seq(from=lx,to=ux,by=dx)
        #xbase=xbase[-length(xbase)]

        ybase = seq(from=ly,to=uy,by=dy)
        ybase=ybase[-length(ybase)]
      }
    }
  }else if(type=="yDep"){
    ybase = y
    ly = min(y)
    uy = max(y)
    dy = abs(y[2]-y[1])

    if(is.null(xbase)){
      if(is.null(lx)){
        if(is.null(dx)){
          #automatically generate x
          xbase = seq(from=xmin,to=xmax,length.out=12800)
          #ybase = seq(from=ymin,to=ymax,length.out=7200)
        }else{
          xbase = seq(from=xmin,to=xmax,by=dx)
          #ybase = seq(from=ymin,to=ymax,by=dy)
        }
      }else{
        #create base from diff and lim
        xbase = seq(from=lx,to=ux,by=dx)
        xbase=xbase[-length(xbase)]

        #ybase = seq(from=ly,to=uy,by=dy)
        #ybase=ybase[-length(ybase)]
      }
    }
  }else if(type=="xyDep"){
    xbase = x
    lx = min(x)
    ux = max(x)
    dx = abs(x[2]-x[1])

    ybase = y
    ly = min(y)
    uy = max(y)
    dy = abs(y[2]-y[1])
  }
  list(xbase = xbase,ybase=ybase,lx=lx,ly=ly,ux=ux,uy=uy,dx=dx,dy=dy)
}
