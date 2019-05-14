dotLocate = function(x,xbase = NULL,lx=NULL,ux=NULL,dx=NULL){
  if(is.null(lx)){
    lx = min(xbase)
    ux = max(xbase)
    dx = abs(xbase[2]-xbase[1])
    nx =length(xbase)
  }
  ni = 1
  #i=1
  i=floor((x-lx)/dx)+1
  #while(i <= nx){
   # if(x<=lx+dx*(i-1)) break
  #  i=i+1
  #}
  if(i>nx) return(NULL)
  i
}

#dotLocate(3.7,(1:500)/100)
lineGrid = function(m,xbase,ybase,xgrid=NULL,ygrid=NULL,
                    GridF = (function(x) 1-x)){
  if(is.null(xgrid)) xgrid = seq(from = min(xbase),to=max(xbase),length.out = 5)
  if(is.null(ygrid)) ygrid = seq(from = min(ybase),to=max(ybase),length.out = 5)




  #x,y pixel indeces
  #xi=numeric(length(xgrid))
  #yi=numeric(length(ygrid))
  #for(i in seq_along(xgrid)) xi[i] = dotLocate(xgrid[i],xbase)
  #for(i in seq_along(ygrid)) yi[i] = dotLocate(ygrid[i],ybase)
  xi = sapply(xgrid,dotLocate,xbase=xbase)
  yi = sapply(ygrid,dotLocate,xbase=xbase)
  xi=xi[xi<=length(xbase)]
  yi =yi[yi<=length(ybase)]
  re=m
  nr = nrow(re);nc= ncol(re)
  if(length(dim(m))==2){
    #bw plot
    re[nr-yi+1,] = GridF(m[nr-yi+1,])
    re[,xi] = GridF(m[,xi])
  }else if(length(dim(m))==3){
    #rgb plot
    if(length(unlist(args(GridF)))==1){
      for(i in 1:3){
        re[nr-yi+1,,i] = GridF(m[nr-yi+1,,i])
        re[,xi,i] = GridF(m[,xi,i])
      }

    }

  }
  return(re)
}
