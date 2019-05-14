scatterBoxCount = function(x,y,xgrid=NULL,ygrid=NULL){
  stopifnot(length(x)==length(y))
  if(is.null(xgrid)) xgrid = seq(from =min(x),to = max(x),length.out = 100)
  if(is.null(ygrid)) ygrid = seq(from =min(y),to = max(y),length.out = 100)
  n=0
  for(i in 2:length(xgrid)){
    for(j in 2:length(ygrid)){
      for (k in 1:length(x)){
        if((x[k]>xgrid[i-1]) && (x[k]<xgrid[i]) && (y[k]>ygrid[j-1]) && (y[k]<ygrid[j])){
          n=n+1
          print(i);print(j)
          break
        }
      }
    }
  }
  return(n)
}

