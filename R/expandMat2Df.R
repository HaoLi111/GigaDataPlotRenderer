expandMat2Df=function(x,y,ybase=NULL,useyNames=T,
                      xname=NULL,yname=NULL,zname=NULL){
  nx=length(x)
  ny=ncol(y)
  stopifnot(nx==nrow(y))

  if(is.null(ybase)){
    if(useyNames==T){
      ybase=  colnames(y)
    }else{
      ybase = 1:ny
    }
  }
  y=as.matrix(y);dim(y)=nx*ny
  g = expand.grid(x,ybase)
  g = cbind(g,y)
  if(is.null(xname)) xname='x'
  if(is.null(yname)) yname=deparse(substitute(ybase))
  if(is.null(zname)) zname='zValue'
  colnames(g) = c(xname,yname,zname)
  return(g)
}
