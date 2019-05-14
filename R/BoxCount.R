BoxCount = function(m,nx=10,ny=10){
  nc =ncol(m);nr = nrow(m)
  sumMnn<-0
  for(i in seq(from=1,to=(nr-nx+1),by=nx)){
    for(j in seq(from=1,to=(nc-ny+1),by=ny)){
      Flow <- m[(i:i+nx-1),(j:(j+ny-1))]
      if(any(Flow>0)) sumMnn <- sumMnn + 1#sumMnn <- sumMnn + Flow
    }
  }
  sumMnn
}
