BmatZoomIndex = function(xbase,ybase,xlim,ylim){
  #lx=length(xbase)
  #ly = length(ybase)
  dx=xbase[2]-xbase[1]
  dy=ybase[2]-ybase[1]
  xStart = (xlim[1]-xbase[1])/dx+1
  xEnd = (xlim[2]-xbase[1])/dx+1
  yStart = (ylim[1]-ybase[1])/dx+1
  yEnd = (ylim[2]-ybase[1])/dx+1
  list(xIndex = xStart:xEnd,
       yIndex = yStart:yEnd)
}
#BmatZoomIndex(1:20/2,1:30/3,c(3,8),c(12,24))

BmatSubset = function(m,xbase,ybase,xlim,ylim){
  Index = BmatZoomIndex(xbase,ybase,xlim,ylim)
  m[as.vector(xIndex),as.vector(yIndex)]
}

