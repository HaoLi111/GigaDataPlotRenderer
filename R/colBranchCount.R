#Column Branch-counting

colBranchCount = function(m,limpower=6){

  #m
  nr = nrow(m)
  nc = ncol(m)

  limPower = 6
  lim = 2^limPower
  if(lim>nc) message("Lim>nc, Calculation may be inaccurate")


  nb =1# number of current branching
  #First branching
  ni=NULL#index of branching
  for(i in 1:nc){
    L = length((1:nr)[(m[,i] > 0)])#Number of pixel unequal to 0
    if(L >nb && L < lim){
      if((L/nb)==2){
        nb = L
        ni = c(ni,i)
      } #message("L branching unequal to 2")
      #or do nothing (omit burr point regions at branching)
    }else if(L>=lim){
      nb = 1
    }
  }
  ni
}
#ni = colBoxCount(Mat)
#dx=0.00025
#ni*dx
###load rd 8 data first
#l#ibrary(GigaDataPlotRenderer)
#L = seq(from=7000*.00025,to=7160*.00025,by=0.00001)
#Mat = Bmatpoints(x=L,y=rd6Data,xbase = L,
      #           ybase = seq(from=-1,to=1,by=.01#))
#ni1  = colBoxCount(Mat)
