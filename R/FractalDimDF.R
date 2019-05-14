FractalDimDF=function(m,...) UseMethod("FractalDimDF")

FractalDimDF.matrix = function(m,Factors){
  Factors
  n =NULL;S=NULL
  for(i in Factors){
    #print(i)
    S = c(S,i/100)
    n = c(n,BoxCount(m,i,i))
  }
  data.frame(n=n,S=S,
             n_1=1/n,
             S_1 = 1/S,
             logS = log10(S),
             logS_1 = log10(1/S),
             logn = log10(n),
             logn_1=log10(1/n),
             d =log10(1/n)/log10(S))
}
