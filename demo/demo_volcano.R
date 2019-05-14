# Required installation from CRAN and sys-modeling toolbox
#please uncomment these if not loaded
#library(rVividImg)
#library(FractalDimBoxCount)
#library(purrr)
#library(ggplot2)
#load data
dim(volcano)
system.time({
  volcanoBee = volcano %>% image_normalize() %>%
    image_sharp_split(.5) %>%
    image_BoundaryExtract() %>%
    image_expand(100,100,0)
})
image(volcanoBee)
##Find factors of 4000

f=NULL;for (i in 1:100) if(100%%i==0) f=c(f,i)
f

Factors = f[-length(f)]
Factors

## Box count
mDimDF = FractalDimDF.matrix(volcanoBee,Factors)
mDimGraphs = ggFractalDimDF(mDimDF)

print(mDimDF)
lapply(mDimGraphs,print)
mDimGraphs[[5]]

#we expect the statistical dim to be about 1.08
