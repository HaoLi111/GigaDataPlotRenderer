# Required installation from CRAN and sys-modeling toolbox
#please uncomment these if not loaded
#library(rVividImg)
#library(FractalDimBoxCount)
#library(purrr)
#library(ggplot2)
#load data
MandelbrotSet = read.csv("Mandelbrot.csv",header = T)
MandelbrotSet = as.matrix(MandelbrotSet)
dim(MandelbrotSet)

system.time({
  mBee = MandelbrotSet %>% image_sharp_split(.99) %>%
    image_BoundaryExtract() %>%
    image_expand(4000,4000,0)
  writeTIFF(mBee,'mBee.tif')
})

##Find factors of 4000

f=NULL;for (i in 1:4000) if(4000%%i==0) f=c(f,i)
f

Factors = f[-length(f)]
Factors

## Box count
mDimDF = FractalDimDF.matrix(mBee,Factors)
mDimGraphs = ggFractalDimDF(mDimDF)

print(mDimDF)
lapply(mDimGraphs,print)

## The fractal dim exeeds geometrical dim of grid(2),
## therefore D=2
