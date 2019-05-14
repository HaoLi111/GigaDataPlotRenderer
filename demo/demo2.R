g=expandMat2Df(x=iris[,1],y = iris[,2:4],xname=colnames(iris)[1],
               yname='Measurement.Type',zname="Measurement")
#g$Measurements=as.factor(g$Measurements)
library(ggplot2)
head(g)
ggplot(data=g,aes(x=Sepal.Length,y=Measurement,color=Measurement.Type)) +
  geom_point(alpha=.4) + geom_rug(alpha=.3)


g=expandMat2Df(x=iris[,1],y = iris[,2:4],useyNames = F,xname=colnames(iris)[1],
               yname='Measurement.Type',zname="Measurement")

library(plot3Drgl)
points2Drgl(x=g$Sepal.Length,y=g$Measurement,type='p',colvar = g$Measurement.Type)




