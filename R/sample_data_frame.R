#Not S3!
sample_data_frame = function(x,size=100){
  nr=nrow(x)
  stopifnot(nr>100)
  x[as.vector(sample(1:nr,size,replace=F)),,drop=T]
}
