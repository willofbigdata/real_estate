library(pryr)
n <- 16000
mat <- matrix(1,nrow=n,ncol=n)

object_size(mat)
mem_used()

rm(mat)
mem_used()

vec <- rep(1,10000^2)
object_size(vec)

saved <- matrix(1:3000000)
object_size(saved)