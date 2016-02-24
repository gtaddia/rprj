red.dim.mat <- function(x,scacen=TRUE,VarLim=0.95) {

  if (scacen){
    #scale and center
    sc.x<-scale(as.matrix(x),scale=T,center=T)
  }
  else {
    sc.x<-as.matrix(x)
  }
    nbpc=0
    x.svd<-svd(sc.x)
    som.pc<-sum(x.svd$d^2)
    somm=0
      for(row in x.svd$d) {
        nbpc <- nbpc + 1
        somm <- (row^2+somm)
          print(row)
          print(somm)
          print (round(c(somm/som.pc),3))
         if (somm/som.pc >= VarLim) {
            break
          }
      }
  
cumulativeVarianceExplained = cumsum(x.svd$d^2/sum(x.svd$d^2)) * 100
plot(cumulativeVarianceExplained,ylim=c(0,100),cex.lab=2, xlab="SVD column",ylab="Cumulative percent of variance explained",pch=19)
x.red<-sc.x %*% x.svd$v[,1:nbpc]

}
