pkgInstall <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgInstall("caret")
pkgInstall("class")
pkgInstall("MASS")
pkgInstall("e1071")
pkgInstall("randomForest")
pkgInstall("nnet")
pkgInstall("neuralnet")
pkgInstall("ROCR")
library(caret)
library(class)
library(MASS)
library(e1071)
library(randomForest)
library(nnet)
library(neuralnet)
library(ROCR)



set.seed(400)


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
  plot(cumulativeVarianceExplained,ylim=c(0,100),cex.lab=2, xlab="SVD column",ylab="Pourcentage de Variance cumulé",pch=19)
  #reduction de la dimensionalité  de p vars to k k<p, 
  #en alternative x.red<-x.svd$u[,1:nbpc] %*% diag(x.svd$d[1:nbpc], nbpc) 

  x.red<-sc.x %*% x.svd$v[,1:nbpc]
  
}


setwd("C:/Users/A42142/Desktop/CNAM/STA211/prj")


tr.dt = read.table(file="ARCENE/arcene_train.data",sep=' ',header=FALSE)
vl.dt = read.table(file="ARCENE/arcene_valid.data",sep=' ',header=FALSE)

lbl.tr.dt <- read.delim(file='ARCENE/arcene_train.labels',sep=' ',header=FALSE)
lbl.vl.dt <- read.delim(file='ARCENE/arcene_valid.labels',sep=' ',header=FALSE)


#tr.dt = read.table(file="MADELON//madelon_train.data",sep=' ',header=FALSE)
#vl.dt = read.table(file="MADELON//madelon_valid.data",sep=' ',header=FALSE)

#lbl.tr.dt <- read.delim(file='MADELON//madelon_train.labels',sep=' ',header=FALSE)
#lbl.vl.dt <- read.delim(file='MADELON//madelon_valid.labels',sep=' ',header=FALSE)



tr.dt <- tr.dt[,colSums(is.na(tr.dt))<nrow(tr.dt)]
vl.dt <- vl.dt[,colSums(is.na(vl.dt))<nrow(vl.dt)]

# Modeles sur SVD
  row.tr=paste("TR", seq(1,nrow(tr.dt)),sep="")
  row.names(tr.dt) <- row.tr
  row.vl=paste("VL", seq(1,nrow(vl.dt)),sep="")
  row.names(vl.dt) <- row.vl
  all.data=rbind(tr.dt,vl.dt)
  all.data = all.data[sapply(all.data, function(x) length(levels(factor(x)))>1)]
  red.all=red.dim.mat(as.matrix(all.data),scacen=TRUE,VarLim=0.8)
  
  # base d'apprendisage et validation reduitees
  tr.dt.red=data.frame(subset(red.all, row.names(red.all) %in% row.tr))
  vl.dt.red=data.frame(subset(red.all, row.names(red.all) %in% row.vl))
  
  tr.dt.red$lbl      <- as.numeric(t(lbl.tr.dt))
  vl.dt.red$lbl      <- as.numeric(t(lbl.vl.dt))
  
  # Modele 1 = Méthode des k plus proches voisins KNN
  
  
  ctrl <- trainControl(method="repeatedcv",repeats = 10) 
  knnFit <- train(lbl ~ ., data = tr.dt.red, method = "knn",  preProcess = c("center","scale"),trControl = ctrl, tuneLength = 20)
  
  plot(knnFit)

  knnPred.Tr <- predict(knnFit,newdata = tr.dt.red)
  knnPred.Tr <- ifelse(knnPred.Tr <0,-1,1) 
  confusionMatrix(knnPred.Tr, tr.dt.red$lbl )

  knnPred.Vl <- predict(knnFit,newdata = vl.dt.red )
  knnPred.Vl <- ifelse(knnPred.Vl <0,-1,1) 
  confusionMatrix(knnPred.Vl, vl.dt.red$lbl )


  # Modele 2 = Regression Logistic 
  
  tr.dt.red$lbl.l <- ifelse(tr.dt.red$lbl ==-1,0,1) 
  vl.dt.red$lbl.l <- ifelse(vl.dt.red$lbl ==-1,0,1) 
  
  
  form=paste('lbl.l ~ ',paste(paste('X',1:(ncol(tr.dt.red)-2),sep=''), collapse='+'), sep='')
  logit <- glm(form, family=binomial(link="logit"), data=tr.dt.red)
  
  LogPred.Tr<-predict(logit, newdata=tr.dt.red, type="response")
  LogPred.Tr <- ifelse(LogPred.Tr <0.5,-1,1)
  confusionMatrix(tr.dt.red$lbl, LogPred.Tr,positive='1' )
  
  LogPred.Vl<-predict(logit, newdata=vl.dt.red, type="response")
  LogPred.Vl <- ifelse(LogPred.Vl <0.5,-1,1) 
  confusionMatrix(vl.dt.red$lbl, LogPred.Vl,positive='1' )



#Selection Variables par une RF

#Suppression variable à zero variance
tr.dt1 = tr.dt[sapply(tr.dt, function(x) length(levels(factor(x)))>1)]
#tr.dt1$lbl = as.factor(t(lbl.tr.dt))

rf <- randomForest(y=tr.dt1$lbl, x=tr.dt1, ntree = 500, importance = TRUE, allowParallel = TRUE) 
#rf <- randomForest(y=tr.dt1$lbl, x=tr.dt1[,-which(names(tr.dt1) == 'lbl')], ntree = 500, importance = TRUE, allowParallel = TRUE) 

varImpPlot(rf)
varimp=as.data.frame(importance(rf))
varsel=varimp[varimp[,"MeanDecreaseAccuracy"] >= quantile(varimp$MeanDecreaseAccuracy,0.98),]
#Creation des bases d'apprendissage et validation en gardent les variables issuees de la selection par RF
tr.dt2 <- tr.dt[rownames(varsel)]
#tr.dt2$lbl=as.factor(t(lbl.tr.dt))
vl.dt2 <- vl.dt[rownames(varsel)]
#vl.dt2$lbl=as.factor(t(lbl.vl.dt))

#Modele 3: Reseaux des neurones sur selection des variables par RF
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## 10 repetition
    repeats = 10)

tr.dt.s<-scale(as.matrix(tr.dt2),scale=T,center=T)
  model.nnet <- train(as.factor(t(lbl.tr.dt))~., data=tr.dt.s,method='nnet',
                      trControl = fitControl,trace=F,maxit=200,
                      tuneGrid = expand.grid(.size=c(1,5,20),.decay=c(0,0.1,0.01)))
  model.nnet
  nnet.Tr <- predict(model.nnet,tr.dt.s) 
    confusionMatrix(nnet.Tr, as.factor(t(lbl.tr.dt)),positive='1' )
  nnet.Vl <- predict(model.nnet,vl.dt2) 
    confusionMatrix(nnet.Vl, as.factor(t(lbl.vl.dt)),positive='1' )



#Modele 4: LDA sur selection des variables par RF
model.lda <- lda(as.factor(t(lbl.tr.dt)) ~ ., data=tr.dt2)
lda.Tr = predict(model.lda,tr.dt2)$class
  confusionMatrix(lda.Tr, as.factor(t(lbl.tr.dt)),positive='1' )
lda.Vl = predict(model.lda,vl.dt2)$class
  confusionMatrix(lda.Vl, as.factor(t(lbl.vl.dt)),positive='1' )
plot(model.lda)

# Modele 5 = Random forest sur la base complete

  bestmtry <- as.data.frame(tuneRF(tr.dt,as.factor(t(lbl.tr.dt)), ntreeTry=200, 
                                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE))
  mtry<- bestmtry[bestmtry$OOBError == min(bestmtry[,2]),]$mtry
  
  rf <- randomForest(y=as.factor(t(lbl.tr.dt)), x=tr.dt,mtry =mtry, ntree = 1000,  allowParallel = TRUE) 
  rf.Tr = predict(rf,newdata=tr.dt)
    confusionMatrix(rf.Tr, as.factor(t(lbl.tr.dt)),positive='1' )
  rf.Vl = predict(rf,newdata=vl.dt)
    confusionMatrix(rf.Vl, as.factor(t(lbl.vl.dt)) ,positive='1')





#Definition de la base de validation d'aggregation des modeles

stk_valid =data.frame(trg=as.numeric(t(lbl.vl.dt)),M1=LogPred.Vl,M2=knnPred.Vl,
                      M3=as.numeric(levels(nnet.Vl))[nnet.Vl],M4=as.numeric(levels(lda.Vl))[lda.Vl],
                      M5=as.numeric(levels(rf.Vl))[rf.Vl]
                      
)



library(neuralnet)
var <- names(stk_valid)
hh=round(ncol(stk_valid)/2)
sc.x<-scale(as.matrix(stk_valid[,2:6]),scale=T,center=T)
new=cbind(trg=stk_valid[,1],sc.x)

form.meta <- as.formula(paste('trg ~', paste(var[!var %in% 'trg'], collapse = ' + ')))
stacknet <- neuralnet(form.meta, new, hidden = hh, threshold=0.01)
dev.off()
plot(stacknet)

stack.results <- compute(stacknet, new[,2:6])
stack.results
stack.res= ifelse(stack.results$net.result <0,-1,1) 
confusionMatrix(stack.res, as.factor(t(lbl.vl.dt)),positive='1' )
detach(package:neuralnet,unload=T)


#MADELON
a=249
b=39
c=51
d=261
BER = 0.5*(b/(a+b) + c/(c+d))
print(BER)
#ARCENE
a=41
b=3
c=15
d=41
BER = 0.5*(b/(a+b) + c/(c+d))
print(BER)
library(ROCR)
detach(package:neuralnet,unload=T)
pred <- prediction( stack.res, as.numeric(t(lbl.vl.dt)))
auc.stk <- performance(pred,"auc"); 
auc.stk@y.values[[1]]

perf <- performance(pred,"tpr","fpr")
plot(perf)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)






