library(caret)
library(class)
library(Zelig)
library(MASS)
library(e1071)
library(randomForest)
library(nnet)
tr.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/ARCENE/arcene_train.data",sep=' ',header=FALSE)
vl.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/ARCENE/arcene_valid.data",sep=' ',header=FALSE)

lbl.tr.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/ARCENE/arcene_train.labels',sep=' ',header=FALSE)
lbl.vl.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/ARCENE/arcene_valid.labels',sep=' ',header=FALSE)

tr.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/DEXTER/dexter_train.data",sep=' ',header=FALSE)
vl.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/DEXTER/dexter_valid.data",sep=' ',header=FALSE)

lbl.tr.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/DEXTER/dexter_train.labels',sep=' ',header=FALSE)
lbl.vl.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/DEXTER/dexter_valid.labels',sep=' ',header=FALSE)

tr.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/MADELON//madelon_train.data",sep=' ',header=FALSE)
vl.dt = read.table(file="C:/Users/A42142/Desktop/CNAM/STA211/prj/MADELON//madelon_valid.data",sep=' ',header=FALSE)

lbl.tr.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/MADELON//madelon_train.labels',sep=' ',header=FALSE)
lbl.vl.dt <- read.delim(file='C:/Users/A42142/Desktop/CNAM/STA211/prj/MADELON//madelon_valid.labels',sep=' ',header=FALSE)



tr.dt <- tr.dt[,colSums(is.na(tr.dt))<nrow(tr.dt)]
vl.dt <- vl.dt[,colSums(is.na(vl.dt))<nrow(vl.dt)]

set.seed(400)
# Modeles sur SVD
  row.tr=paste("TR", seq(1,nrow(tr.dt)),sep="")
  row.names(tr.dt) <- row.tr
  row.vl=paste("VL", seq(1,nrow(vl.dt)),sep="")
  row.names(vl.dt) <- row.vl
  all.data=rbind(tr.dt,vl.dt)
  all.data = all.data[sapply(all.data, function(x) length(levels(factor(x)))>1)]
  red.all=red.dim.mat(as.matrix(all.data),scacen=TRUE,VarLim=0.9)
  
  # base d'apprendisage et validation reduitees
  tr.dt.red=data.frame(subset(red.all, row.names(red.all) %in% row.tr))
  vl.dt.red=data.frame(subset(red.all, row.names(red.all) %in% row.vl))
  
  tr.dt.red$lbl      <- as.factor(t(lbl.tr.dt))
  vl.dt.red$lbl      <- as.factor(t(lbl.vl.dt))
  
  # Modele 1 = Méthode des k plus proches voisins KNN
  
  
  ctrl <- trainControl(method="repeatedcv",repeats = 10) 
  knnFit <- train(lbl ~ ., data = tr.dt.red, method = "knn", trControl = ctrl, tuneLength = 20)
  
  plot(knnFit)
  knnPred.Tr <- predict(knnFit,newdata = tr.dt.red )
    confusionMatrix(knnPred.Tr, tr.dt.red$lbl )

  knnPred.Vl <- predict(knnFit,newdata = vl.dt.red )
   confusionMatrix(knnPred.Vl, vl.dt.red$lbl )
  
  
  # Modele 2 = Regression Logistic 
  
  tr.dt.red$lbl.l <- ifelse(tr.dt.red$lbl ==-1,0,1) 
  vl.dt.red$lbl.l <- ifelse(vl.dt.red$lbl ==-1,0,1) 
  
  
  form=paste('lbl.l ~ ',paste(paste('X',1:(ncol(tr.dt.red)-2),sep=''), collapse='+'), sep='')
  logit <- glm(form, family=binomial(link="logit"), data=tr.dt.red)
  
  LogPred.Tr<-predict(logit, newdata=tr.dt.red, type="response")
  LogPred.Tr <- ifelse(LogPred.Tr <0.5,-1,1) 
  confusionMatrix(vl.dt.red$lbl, LogPred.Tr,positive='1' )
  
  LogPred.Vl<-predict(logit, newdata=vl.dt.red, type="response")
  LogPred.Vl <- ifelse(LogPred.Vl <0.5,-1,1) 
  confusionMatrix(vl.dt.red$lbl, LogPred.Vl,positive='1' )



#Selection Variables par une RF

#Suppression variable à zero variance
tr.dt1 = tr.dt[sapply(tr.dt, function(x) length(levels(factor(x)))>1)]
tr.dt1$lbl = as.factor(t(lbl.tr.dt))

rf <- randomForest(y=tr.dt1$lbl, x=tr.dt1[,-which(names(tr.dt1) == 'lbl')], ntree = 500, importance = TRUE, allowParallel = TRUE) 
varImpPlot(rf)
varimp=importance(rf)
varsel=varimp[varimp[,"MeanDecreaseAccuracy"] > 1.8,]
#Creation des bases d'apprendissage et validation en gardent les variables issuees de la selection par RF
tr.dt2 <- tr.dt[rownames(varsel)]
tr.dt2$lbl=as.factor(t(lbl.tr.dt))
vl.dt2 <- vl.dt[rownames(varsel)]
vl.dt2$lbl=as.factor(t(lbl.vl.dt))
#Modele 3: Reseaux des neurones sur selection des variables par RF
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## 10 repetition
    repeats = 10)
  model.nnet <- train(as.factor(t(lbl.tr.dt))~., data=tr.dt2,method='nnet',
                      trControl = fitControl,trace=F,maxit=200,
                      tuneGrid = expand.grid(.size=c(1,5,20),.decay=c(0,0.1,0.01)))
  model.nnet
  nnet.Tr <- predict(model.nnet,tr.dt2) 
    confusionMatrix(nnet.Tr, as.factor(t(lbl.tr.dt)),positive='1' )
  nnet.Vl <- predict(model.nnet,vl.dt2) 
    confusionMatrix(nnet.Vl, as.factor(t(lbl.vl.dt)),positive='1' )

#Modele 4: LDA sur selection des variables par RF
model.lda <- lda(as.factor(t(lbl.tr.dt)) ~ ., data=tr.dt2)
lda.Tr = predict(model.lda,tr.dt2)$class
  confusionMatrix(lda.Tr, tr.dt2$lbl,positive='1' )
lda.Vl = predict(model.lda,vl.dt2)$class
  confusionMatrix(lda.Tr, vl.dt2$lbl,positive='1' )
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



