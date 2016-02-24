require(multigroup)

medi.data <- read.csv("/donnees/R/sta201/multi-in/medi_data.csv",sep=';',dec=',')
medi_meta <- read.csv("/donnees/R/sta201/multi-in/medi_meta.csv")
#suppression variables manquants
varna=c("EN.BIR.THRD.NO","EN.FSH.THRD.NO","AG.LND.EL5M.ZS","EN.MAM.THRD.NO","EN.HPT.THRD.NO","EN.POP.EL5M.ZS")
medi.data = medi.data[!(medi.data$Country.Code %in% c('MCO','MLT')), -which(names(medi.data) %in% varna)]
medi.data$Country.Name <- factor(medi.data$Country.Name)
medi.data$Country.Code <- factor(medi.data$Country.Code)

medi.data = medi.data[, -grep("NY.*.ZS", colnames( medi.data))]
medi.data = medi.data[with(medi.data, order(Country.Code)), ] 
medi.num=medi.data[ ,-(1:3)]
medi.lbl=data.frame(Country.Nm=medi.data[ ,1],Country=as.factor(medi.data[ ,2]),Year=as.factor(substr(medi.data[ ,3],1,4)))
S
cM <- colMedians(medi.num, na.rm=TRUE)

#cM <- colMeans(medi.num, na.rm=TRUE)

indx <- which(is.na(medi.num), arr.ind=TRUE)
medi.num[indx] <- cM[indx[,2]]
medi.num=medi.num[,order(names(medi.num))]
medi.all=data.frame(medi.data[ ,(1:3)],medi.num) 
rownames(medi.num)=paste0(medi.lbl$Country,medi.lbl$Year)

#centrer et reduites
medi.num = scale(medi.num, center=TRUE, scale=TRUE)
colnames(medi.num)
nBlock <- c(3, 5, 42, 5,10)
Group=medi.lbl$Country
BlockNames <- c("Olfaction at rest", "Vision", "Olfaction after shaking", "Taste",'tte')
res = mbmgPCA(Data = medi.num, Group, nBlock ,ncomp=5)
res = mbmgPCA(Data = medi.num, Group, nBlock ,ncomp=6)

res$K.Data 
res$global.scores
res$block.scores 
res$block.group.scores
res$global.expvar
res$concat.Data 
res$concat.block.Data

res$block.common.loading
res$similarity

library(ggplot2)

# create data frame with scores
scores = as.data.frame(res$global.scores)

# plot of observations
ggplot(data = scores, aes(x = Dim1, y = Dim2, label = rownames(res$global.scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("Commoms Score Medi - Crime Rates")


