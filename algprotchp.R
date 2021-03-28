require(parallel)
require(limma)
require(stringi)

path="ChipG\\"
lf=list.files(path = path)
vG=getprotchip(lf, p=path)
path="ChipI\\"
lf=list.files(path = path)
vM=getprotchip(lf, p=path)
nmvG=unique(vG$Name[!vG$Name %in% c("ND","Control")])
nmvM=unique(vM$Name[!vM$Name %in% c("ND","Control")])

nms=intersect(nmvG,nmvM)
vgv=vG[vG$Name %in% nms,c(2,6)]
vgv$V=log2(vgv$V-min(vgv$V)+1)
vgva=aggregate(V~Name, data=vgv, FUN =function(x)if (length(x)>1) if (sd(x)/(mean(x)+0.1)>0.5) min(x) else mean(x) else x)
rownames(vgva)=vgva$Name
vmv=vM[vM$Name %in% nms,c(2,6)]
vmv$V=log2(vmv$V-min(vmv$V)+1)
vmva=aggregate(V~Name, data=vmv, FUN =function(x)if (length(x)>1) if (sd(x)/(mean(x)+0.1)>0.5) min(x) else mean(x) else x)
rownames(vmva)=vmva$Name
vals=data.frame(GBM=vgva[nms,2],IVIgM=vmva[nms,2])
rownames(vals)=nms
plot(vals)
van=normalizeCyclicLoess(vals, method="affy", iterations = 5)
vanr=data.frame(Means=rowMeans(van),Ratio=van[,1]/van[,2])
rownames(vanr)=rownames(van)
hist(log2(vanr$Ratio), breaks = 100)
range(vanr$Ratio)
zsc=scale(log(vanr$Ratio+1.5))
pzsc=p.adjust(pnorm(zsc))
vancalls=vanr[pzsc<0.05,]
