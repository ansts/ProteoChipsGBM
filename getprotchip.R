#
#
#
getprotchip<-function(fin, p=NULL) {
  require(parallel)
  require(reshape2)
  require(stringr)
  require(e1071)
  
  prt=proc.time()
  finp=paste(p,fin,sep = "")
  fcon=file(finp)
  f2l=readLines(con=fcon, n=2)
  nlns=as.double(str_extract(f2l[2], "[1-9]+"))
  fhead=readLines(con = fcon, n=nlns+2)
  f=read.delim(finp, skip=nlns+2, header=T, check.names = F, stringsAsFactors = FALSE)
  close(fcon)
  f=f[f$`ID`!="000000000000000",]
  dfim=data.frame(ID=f$`ID`,Name=f$`Name`,B=f$`Block`,R=f$`Row`, C=f$`Column`, F=f$`F635 Median`, FCV=f$`F635 CV`, B=f$`B635 Median`, BCV=f$`B635 CV`, fl=f$`Flags` )
  
  v=dfim$F-dfim$B
  #fl=(dfim$fl!=0)|(dfim$FCV>100)
  fl=(dfim$fl!=0|dfim$FCV>100)
  vals=data.frame(dfim[,1:5], V=v, fl=fl)
  
  return(vals)
  
}