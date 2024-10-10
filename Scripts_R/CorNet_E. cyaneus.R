#
# correlation networks 
#   a R-script added to the paper
#    
#   Impacts of seawater acidification on mantle gene expression patterns 
#   of the Baltic Sea blue mussel: implications for shell formation and energy budget
#   2011
#   Anne K. H?ning, Frank Melzner, J?rn Thomsen, Magdalena A. Gutowska, 
#	Lars Kraemer, Stephan Frickenhaus, Philip Rosenstiel, Hans-Otto Pˆrtner, 
#	Eva Philipp, and Magnus Lucassen 
#   Marine Biology, special issue: ocean acidification
#   doi: http://dx.doi.org/10.1007/s00227-012-1930-9

### see www.awi.de/en/go/bioinformatics
#
# you should install the following R-package with
# install.packages("igraph")
#
###


require(igraph)

corr_net = function (file="MR_allkor.txt",cor.file=FALSE,col.excl=1:2,
  main="correlation network",cor.out=paste("corr",file,sep="_"),cut=0.5,seed=12345,method="pearson",
  na.cor="complete.obs")
{
if (cor.file) 
 { cor_mat=read.table(file,header=T,sep="\t",dec=",",comment="") ; method="unk"} 
else {
 mat=read.table(file,header=T,na.strings=c("#WERT!","nv"),sep="\t",dec=",",comment="")
 cor_mat<- cor(mat[,setdiff(1:dim(mat)[2],col.excl)],method=method,use=na.cor)
} 
 cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0
 cor_mat[ abs(cor_mat) <= cut]<- 0
write.table(cor_mat,file=cor.out,sep="\t",dec=",",quote=F)
sign=sign(cor_mat[abs(cor_mat) > cut])
graph <- graph.adjacency(abs(cor_mat)>cut, weighted=TRUE, mode="upper",add.colnames="label")
set.seed(seed)
E(graph)$weight <- (t(cor_mat)[abs(t(cor_mat))>cut])
E(graph)$color="black"
E(graph)[weight<0]$color="red"
abs(E(graph)$weight) -> E(graph)$weight
graph$layout <- layout.fruchterman.reingold
x11()
graph.decomp=decompose.graph(graph)[[which.max(sapply(decompose.graph(graph), vcount))]]
V(graph.decomp)$color="white"
V(graph.decomp)$font.lab=2

op=par(no.readonly = TRUE)
par(cex=0.65)
ct=cut(E(graph.decomp)$weight,c(round(cut,2),round(cut+(1-cut)/4,2),round(cut+(1-cut)*2/4,2),round(cut+(1-cut)*3/4,2),1))
plot(graph.decomp,  edge.width =ct,frame=T) 
print("Choose topleft corner of legend\n")
locator(1)->p
par(cex=1)
legend(x=p$x,y=p$y,lwd=1:4,legend=levels(ct) ) 
title(paste(main,method))
par(op)
}


# read full data sets

# compare following with Fig 3a in MS
corr_net("Verrucosus.txt",main="IM",cut=0.50,seed=1324123,method="pearson",na.cor="complete.obs")
corr_net("Verrucosus.txt",main="IM",cut=0.50,seed=1324123,method="spearman",na.cor="complete.obs")

# compare following with Fig 3b in MS
corr_net("Verrucosus.txt",main="MR",cut=0.50,seed=1324123,method="pearson",na.cor="complete.obs")
corr_net("Verrucosus.txt",main="MR",cut=0.50,seed=1324123,method="spearman",na.cor="complete.obs")



