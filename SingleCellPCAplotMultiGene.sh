#!/bin/bash

first=$1 #pass the first argument to a variable so that it can be used in grep with double quotes, in order to use spaced argument 
second=$2
third=$3

echo "#!/usr/bin/Rscript
library(reshape2)

x<-read.delim(\"$first\", header=T, row.names=1)

x1<-x
#x1<-x[c(-1,-2),c(-186,-187,-188)] #if formating is needed

numc <- sapply(x1, is.factor)
x1[numc]<- lapply(x1[numc], function(x) as.numeric(as.character(x)))

pca= prcomp( x1 , center=T, scale=T)

#plot simple pca 1/2

pdf(\"pca_nocol.1.2.pdf\")
plot(pca\$rotation[,1],pca\$rotation[,2], xlab = \"PC1\", ylab = \"PC2\", main=\"PCA plot : PC1/PC2\")
text(pca\$rotation[,1],pca\$rotation[,2], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

#plot simple pca 2/3

pdf(\"pca_nocol.2.3.pdf\")
plot(pca\$rotation[,2],pca\$rotation[,3], xlab = \"PC2\", ylab = \"PC3\", main=\"PCA plot : PC2/PC3\")
text(pca\$rotation[,2],pca\$rotation[,3], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

#select row with the gene of interest (second parameter), melt, assign rownames
x2<- melt(x1[\"$second\",])
row.names(x2)<-x2\$variable
x2\$variable<-NULL

#cbind with pca matrix, pca$rotation
x3b<-cbind(pca\$rotation,x2)

#select row with the second gene of interest (second parameter), melt, assign rownames
x2b<- melt(x1[\"$third\",])
row.names(x2b)<-x2b\$variable
x2b\$variable<-NULL

#cbind with pca matrix, pca$rotation
x3<-cbind(x3b,x2b)

#assign basic color
x3\$color=\"grey\"

#assign 3 color scheme
x3\$color[x3\$value<range(x3\$value)[2]/3 & x3\$value>0] =\"#FFAAAA\"
x3\$color[x3\$value<(range(x3\$value)[2]/3)*2 & x3\$value>range(x3\$value)[2]/3] =\"#FF5555\"
x3\$color[x3\$value<(range(x3\$value)[2]/3)*3 & x3\$value>(range(x3\$value)[2]/3)*2] =\"#FF0000\"

#plot pca 1/2 with 3 colors in a gradient

pdf(\"pca.3col.1.2.pdf\")
plot(pca\$rotation[,1],pca\$rotation[,2], col=x3\$color, xlab = \"PC1\", ylab = \"PC2\", pch=19, main=\"PCA plot : PC1/PC2. Coloring scale: $second expression\")
text(pca\$rotation[,1],pca\$rotation[,2], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

#plot pca 2/3 with 3 colors in a gradient

pdf(\"pca.3col.2.3.pdf\")
plot(pca\$rotation[,2],pca\$rotation[,3], col=x3\$color, xlab = \"PC2\", ylab = \"PC3\", pch=19, main=\"PCA plot : PC2/PC3. Coloring scale: $second expression\")
text(pca\$rotation[,2],pca\$rotation[,3], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

x3\$color=\"grey96\"
x3\$color[x3\$value<range(x3\$value)[2]/10 & x3\$value>0] =\"#FFE5E5\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*2 & x3\$value>range(x3\$value)[2]/10] =\"#FFCCCC\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*3 & x3\$value>(range(x3\$value)[2]/10)*2] =\"#FFB2B2\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*4 & x3\$value>(range(x3\$value)[2]/10)*3] =\"#FF9999\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*5 & x3\$value>(range(x3\$value)[2]/10)*4] =\"#FF7F7F\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*6 & x3\$value>(range(x3\$value)[2]/10)*5] =\"#FF6666\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*7 & x3\$value>(range(x3\$value)[2]/10)*6] =\"#FF4C4C\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*8 & x3\$value>(range(x3\$value)[2]/10)*7] =\"#FF3333\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*9 & x3\$value>(range(x3\$value)[2]/10)*8] =\"#FF1919\"
x3\$color[x3\$value<(range(x3\$value)[2]/10)*10 & x3\$value>(range(x3\$value)[2]/10)*9] =\"#FF0000\"


#plot pca 1/2 with 10 colors in a gradient

pdf(\"pca.10col.1.2.pdf\")
plot(pca\$rotation[,1],pca\$rotation[,2], col=x3\$color, xlab = \"PC1\", ylab = \"PC2\", pch=19, main=\"PCA plot : PC1/PC2. Coloring scale: $second expression\")
text(pca\$rotation[,1],pca\$rotation[,2], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

#plot pca 2/3 with 10 colors in a gradient

pdf(\"pca.10col.2.3.pdf\")
plot(pca\$rotation[,2],pca\$rotation[,3], col=x3\$color, xlab = \"PC2\", ylab = \"PC3\", pch=19, main=\"PCA plot : PC2/PC3. Coloring scale: $second expression\")
text(pca\$rotation[,2],pca\$rotation[,3], row.names(pca\$rotation), cex=0.2, pos=4)
dev.off()

" > script.r

#run R script

chmod 775 script.r
./script.r
