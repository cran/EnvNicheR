niche<-function(data=data, pixs=3, d.main=1, xlab="Polar coordinate X",
ylab="Polar coordinate Y", cex.lab=1.5, font.lab=1, main=NULL, zmax = NULL,
ztransf = function(x){x}, colramp = IDPcolorRamp, cex = 1,cex.main = 1, font.main=1,
legend = TRUE, d.legend = 1, nlab.xaxis = 5, nlab.yaxis = 5, 
minL.axis = 3, las = 1, border = FALSE,oma = c(5,4,1,0)+0.1, tcl = -0.3,
outline=FALSE, color="NULL", range = 1.5, width = NULL, varwidth = FALSE,
plot = TRUE, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
cex.boxplot=1.5, family="serif", line=1, 
file1 = "List of species.txt", file2 = "Environmental variables.txt",
append = FALSE, quote = TRUE, sep = " ", na = "NA", dec = ".",
row.names = FALSE,col.names = TRUE,
qmethod = c("escape", "double"), fileEncoding = ""){

datos<-na.exclude(data)

library(IDPmisc)

if (missing(pixs)) pixs=3 else pixs=pixs
if (missing(d.main)) d.main=1 else d.main=d.main
if (missing(xlab)) xlab="Polar coordinate X" else xlab=xlab
if (missing(ylab)) ylab="Polar coordinate Y" else ylab=ylab
if (missing(cex.lab)) cex.lab=1.5 else cex.lab=cex.lab
if (missing(font.lab)) font.lab=1 else font.lab=font.lab
if (missing(main)) main=NULL else main=main
if (missing(zmax)) zmax=NULL else zmax=zmax
if (missing(ztransf)) ztransf=function(x){x} else ztransf=ztransf
if (missing(colramp)) colramp=IDPcolorRamp else colramp=colramp
if (missing(cex)) cex=1 else cex=cex
if (missing(cex.main)) cex.main=1 else cex.main=cex.main
if (missing(font.main)) font.main=1 else font.main=font.main
if (missing(legend)) legend=TRUE else legend=legend
if (missing(d.legend)) d.legend=1 else d.legend=d.legend
if (missing(nlab.xaxis)) nlab.xaxis=5 else nlab.xaxis=nlab.xaxis
if (missing(nlab.yaxis)) nlab.yaxis=5 else nlab.yaxis=nlab.yaxis
if (missing(cex.boxplot)) cex.boxplot=1.2 else cex.boxplot=cex.boxplot
if (missing(minL.axis)) minL.axis=3 else minL.axis=minL.axis
if (missing(las)) las=1 else las=las
if (missing(border)) border=FALSE else border=border
if (missing(oma)) oma=c(5,4,1,0)+0.1 else oma=oma
if (missing(tcl)) tcl=-0.3 else tcl=tcl
if (missing(outline)) outline=FALSE else outline=outline
if (missing(range)) range=1.5 else range=range
if (missing(width)) width=NULL else width=width
if (missing(varwidth)) varwidth=FALSE else varwidth=varwidth
if (missing(plot)) plot=TRUE else plot=plot
if (missing(log)) log="" else log=log
if (missing(pars)) pars=list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5) else pars=pars
if (missing(append)) append=FALSE else append=append
if (missing(quote)) quote=TRUE else quote=quote
if (missing(sep)) sep=" " else sep=sep
if (missing(na)) na="NA" else na=na
if (missing(dec)) dec="." else dec=dec
if (missing(row.names)) row.names=FALSE else row.names=row.names
if (missing(col.names)) col.names=TRUE else col.names=col.names
if (missing(qmethod)) qmethod=c("escape", "double") else qmethod=qmethod
if (missing(fileEncoding)) fileEncoding="" else fileEncoding=fileEncoding
if (missing(family)) family="serif" else family=family
if (missing(line)) line=1 else line=line

#Standardization 0 to 1

a<-dim(datos)

datosE<-datos[,1:7]

for (z in 8:a[2]){
datosC<-(datos[,z]-min(datos[,z], na.rm=TRUE))/(max(datos[,z], na.rm=TRUE)-min(datos[,z], na.rm=TRUE))
datosE<-cbind(datosE,datosC)
}

colnames(datosE)<-colnames(datos)

#Estimation of polar coordinates

angle<-360/(a[2]-7)

datosX<-datosE[,1:7]
h<-0
for (z in 8:a[2]){
h<-h+1
datosC<-datosE[,z]*cos(angle*h)
datosX<-cbind(datosX,datosC)
}


datosF<-cbind(datosE[,1:7],apply(datosX[,8:a[2]],1,sum))

datosY<-datosE[,1:7]
h<-0
for (z in 8:a[2]){
h<-h+1
datosC<-datosE[,z]*sin(angle*h)
datosY<-cbind(datosY,datosC)
}

datosF<-cbind(datosF,apply(datosY[,8:a[2]],1,sum))

colnames(datosF)<-c(colnames(datosE[1:7]),"X","Y")


#Plot of polar coordinates

zmax<-iplot(x=datosF$X,y=datosF$Y, pixs=pixs, d.main=d.main, xlab=xlab,
ylab=ylab, cex.lab=cex.lab, font.lab=font.lab, main=main, zmax = zmax,
ztransf = ztransf, colramp = colramp, cex = cex,cex.main = cex.main,
legend = legend, d.legend = d.legend, nlab.xaxis = nlab.xaxis, nlab.yaxis = nlab.yaxis,
minL.axis = minL.axis, las = las, border = border,oma = oma, tcl= tcl, family=family)

#Identify the coordinates
point<-""
point<-as.data.frame(locator(4))
hh<-length(point)
if (hh==0){
maxx<-max(datosF$X, na.rm=TRUE)
minx<-min(datosF$X, na.rm=TRUE)
maxy<-max(datosF$Y, na.rm=TRUE)
miny<-min(datosF$Y, na.rm=TRUE)
}
else{
maxx<-max(point$x, na.rm=TRUE)
minx<-min(point$x, na.rm=TRUE)
maxy<-max(point$y, na.rm=TRUE)
miny<-min(point$y, na.rm=TRUE)
}



matriz<-cbind(datos,datosF$X, datosF$Y)

datos3<-matriz[(datosF$X>minx)&(datosF$X<maxx),]
datos3<-datos3[(datosF$Y>miny)&(datosF$Y<maxy),]
datos3<-na.exclude(datos3)
names<-colnames(datos3)


m<-a[2]-7

#Boxplot with the range of the environmental variables of the polar coordinates selected


par(mfcol=c(1,m))
for (z in 8:a[2]){
h<-z/a[2]
if (color=="NULL") col=hsv(h = h, s = 1, v = 1, alpha=1) else col=color[z-7]
boxplot(x=datos3[,z],outline= outline, col=col,
range = range, width = width, varwidth = varwidth, plot = plot, log = log,
pars = pars, horizontal = FALSE, cex.axis=cex.boxplot)
mtext(names[z], side=1, line=line,outer = FALSE,at = NA,adj = NA, padj = NA, cex = cex.lab, col = NA, font = font.lab, family=family, las=las)
}



datos4<-subset(datos3[,1:5], !duplicated(datos3$Species)) 

#List of species of the polar coordinates selected
write.table(x=datos4,file = file1, append = append, quote = quote, sep = sep,
eol = "\n", dec = dec, row.names = row.names,col.names = col.names,
qmethod = qmethod, fileEncoding = fileEncoding)
y<-dim(datos3)

Env<-summary(datos3[,c(8:y[2]-2)])

write.table(x=Env,file = file2, append = append, quote = quote, sep = sep,
eol = "\n", dec = dec, row.names = row.names,col.names = col.names,
qmethod = qmethod, fileEncoding = fileEncoding)


}
