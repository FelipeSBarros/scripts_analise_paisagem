######
##Script de preparação dos dados para uso no CONEFOR para um ou shapefiles de paisagem
#####

#carregando pacotes
library(raster)
library(reshape)
library(rgdal)
library(rgeos)
library(maptools)
library(beepr)
#####
##Defining path to all landscape shapefiles (after split)
#####
dsn_shapelist <- '/home/felipe/Projetos/Paraitinga/Conefor/Input_QGIS/Split/Oldgrowth'
#Creating list with all landscape shapefiles
shapelist <- as.list(ogrListLayers(dsn_shapelist))#lista dos shapes existentes na pasta dsn
#Creating some objects to be used guring the loop
shapes <-vector(mode="list", length= 28)
nodes <- NULL
dists <- NULL
dists_melt <- NULL
nlinhaso <- NULL
dimpesos <- NULL
posicoes <- NULL
pesos <- NULL
nlinhaso <- NULL
dimpesos <- NULL
pesos2 <- NULL
distancia <- NULL
#####
#loop para importar shapes
#####
system.time( #Monitorando o tempo de processamento do loop
for( i in 1:length(shapes))
{

#Importando os shapes para uma lista
  shapes[[i]] <-  readOGR(dsn=dsn_shapelist, layer=shapelist[[i]])
#Adding a new column and calculating the area of each polygon
  shapes[[i]]$area <- round(gArea(shapes[[i]], byid=TRUE),4)
#creating nod matrix to have values inserted
  nodes[[i]]<-matrix(c(1:length(shapes[[i]]),shapes[[i]]$area),nrow=length(shapes[[i]]))
#writing tables with the IDHEX in the name of the text file
  write.table(nodes[[i]], file=sprintf("%s.nodes_oldgrowth",shapes[[i]]$HEXID[i]), row.names = F, col.names = F,    sep="\t", quote=F)
#Calculating distances between each polygon
  dists[[i]]<-gDistance(shapes[[i]], shapes[[i]],byid=T)
  dists_melt[[i]] <- melt(dists[[i]])[melt(upper.tri(dists[[i]]))$value,]
  dists_melt[[i]][,3] <- round(dists_melt[[i]][,3],2) 
#organizing distance data to be exported as text file
  posicoes[[i]] <- data.frame(c(1:length(shapes[[i]])))
  pesos[[i]] <- data.frame(matrix(NA,nrow=1, ncol=2))
  
  for(o in 1:(length(shapes[[i]])-1))
  {
    nlinhaso[[i]] <- (dim(posicoes[[i]])[1])-o
    dimpesos[[i]] <- dim(pesos[[i]])[1]
    pesos[[i]][(1+dimpesos[[i]]):(dimpesos[[i]]+nlinhaso[[i]]),1]<- o
    pesos[[i]][(1+dimpesos[[i]]):(dimpesos[[i]]+nlinhaso[[i]]),2]<-((o+1):dim(posicoes[[i]])[1])
   }
  
  pesos2[[i]] <- (pesos[[i]][2:dim(pesos[[i]])[1],])
  distancia[[i]] <- cbind(pesos2[[i]],dists_melt[[i]][,3])
#writting distance results in text files
  write.table(distancia[[i]],file=sprintf("%s.dists_oldgorwth", shapes[[i]]$HEXID[i]), sep="\t", quote=F)
}, gcFirst = TRUE)
i <- 1
############
