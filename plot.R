#bubblebpie
library(ggplot2)
library(scatterpie)
library(RColorBrewer)
colormap <- c( "#FEFF1A","#FF8C00","#1SUMEFF","#0075EB","#DDDDDD")
#colormap <- colorRampPalette(brewer.pal(4, "Set2"))(4)
crime <- read.csv("ARG.txt",header = TRUE, sep = "\t", stringsAsFactors = F)
radius <- sqrt(crime$SUM / pi)
Max_radius<-max(radius)
Bubble_Scale<-0.1
crime$radius <- Bubble_Scale * radius/Max_radius
mydata<-crime[,c(2,4,3,5:9)] #Selection and sorting of data sets
Col_Mean<-apply(mydata,2,mean)
Col_Sort<-sort(Col_Mean,index.return=TRUE,decreasing = TRUE)
mydata<-mydata[,Col_Sort$ix]
#The values of X-axis and Y-axis variables are normalized to the interval [0, 1]
x<-(mydata$Region-min(mydata$Region))/(max(mydata$Region)-min(mydata$Region))+0.00001
y<-(mydata$subtype-min(mydata$subtype))/(max(mydata$subtype)-min(mydata$subtype))+0.00001
#y<-(mydata$subtype)
#Set the scale labels for the X and Y axes
xlabel<-seq(0,8,1)
xbreak<-(xlabel-min(mydata$Region))/(max(mydata$Region)-min(mydata$Region))+0.00001
ylabel<-seq(0,20,1)
ybreak<-(ylabel-min(mydata$subtype))/(max(mydata$subtype)-min(mydata$subtype))+0.00001
mydata1<-data.frame(x,y,radius=crime$radius) # mydata1 Plot numerical variables for the X and Y axes and pie charts plot radius
mydata2<-cbind(mydata1,mydata)
Legnd_label<-colnames(mydata2)[4:8] 
colnames(mydata2)[4:8]<-LETTERS[1:5] 
ggplot() +
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour=NA),
         panel.border = element_rect(colour="black",fill=NA)) +
  geom_scatterpie(aes(x=x, y=y,r=radius), data=mydata2, cols=colnames(mydata2)[4:8],alpha=0.9,size=0.25) +
  scale_fill_manual(values=colormap,labels=Legnd_label)+
  geom_scatterpie_legend(mydata2$radius, x=0.1, y=0.95, n=5,labeller=function(x) round((x* Max_radius/
                                                                                          Bubble_Scale)^2*pi))+
  scale_x_continuous(breaks=xbreak, labels=xlabel)+
  scale_y_continuous(breaks=ybreak, labels=ylabel)+
  xlab("Region")+
  ylab("subtype")+
  coord_fixed()



#heatmap
install.packages("pheatmap")
library(pheatmap)
ARG<- read.table("Genus_percentage.txt",header=TRUE)

matr<-data.matrix(ARG)

pheatmap(matr, cluster_rows=FALSE, cluster_cols=FALSE, fontsize=13, fontsize_row=11, cellwidth=19, cellheight=11.5, legend=TRUE)



#upset venn
install.packages("UpSetR")
library(UpSetR)
library(ggplot2)
regions <-read.csv(system.file("upsetdata", "data.csv", package ="UpSetR")，header = T，sep =";")
 
##text.scale =c(intersection size title,intersection size ticklabels, set size title, set size tick labels, set names, numbers above bars)
upset(regions,nsets = 7, number.angles = 30, point.size = 3.5, line.size = 2, mainbar.y.label = "Intersection Size",sets.x.label = "No. of ARG subtype", text,scale =c(1.3, 1.3,1, 1,2, 0.75))
#Custom groups for the intersection
upset(regions ,sets = c("South Africa","Macau","Singapore","Brazil", "Hong Kong", "The USA", "Taiwan", "Mainland China"), mb.ratio = c(0.55,0.45), order.by = "freq")
#The intersection results are grouped, nintersects the number of crossing points and cutoff the threshold of crossing results
upset(movies,nintersects = 70, group.by = "sets", cutoff = 7)


#bubble matrix
data<-read.csv("bubble.csv", header = T)        
library(ggplot2)
library(tidyverse)
data_melt<- gather(data, key=ARGtype, value=Value, Vancomycin:Aminoglycoside)
names(data_melt) = c("color", 'Ecotype', 'ARGtype', 'Value')
data_melt$Ecotype <- as.character(data_melt$Ecotype)
data_melt$Ecotype <- factor(data_melt$Ecotype, levels=unique(data_melt$Ecotype))
data_melt$ARGtype <- as.character(data_melt$ARGtype)
data_melt$ARGtype <- factor(data_melt$ARGtype, levels=unique(data_melt$ARGtype))
cbPalette <- c( "#FC690C","#FEFF1A","#1AFEFF","#00A9E6","#006400","#8CB2D2" )
p<-ggplot(data_melt, aes(x = Ecotype, y = ARGtype, size = Value, color=color)) + geom_point() +   scale_colour_manual(values=cbPalette) +
  theme_bw(base_size = 12)+
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour=NA),
         panel.border = element_rect(colour="black",fill=NA))
p
p + xlab("Ecotype(s)") + theme(axis.title.x = element_text(size = 20, family = "myFont", color = "black", face = "bold", vjust = 0.5, hjust = 0.5))
p + theme(axis.text.x=element_text(size=13,angle = 90,color = "black"),axis.text.y=element_text(size=13,color = "black",),panel.grid.major=element_line(colour=NA))  +
  guides(color = FALSE)  + labs(size="Abundance") + xlab("Ecotype(s)") + ylab("ARG Type") +
  scale_size(range = c(3, 12))

p <- p +  scale_y_continuous(trans = "reverse", breaks = unique(data_melt$ARGtype))


#boxplot
library(ggplot2)
  install.packages("tydir")
  library(tidyr)
  
B.data<-read.csv("D:/R/boxplot.csv",
                   header=TRUE,check.names = FALSE)
  str(B.data)
  
  B.data$SampleID<-NULL
  B.data_long <- gather(B.data, group, value, `Animal Feces`:Unknown, factor_key=TRUE)
  
  ggplot(B.data_long, aes(x=group,y=value,color=group)) +
    geom_boxplot(outlier.shape = NA) + 
    labs(fill = "group") + 
    xlab("Group")+ylab("Percentage(100%)")+
    geom_point(aes(x=group,y=value,color=group),position=position_jitterdodge(),alpha=0.7) +
    theme_bw(base_size = 12)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  #ggplot(B.data_long, aes(x=group,y=value,fill=factor(group))) +
    geom_boxplot() + 
    labs(fill = "group") + 
    xlab("Group")+ylab("Percentage(100%)")+
    geom_point(position=position_jitterdodge(),alpha=0.7) +
    theme_bw(base_size = 12)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
  
  ####################### New ######################
  AF<-B.data$`Animal Feces`
  OutVals <- boxplot(AF)$out
  which(AF %in% OutVals)
  AF <- AF[-which(AF %in% OutVals)]
  mean(AF)
  
  HF<-B.data$`Human Feces`
  OutVals <- boxplot(HF)$out
  which(HF %in% OutVals)
  HF <- HF[-which(HF %in% OutVals)]
  mean(HF)
  
  NATURAL<-B.data$Natural
  mean(NATURAL)
  
  WWTP<-B.data$WWTP
  mean(WWTP)
  
  UNKNOWN<-B.data$Unknown
  OutVals <- boxplot(UNKNOWN)$out
  which(UNKNOWN %in% OutVals)
  UNKNOWN <- UNKNOWN[-which(UNKNOWN %in% OutVals)]
  mean(UNKNOWN)
  
  value<-c(AF,HF,NATURAL,WWTP,UNKNOWN)
  group<-c(rep("Animal Feces",length(AF)),
           rep("Human Feces",length(HF)),
           rep("Natural",length(NATURAL)),
           rep("WWTP",length(WWTP)),
           rep("Unknown",length(UNKNOWN)))
  B.data.new<-as.data.frame(cbind(group,value))
  B.data.new$value<-as.character(B.data.new$value)
  B.data.new$value<-as.numeric(B.data.new$value)
  
  
  ggplot(B.data.new, aes(x=group,y=value,color=group)) +
    geom_boxplot(outlier.shape = NA) + 
    labs(fill = "group") + 
    xlab("Group")+ylab("Percentage(100%)")+
    geom_point(aes(x=group,y=value,color=group),position=position_jitterdodge(),alpha=0.5) +
    theme_bw(base_size = 12)+
    annotate(label = sprintf("0"),
             geom = "text", x = "Animal Feces", y = 5, size = 4)+
    annotate(label = sprintf("0"),
             geom = "text", x = "Human Feces", y = 5, size = 4)+
    annotate(label = sprintf("57.42"),
             geom = "text", x = "Natural", y = 62, size = 4)+
    annotate(label = sprintf("2.90"),
             geom = "text", x = "Unknown", y = 15, size = 4)+
    annotate(label = sprintf("36.03"),
             geom = "text", x = "WWTP", y = 40, size = 4)+
    coord_cartesian(ylim=c(0, 100))
                           
                    
                         
