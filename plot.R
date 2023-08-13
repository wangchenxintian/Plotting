setwd("D:/R/")
#bubblematrix
library(ggplot2)
library(scatterpie)
library(RColorBrewer)
colormap <- c( "#FEFF1A","#FF8C00","#1SUMEFF","#0075EB","#DDDDDD")
#colormap <- colorRampPalette(brewer.pal(4, "Set2"))(4)
crime <- read.csv("E:/R/coreARG.txt",header = TRUE, sep = "\t", stringsAsFactors = F)
radius <- sqrt(crime$SUM / pi)
Max_radius<-max(radius)
Bubble_Scale<-0.1
crime$radius <- Bubble_Scale * radius/Max_radius
mydata<-crime[,c(2,4,3,5:9)] #数据集的选择与排序处???
Col_Mean<-apply(mydata,2,mean)
Col_Sort<-sort(Col_Mean,index.return=TRUE,decreasing = TRUE)
mydata<-mydata[,Col_Sort$ix]
#对X 轴和Y 轴变量数值做归一化处理至[0, 1]区间
x<-(mydata$Region-min(mydata$Region))/(max(mydata$Region)-min(mydata$Region))+0.00001
y<-(mydata$subtype-min(mydata$subtype))/(max(mydata$subtype)-min(mydata$subtype))+0.00001
#y<-(mydata$subtype)
#设置X 和Y 轴的刻度标签
xlabel<-seq(0,8,1)
xbreak<-(xlabel-min(mydata$Region))/(max(mydata$Region)-min(mydata$Region))+0.00001
ylabel<-seq(0,20,1)
ybreak<-(ylabel-min(mydata$subtype))/(max(mydata$subtype)-min(mydata$subtype))+0.00001
mydata1<-data.frame(x,y,radius=crime$radius) # mydata1 为X 轴和Y 轴绘制数值变量和饼图绘制半径
mydata2<-cbind(mydata1,mydata)
Legnd_label<-colnames(mydata2)[4:8] #保存图例的数据系列名???
colnames(mydata2)[4:8]<-LETTERS[1:5] #按字母顺序重新命名数据系列的列名
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

