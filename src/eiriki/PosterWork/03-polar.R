#make polar hist
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)

pharmtimes <- read.csv('./data/business_innovation/final/Trade_Journals/pharmacytimes_by_year.csv',stringsAsFactors = F)
pharmtimes$Year <- factor(pharmtimes$Year, levels = c(2015,2014,2013))
vis <- ggplot(pharmtimes,aes(x= reorder(Company, -as.numeric(pharmtimes$max)),y=Freq)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 35, by = 5))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Product News 2013-2015")
vis + coord_polar()


#####combining all together

temp <- read_xlsx('./data/business_innovation/working/tmp_Stuff.xlsx')
temp$Year <- factor(temp$Year, levels = c(2015,2014,2013))
temp$Company <- as.factor(temp$Company)


vis <- ggplot(temp,aes(x= Company,y=Count)) + geom_col(color='black',aes(fill = Year)) +
  scale_y_continuous(breaks = round(seq(0, 35, by = 5))) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=24, face = 'bold'), axis.text.y = element_text(size = 22, face = 'bold'),
        axis.title.x = element_text(size = 28, face = 'bold'),axis.title.y = element_text(size = 28, face = 'bold'))+
  theme(plot.title = element_text(size = 38, face = 'bold'))+
  theme(legend.text=element_text(size=20),legend.key.size = unit(1.25,'cm'))+
  theme(axis.text.x = element_text(angle = -35, hjust = 0)) + xlab("Company") +
  ylab("Number of News Articles") + ggtitle("Top Companies in Pharmacy Times Product News 2013-2015")
vis + coord_polar()


####PASTE
# Building the ggplot object

# histograms
p<-ggplot(temp)+geom_rect(
  aes(
    xmin=0,
    xmax=40,
    ymin=0,
    ymax=40,
    fill=temp$Year)
)

# item labels
readableAngle<-function(x){
  angle<-x*(-360/totalLength)-alphaStart*180/pi+90
  angle+ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,180,0)
}
readableJustification<-function(x){
  angle<-x*(-360/totalLength)-alphaStart*180/pi+90
  ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,1,0)
}

dfItemLabels<-ddply(df,.(item),summarize,xmin=xmin[1])
dfItemLabels<-within(dfItemLabels,{
  x<-xmin+binSize/2
  angle<-readableAngle(xmin+binSize/2)
  hjust<-readableJustification(xmin+binSize/2)
})

p<-p+geom_text(
  aes(
    x=x,
    label=item,
    angle=angle,
    hjust=hjust),
  y=1.02,
  size=6,
  vjust=0.5,
  data=dfItemLabels)

# guides




# family labels
if(familyLabels){
  #     familyLabelsDF<-ddply(df,.(family),summarise,x=mean(xmin+binSize),angle=mean(xmin+binSize)*(-360/totalLength)-alphaStart*180/pi)
  familyLabelsDF<-aggregate(xmin~family,data=df,FUN=function(s) mean(s+binSize))
  familyLabelsDF<-within(familyLabelsDF,{
    x<-xmin
    angle<-xmin*(-360/totalLength)-alphaStart*180/pi
  })

  p<-p+geom_text(
    aes(
      x=x,
      label=family,
      angle=angle),
    data=familyLabelsDF,
    y=1.2)
}
#   # empty background and remove guide lines, ticks and labels
p<-p+theme(
  panel.background=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank()
)

# x and y limits
p<-p+xlim(0,tail(df$xmin+binSize+spaceFamily,1)/circleProportion)
p<-p+ylim(0,outerRadius+0.2)

# project to polar coordinates
p<-p+coord_polar(start=alphaStart)

# nice colour scale
p<-p+scale_fill_brewer(palette='Set3',type='qual')

p
}


#CREATE POLAR HISTOGRAM
p<-polarHistogram(hope,familyLabel=FALSE, guides = NULL, direction = "outwards")
print(p)
