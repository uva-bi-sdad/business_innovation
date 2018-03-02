#temp for visualizations
library(stringr)
library(ggplot2)

#read in all of the text files: note that the dataset used is ALL FDA APPROVALS
dat <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_JAN15.csv')
dat2 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_FEB15.csv')
dat3 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_MAR15.csv')
dat4 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_APR15.csv')
dat5 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_MAY15.csv')
dat6 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_JUN15.csv')
dat7 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_JUL15.csv')
dat8 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_AUG15.csv')
dat9 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_SEP15.csv')
dat10 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_OCT15.csv')
dat11 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_NOV15.csv')
dat12 <- read.csv('./data/business_innovation/working/FDA_2015/DrugsFDA FDA Approved Drug Products_DEC15.csv')
#combine all the files
FDA <- rbind(dat,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12)
FDA = data.frame(FDA, stringsAsFactors = F)

#do some basic cleaning
FDA$Approval.Date = as.character.Date(FDA$Approval.Date)
FDA$Drug.Name = as.character(FDA$Drug.Name)
#clean out all of the submissions who had Submission Status as "Tentative Approval"? Maybe in future
#this takes out about 400 entries
#FDA = subset(FDA,FDA$Submission.Status == "Approval")

#do some summaries
p1 <- table(FDA$Company)
p1 <- as.data.frame(p1)
p1 <- subset(p1, Freq >= 50)
barplot(p1$Freq, names.arg = p1$Var1)

#now in ggplot2
vis <- ggplot(p1,aes(x= reorder(Var1, -as.numeric(Freq)), y= Freq)) + geom_bar(stat= "identity") +
  scale_y_continuous(breaks = round(seq(0, max(p1$Freq), by = 20),1)) +
  theme_bw()+
  theme(title = element_text(size=20), axis.text.x=element_text(size=10, face = 'bold'), axis.text.y = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),axis.title.y = element_text(size = 17, face = 'bold'))+
  theme(plot.title = element_text(size = 20, face = 'bold'))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("Company") +
  ylab("Number of Approvals") + ggtitle("Companies with most FDA Approvals in 2015")
vis
ggsave(vis, filename = "FDA_Companies_2015.png",width=20,height=11.25,scale=1,path = "./data/business_innovation/working/FDA_2015/")
