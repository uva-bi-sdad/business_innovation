# library
library(tidyverse)
library(viridis)
library(readxl)
library(dplyr)
data <- read_xlsx('./data/business_innovation/working/tmp_Stuff.xlsx')
data$Year <- factor(data$Year, levels = c(2015,2014,2013))
data$`Trade Journal` <- as.factor(data$`Trade Journal`)

#fix levels
data$`Trade Journal` <- str_replace_all(data$`Trade Journal`,"Phamacy","Pharmacy")

data <-spread(data,key=Year,value = Count)
# Create dataset
# data=data.frame(
#   individual=paste( "Mister ", seq(1,60), sep=""),
#   group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
#   value1=sample( seq(10,100), 60, replace=T),
#   value2=sample( seq(10,100), 60, replace=T),
#   value3=sample( seq(10,100), 60, replace=T)
# )

# Transform data in a tidy format (long format)
data = data %>% gather(key = 'Year', value='Count', `2013`:`2015`)
data = data %>% arrange(data$`Trade Journal`, data$`Total Count`)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=1
nObsType=nlevels(as.factor(data$Year))
to_add = data.frame(matrix(NA, empty_bar*nlevels(as.factor(data$`Trade Journal`))*nObsType, ncol(data)))
colnames(to_add) = colnames(data)
to_add$`Trade Journal`=rep(levels(as.factor(data$`Trade Journal`)), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(`Trade Journal`, data$Company)
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data= data %>% group_by(id, Company) %>% summarize(tot=sum(Count))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data %>%
  group_by(`Trade Journal`) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
# grid_data=grid_data[-1,]

data = data %>% arrange(data$id, data$`Total Count`)
# Make the plot
p = ggplot(data) +

  # Add the stacked bar
  geom_bar(aes(x=(as.factor(id)), y=data$Count, fill=Year), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  # geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(0,10,20,30), label = c("0", "10", "20", "30") , color="grey", size=5 , angle=0, fontface="bold", hjust=1) +

  ylim(-20,40) +
  theme_minimal() +
  theme(legend.text=element_text(size=28))+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    #panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+2, label=paste(Company,paste0('(',tot,')'),by=' '), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -1.5, label=`Trade Journal`), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p

