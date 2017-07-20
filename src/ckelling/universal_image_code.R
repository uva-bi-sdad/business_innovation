png(filename="~/git/oss/src/ckelling/images/7-19_images/dual_bar.png",
    units="in",
    width=15,
    height=15,
    #pointsize=12,
    res=72,
    bg = "transparent"
)

top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ topic, scales = "free") +
    theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))+
    ggtitle("Figure 4: Top Terms per Category")+ guides(fill=guide_legend(ncol=2))+
    theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=30))+
    theme(legend.position="bottom",legend.direction="vertical")


dev.off()

# ,size=1.5
# +
#     theme(text = element_text(size=20))

# +
#     theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))+
  theme(text = element_text(size=25))+theme(axis.text.x=element_text(size=20))

library(gridExtra)
png(filename="~/git/oss/src/ckelling/images/top_cat.png",
    units="in",
    width=10,
    height=10,
    #pointsize=20,
    res=72
)
p<-tableGrob(top_cat)
grid.arrange(p)
dev.off()




ggplot(summary, aes(x=date2, y=freq))+
    geom_point(aes(group = 1,colour = post.from_name), size=5) +
    #geom_point()
    labs(x = "Date", y = "Total Comments") +
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5))  +
    scale_colour_discrete(name="Source:" )+
    ggtitle("7. Comments Over Time")+
    theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))+
    theme(text = element_text(size=25))+theme(axis.text.x=element_text(size=20))

