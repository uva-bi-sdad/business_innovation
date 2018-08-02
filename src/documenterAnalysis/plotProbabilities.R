dt = data.table::fread("./data/business_innovation/working/DocumenterModelData/unratedArticlesWithRFProbs.csv")
rf = dt$rfLaunchProb
hist(rf, freq = TRUE)


library(ggplot2)
ggplot(dt, aes(rfLaunchProb))+
  geom_histogram(color="darkblue", fill="lightblue") +
  coord_flip() +
  ylab("Count of Articles") +
  xlab("Probability") +
  ggtitle("Predicted Probability of \nan Article being about a Launch") +
  theme(plot.title = element_text(hjust = 0.5))

#
# qplot(rf, data=dt, geom="histogram")
#
# d <- density(dt$rfLaunchProb,from = -0.1, to = 0.2) # returns the density data
# plot(d, xlab = "Count of Articles", y = "Probability", title = "Predicted Probability of \nan Article being about a Launch") # plots the results


