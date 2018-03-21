library(ggplot2)
source("geom_eboxplot.R")

data <- read.csv("sample_dataset.csv")

p <- ggplot(data, aes(x=factor(age), y=value, color=factor(severity))) +
  geom_eboxplot(varwidth=TRUE, width=1.1, shade.lower=0.25, shade.value.absolute=FALSE, shade.fill="red", shade.alpha=0.1, digits=1, font.size=2, print.median=FALSE)
p
