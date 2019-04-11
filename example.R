library(ggplot2)
source("geom_eboxplot.R")


#:::::::::::::::::::::::::::::::::::::::::
# Read in the dataset                    :
#:::::::::::::::::::::::::::::::::::::::::

data <- read.csv("sample_dataset.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Example 1: use median.show to add median values to the plot                    :
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# This does not work when y axis is transformed (for example, scale_y_log)
p <- ggplot(data, aes(x=factor(age), y=value, color=factor(severity))) +
  geom_eboxplot(varwidth=TRUE, 
	width=1.1, 
	shade.lower=0.25, 
	shade.value.absolute=FALSE, 
	shade.fill="red", 
	shade.alpha=0.1, 
	median.show=TRUE,
	median.gap.coef=0.1)
p


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Example 2: use stat_summary to add median values to the plot                    :
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# you need to specify the same position option to make the median values in the right position
# this method will have problem when varwidth is set to be TRUE

p <- ggplot(data, aes(x=factor(age), y=value, color=factor(severity))) +
  geom_eboxplot(varwidth=FALSE, 
	width=0.8, 
	shade.lower=0.25, 
	shade.value.absolute=FALSE, 
	shade.fill="red", 
	shade.alpha=0.1, 
	median.gap.coef=0.1, 
	position = position_dodge(width = 0.8)) + 
  stat_summary(geom="text", 
  	aes(label=sprintf("%3.2f",..y..)), 
  	fun.y=median, 
  	position = position_dodge(width = 0.8),
  	show.legend=FALSE)
p