Overview
========

Traditional boxplot can be limited in use because it only shows median
and interquartile range, while this type of enhanced boxplot (eboxplot)
also shows 2.5th, 5th, 10th, 90th, 95th, and 97.5th percentiles.
Detailed descriptions of this new boxplot can be found at (reference
needed here). This program is an implemention of eboxplot in the
framework of ggplot2.

Installation
============

-   First download the program `geom_eboxplot.R`
-   Then in R: `source("geom_eboxplot.R")`
-   Use `geom_eboxplot` as a normal ggplot function

Usage
=====

    geom_eboxplot <- function(mapping = NULL, data = NULL,
                         stat = "eboxplot", position = "dodge2",
                         ...,
                         outlier.show = TRUE,
                         outlier.colour = NULL,
                         outlier.color = NULL,
                         outlier.fill = NULL,
                         outlier.shape = 19,
                         outlier.size = 1.5,
                         outlier.stroke = 0.5,
                         outlier.alpha = NULL,
                         median.show=FALSE,
                         median.digits=0,
                         median.font.size = 3.88,
                         median.font.angle = 0,
                         median.gap.coef = 1,
                         percent.show.group=NULL,
                         percent.show.side=1,
                         percent.font.size=3.88,
                         percent.font.angle=0,
                         percent.side.offset=0.1,
                         percent.font.colour="black",
                         varwidth = FALSE,
                         shade.upper = NA,
                         shade.lower = NA,
                         shade.fill = "pink",
                         shade.alpha= 1,
                         shade.value.absolute = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE)

Descriptions of arguments
=========================

-   **`outlier.show`** if outliers are added to the plot
-   **`outlier.colour, outlier.color, outlier.fill, outlier.shape, outlier.size, outlier.stroke, outlier.alpha`**
    aesthetics of outliers
-   **`median.show`** if `TRUE` then median values will be added to the plot. Adding median values using this way is not recommended, because the values will change with the y axis is not in the original scale. The recommended way to add the median values is adding `stat_summary(geom="text",aes(label=sprintf("%3.2f",..y..)), fun.y=median)`. See example.R for example.
-   **`median.digits, median.font.angle, median.font.angle`** aesthetics of median text
-   **`median.gap.coef`** controls how wide the gap in the median line. It takes values from 0 (completely open) to 1 (completely closed).
-   **`varwidth`** if `TRUE` then width of boxplots is proportional to square root of sample size
-   **`shade.upper, shade.lower`** range of shaded area
-   **`shade.value.absolute`** if `TRUE` then `shade.upper` and
    `shade.lower` are in absolute scale, otherwise they are percentiles
    ranging from 0 to 1
-   **`shade.fill, shade.alpha`** aesthetics of shaded area

Reference
=========

