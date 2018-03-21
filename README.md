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

    geom_eboxplot(mapping = NULL, data = NULL,
        stat = "eboxplot", position = "dodge2",
        ...,
        outlier.show = TRUE, outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL,
        outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5, outlier.alpha = NULL,
        font.size = 3.88, font.angle = 0, digits=0, varwidth = FALSE,
        shade.upper = NA, shade.lower = NA, shade.fill = "pink", shade.alpha= 1, print.median=TRUE,
        shade.value.absolute = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

Descriptions of arguments
=========================

-   **`outlier.show`** if outliers are added to the plot
-   **`outlier.colour, outlier.color, outlier.fill, outlier.shape, outlier.size, outlier.stroke, outlier.alpha`**
    aesthetics of outliers
-   **`print.median`** if `TRUE` then median values will be added to the
    plot
-   **`font.size, font.angle, digits`** aesthetics of median text
-   **`varwidth`** if `TRUE` then width of boxplots is proportional to
    square root of sample size
-   **`shade.upper, shade.lower`** range of shaded area
-   **`shade.value.absolute`** if `TRUE` then `shade.upper` and
    `shade.lower` are in absolute scale, otherwise they are percentiles
    ranging from 0 to 1
-   **`shade.fill, shade.alpha`** aesthetics of shaded area

Reference
=========
