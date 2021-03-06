How did they make that? : R and visualization for science
========================================================
author: Todd Suomela
date: 2017-04-06
autosize: true


Introducing R and ggplot
========================================================



R is a statistical software programming environment that can be used across disciplines to visualize many different types of data.

You can add functions to the base R system by installing packages.

ggplot2 is one of the most popular packages for creating data visualizations

- use your package manager to install ggplot
- [ggplot2 documentation](http://docs.ggplot2.org/current/)


ggplot concept
========================================================

ggplot is built around the idea of building a plot with layers

- aesthetics
- geoms
- statistics
- coordinates and facets
- themes

aesthetics
========================================================

The aesthetics layer is required and contains the basic mapping between data and visual scales.


```r
p <- ggplot(iris, 
            aes(x = Sepal.Length,
                y = Sepal.Width,
                color = Species))
```


geoms
=========================


```r
p + geom_point()
```

![plot of chunk unnamed-chunk-3](s17_hdtt-ScienceVis-figure/unnamed-chunk-3-1.png)

statistics
=========================

using the stat layer to add a smoothing curve


```r
p + geom_point() + stat_smooth()
```

![plot of chunk unnamed-chunk-4](s17_hdtt-ScienceVis-figure/unnamed-chunk-4-1.png)

facets
=========================

facets allow you to facet the data across multiple plots


```r
p + geom_point() + facet_grid(. ~ Species) + stat_smooth()
```

![plot of chunk unnamed-chunk-5](s17_hdtt-ScienceVis-figure/unnamed-chunk-5-1.png)

coordinates
=========================

coordinates let you zoom into parts of a visualization, fix aspect ratios, and use polar coordinates (for pie charts)


```r
p + geom_point() + stat_smooth() + coord_cartesian(xlim = c(4.5, 5.5))
```

![plot of chunk unnamed-chunk-6](s17_hdtt-ScienceVis-figure/unnamed-chunk-6-1.png)

themes
=========================

adding decorations and formatting with themes


```r
p + geom_point() + facet_grid(. ~ Species) + stat_smooth() + theme_bw()
```

![plot of chunk unnamed-chunk-7](s17_hdtt-ScienceVis-figure/unnamed-chunk-7-1.png)


available mappings
========================

- x
- y
- colour
- fill
- size
- alpha
- linetype
- labels
- shape

****

- X axis position
- Y axis position
- color of dots, outlines of other shapes
- fill color
- diameter of points, thickness of lines
- transparency
- line, dash, pa, ern
- text on plot or axes
- shape

