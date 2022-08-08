# Treehugging: A Global Multilevel Analysis

R code to run multilevel models of treehugging (i.e. valuing nature more than humans) around the world.

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("brms", "ggrepel", "readxl", "rnaturalearth", 
                   "rnaturalearthdata", "rmapshaper"))
```

### Executing code

1. Download this code repository
2. Set the working directory to this code repository on your local machine `setwd("myPath")`
3. Load the `targets` package with `library(targets)`
4. Run the pipeline with `tar_make()`
5. To load individual targets into your environment, run `tar_load(targetName)`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
