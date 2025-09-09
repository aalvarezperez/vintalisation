# vintalisation

Custom palettes and utility functions for use with ggplot2 in R.

## Installation

You can install the development version of `vintalisation` from GitHub using the `devtools` package:

```r
# install.packages("devtools")
library(devtools)
install_github("aalvarezperez/vintalisation")
```

## Basic Usage

After installation, load the package and use the custom palettes in your ggplot2 visualizations:

```r
library(ggplot2)
library(vintalisation)

# Example: Using a custom color scale
p <- ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
  geom_bar() +
  scale_fill_adevinta() +
  theme_adevinta()

print(p)
```

## Features
- Custom color palettes for ggplot2
- Utility functions for palette management

## License
MIT
