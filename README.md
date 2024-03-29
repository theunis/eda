# Binary Exploratory Data Analysis

This tool will help you do initial analysis on your data where your goal is binary classification. It will automatically create plots for each variable with the averages of the target variable for each category or automatically created bins (for numeric variables).

## Installation

You can install this using the `remotes` package:

``` r
install.packages('remotes')
remotes::install_github('theunis/eda')
```

## Usage

This package contains an add-in which you can only use use in RStudio. You will need to have a data.frame loaded in your workspace. Then you can click on Addins under your menu bar at the top and click on 'Run EDA tool'. If it doesn't show, try restarting RStudio first.

Point to the right data.frame, select your target variable and select the variables you want to analyze (be careful not to pick variables with many categories, or numeric variables which are imported as text, it will take a long time and will not be useful).

There are functions to create seperate plots of each variable, you can check the help file for these functions.
