# Levy Summer Research

The code is all in R. You must have `tidyverse` installed. All other libraries that
are needed will be installed at script runtime (if not installed already).

This folder, "Levy Summer Datasets",
is what I will call the "project directory" here. All the code in this project should be run
with this folder as the working directory. You can use `setwd` to set the working directory.
Here I am using `getwd` to show the path of my current working directory.

```r
r$> getwd()
[1] "/home/brandon/GDrive/Levy/Levy Summer Datasets"
```

Raw data is in `data_raw`. Do not modify data in there. The scripts in `cleaning`
read in data from `data_raw` and create new, cleaned up tables in the project directory.
Analysis scripts create files in the `output` directory.