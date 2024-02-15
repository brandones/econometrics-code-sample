# Code Sample

This code is from a summer research project done at the Levy Institute. It was all written
by Brandon Istenes. It has been simplified substantially; the original version has about ten
times as many scripts and data files. However it is organized according to similar principles,
using the same set of stages.

It is organized as a data pipeline. Scripts for the pipeline are in folders numbered
by stage. Data is organized similarly, starting with inputs at `data_0_raw` and ending
with `data_output`.

Mac and Linux users may set up and run the project with

```sh
./setup.sh
./run.sh
```

The setup file will install the Tidyverse R package if it is not already installed on your computer, and
will set up a Python virtual environment. The run file will execute all the scripts in order.

These scripts might work in the Windows Linux subsystem, but no guarantees.

## Notes on Script Files

Though the scripts in `0_downloading` are used to obtain the raw
data in the first place, I have checked in the raw data to this repository for simplicity.
The python file `0_downloading/download-wits-imports.py` should work for anyone with Python 3
installed. However a few other data files are generally stored on my Google Drive. These are
managed with the file `0_downloading/data.sh`. This will not work on your local machine,
as it requires access to the Google Drive API, and to my Drive in particular. It is included to
demonstrate one of the methods I use to manage data for analysis. Of course, Git LFS would be
preferable, but this method is considerably cheaper. Using APIs to download data directly, as
demonstrated in `download-wits-imports.py`, is also preferable, but not always possible.

All the R code in this project should be run with this folder as the working directory. In R, you can
use `setwd` to set the working directory, or start the R session from this folder. You can check the
current working directory using `getwd`:

```r
r$> getwd()
[1] "/home/brandon/Code/economics/code-sample"
```

## Nota Bene

I have professional experience building automated pipelines running at scale to populate databases;
building data analysis pipelines for econometric projects like this is new to me. I look forward to learning
other norms, tools, and practices from your team. I also have experience working with Jupyter
notebooks, R Shiny, and various JavaScript data visualization libraries.

The meaning and usefulness of the output will of course be a bit opaque to the reader. This code
sample is intended to showcase organization and code style rather than presentation.
