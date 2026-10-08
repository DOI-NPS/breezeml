# breezeml

breezeml is a shiny interface for the NPSdataverse suite of packages. A user should be able to create a data package including Ecological Language Metadata based on one or more data files in .csv format.

# Install
```{r install, eval = FALSE}
remotes::install_github("doi-nps/breezeml")
```
Installation may take a few minutes and may require dependencies also be installed.

# Launch breezeml
From within an R console, run:

```{r launch, eval = FALSE}
breezeml::run_breezeml()
```
A blank window should pop up on your screen. I may take 1-2 minutes before the app launches. Please be patient.

# Using breezeml
Follow the in-app prompts and warnings

# breezeml products
breezeml will write directories and files to your working directory including:

1) A directory for all of the files and directories breezeml will write
2) A sub-directory that contains just the data package: a copy of your .csv files and the .xml file containing the Ecological Metadata Language metadata describing your data.
3) A second sub-directory containing all of the files used to build the EML as well as a .R script that can be run re-generate the EML metadata without having to use the breezeml app. This script and the associated .txt files effectively provides documentation for how the data package was created as well as reproducibility. 

# what to do with breezeml products

The data package should be uploaded to DataStore, appropriately reviewed if required, and activated/published.

The .txt files and .R script should be uploaded to DataStore in a Script reference type.
