# breezeml

breezeml is a shiny interface for the NPSdataverse suite of R packages. A user should be able to create a data package including Ecological Language Metadata based on one or more data files in .csv format. The metadata creation is handled locally without exposing sensitive data to external servers. It is therefore suitable for creating data packages that contain information that should be restricted.

# Install
```{r install, eval = FALSE}
remotes::install_github("doi-nps/breezeml")
```
Installation may take a few minutes and may require dependencies also be installed. If you have not previously used the NPSdataverse suite of packages, this could take a fair amount of time.

## Installing during development
Beta testers that want to install and test breezeml before its official release can do so, but should check here regularly to see if the dependencies require development versions of other packages.

Currently, breezeml requires a development version of DPchecker and EMLeditor. To load the development version, run:

```{r install_dev_packages, eval = FALSE}
NPSdataverse::detach_NPSdataverse
remotes::install_github("doi-nps/DPchecker@breezeml_dev")
remotes::install_github("doi-nps/EMLeditor@breezeml_dev")
```
You will then need to restart your R session before launching breezeml. The breezeml_dev branch of DPchecker and EMLeditor should be 100% reverse compatible with existing script-based EML creation workflows so loading these development versions shouldn't affect any existing metadata creation or data package authoring projects.

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

The resulting data package should be uploaded to DataStore, appropriately reviewed if required, and activated/published.

The .txt files and .R script should be uploaded to DataStore in a Script reference type.
