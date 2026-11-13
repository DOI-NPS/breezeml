# breezeml

breezeml is a shiny interface for the NPSdataverse suite of R packages. A user should be able to create a data package including Ecological Language Metadata (EML) based on one or more data files in .csv format. The metadata creation is handled locally without exposing sensitive data to external servers. It is therefore suitable for creating data packages that contain information that should be restricted. 

Users will need to supply their data in .csv format. Based on those files, they can create a new metadata file .xml file containing schema-valid EML. Progress can be saved as a .json and reloaded for later work or sent to collaborators to work on.

Alternatively, users can opt to load an existing EML document in .xml format into breezeml to either edit the .xml (for instance if reviewers have suggestions) or to facilitate creating a new data package with substantial similarities to an existing data package (e.g. adding a subsequent year of data).

Finally, data package reviewers can load the .xml and .csv files into breezeml to run congruence checks necessary during a data package review.

breezeml makes every attempt to guide a user through the metadata creation process including multiple checks run on input values and will run DPchecker to check the entire data package. This necessitates having access to an NPS or DOI internal network or being signed on to the VPN (or whatever equivalent is currently in vogue).

breezeml write to disc, using the current working directory. It will write a single directory with a user-defined name that contains two sub directories. The directory name is based on the metadata file name (but without "_metadata.xml"). One sub-directory ("data_package") contains the data package (\*.csv data files and \*_metadata.xml EML metadata file). The other sub-directory ("data_package_creation") contains a copy of the data package (\*.csv data files and \*_metadata.xml EML metadata file) as well as a \*.R script file and all the \*.txt files necessary to recreate the metadata. The files within the "data_package_creation" sub-directory therefore represent completely transparent and reproducible documentation for post-data quality control data package creation. 


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

1) A user-named directory (based off of the user-defined metadata file name, but without the "_metadata.xml" suffix) for all of the files and directories breezeml will write
2) A sub-directory ("data_package") that contains just the data package: a copy of your .csv files and the \*.metadata.xml file containing the Ecological Metadata Language metadata describing your data.
3) A second sub-directory ("data_package_creation") containing all of the files used to build the EML as well as a .R script that can be run re-generate the EML metadata without having to use the breezeml app. This script and the associated .txt files effectively provides documentation for how the data package was created as well as providing reproducibility and transparency. 

# what to do with breezeml products

breezeml will faciliate uploading the data package to DataStore, requesting a data package review, and publishing/activating the data package upon completion of the review.

The .txt files and .R script should be uploaded to DataStore in a Script reference type.
