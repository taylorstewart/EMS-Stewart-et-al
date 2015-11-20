EMS_Stewart
==========

This repository contains data files, analysis scripts, and the draft manuscript *Spatio-temporal and interindividual variation of Emerald Shiner, Notropis atherinoides, diet and energy density in Lake Erie* by Taylor R. Stewart and Mark W. Rogers.


# Data files (not in the repo)
* `data/CSMI_2014_EmeraldShiner.xlsx` -- Excel file containing the diet and calorimetry data specific to this project.
* `data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx` -- Excel file containing predatory zooplankton data (Replicate #1).
* `data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep2.xlsx` -- Excel file containing predatory zooplankton data (Replicate #2).
* `data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx` -- Excel file containing subsampled zooplankton data (Replicate #1).
* `data/zooplankton/2014_CSMI_Zp_Sub_Rep2.xlsx` -- Excel file containing subsampled zooplankton data (Replicate #2).
* `data/zooplankton/CSMI_Zp_Effort.xlsx` -- Excel file containing the effort needed for summarizing zooplankton biomass.
* `data/zooplankton/CSMI_Zp_Biomass_Summary.xlsx` -- Excel file containing summarized zooplankton biomass data.
* `data/water_quality/CSMI_WQ.xlsx` -- Excel file containing summarized whole-column water quality parameters.
* `data/nutrients/` -- Multiple Excel files containing nutrient parameters.

# Scripts
## Initialization
* `Data_Init.R` -- Initial loading of packages and data, initial preparation of data for other scripts.  This is sourced by all other scripts and creates the following data.frames.
    * `ems_cal`: Used in the calorimetry analysis.
    * `ems_diet`: Used in the diet analysis.
    * `zoop_biomass`: Used in the prey selectivity analysis.

## Length Frequency Analysis
* `lw_regression.R` -- Processes the original length frequency data to produce a length-weight regression which was used to predict missing weights.


# Other Files
* `EMS_Stewart.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
* `literature_resources` -- Folder of PDFs of relevant literature.
