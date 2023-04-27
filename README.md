# Lewis Flax
This repository contains all the R and SAS scripts to reproduce results from Gossweiler et al 2023, "Survival analysis of freezing stress in the North American native perennial flax, Linum lewisii Pursh"
The code within 'FreezeCombinedOutput.sas' contains the all analyses performed in SAS 9.4. This was the script of primary use, and has not been parsed out for simple use. This exists for transparency, components of the SAS analyses can be found in the respective 'Height' , 'Survival', and 'Correlation' scripts. Data transformation steps may exist within each step and each requires data files which may be better illustrated within the SAS and R Markdown file.

# R
'SurvAvgs.R' was used to produce the 'by treatment' average height values that would be used in correlations.
'BNCGraphics.R' , 'CumHazRPlots.R' , 'RHeightbyGenotypeGraphs.R' were used to produce the primary figures of Gossweiler et al. 2023. Additionally The RDA analysis and figures were produced using the code within Bioclim.R
'Bioclim.R' contains the code needed to extract the relevant bioclimatic variables for our sample sites of origin, perform the RDA analysis, and produce the relevant figure used in the paper.

#SAS
'Height.sas' contains the mixed model and box-Box for determining Height fixed effects, 'Survival.sas' contains the PHreg procedure for estimating Survival, Hazard Ratios, and Cumulative Hazards, and 'Correlations.sas' produces graphs and tables for the full data set and correlation for each of treatments. Note:  not all treatment are present as some test like -6 and -9 were overwritten. Changing sheet names and .work to match the datatable will include these. 

All data is stored in the accompanying Dryad repository: TBD
Reference the DataRepoNameChanges.md for info on how to rename files for use with these scripts
