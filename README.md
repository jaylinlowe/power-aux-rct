# Data Build and Analysis for Power Calculations for Randomized Controlled Trials with Auxiliary Observational Data

This repository contains the code used to complete the analysis discussed in "Power Calculations for Randomized Controlled Trials for Auxiliary Observational Data". The analysis is also dependent on functions from the `AuxRCT` R package, which can be found at  [github.com/jaylinlowe/AuxRCT](github.com/jaylinlowe/AuxRCT). 

**Contents:**

* `raw-data/`
  * location for raw AEIS data 

* `data/`
  * data products generated with scripts 
  * `temp/`
    * temporary files used for data build
  * `xwalk/` 
    * files used to create keys for grade types 
* `scripts/` 
  * `010-make-data.R`
    * combines data from `raw-data/`
    * outputs `data/HS_MS.Rdata`
    * creates covariate data for the years 2003-2008 and outcome data for 2008-2009 for auxiliary schools in Texas
* `analysis/`
  * `cta_analysis` 
    * reproduces analysis in the paper using data files 
    * relies on `AuxRCT` R package 
    
**Sources:**

* Texas [AEIS](https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/DownloadData.html) (publicly available)
   * data [reference](https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/xplore/aeisref.html)

**Notes on data build:**

* The AEIS data for the 2003-4 and 2004-5 school years does not have any column names, although there are format files which contain labels and descriptions for the columns. Therefore, we follow the following naming convention: `[data-file-name]_[column-number]_[data-year]`. For example, the 5th column in the 2003-4 campus financial dataset `y34/cfin3.dat` is labeled `cfin3_5_34`.
* All covariates from the datasets in the `y34/` ... `y78/` folders are named with a suffix that aligns with the school year because column names can be the same between years. For example, all covariates read in from the .dat files in `y78` have a suffix `_78`.
* There is a considerable amount of missing data. Data is missing for various reasons:
  * For covariates from earlier years, a school didn't exist in the data until later years. 
  * The value is masked because the number of students it represents is too small. 
  * The value was not gathered. 
* A couple of variables are not found in the AEIS data dictionary:
  * `OUTTYPE` is created in the data build and indicates whether only high school (9th grade) "H", only middle school (8th grade) "M", or both "B" outcomes are available for a school 
* '(C)([ABHFE])(009TM)(0[89])(R)' is the regex to pull the TAKS math scores (009TM indicates 9th grade and 0[89] are the years of the scores pulled). A, B, H, F, and E indicate subgroups of students that the passing rate applies to.

