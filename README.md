# risk_scoring

This repository contains code used to produce results in "Quantifying SARS-CoV-2 infection risk within the Apple/Google exposure notification framework to inform quarantine recommendations," by AM Wilson, N Aviles, PI Beamer, Z Szabo, KC Ernst, and J Masel, available at: https://www.medrxiv.org/content/10.1101/2020.07.17.20156539v1

# Files for Generating Figures

System Requirements
--------------------
To run this code, you will need R and packages indicated in the code files in "require" or "install" statements and the data files described below. Code for generating figures was run using R version 4.0.2 on Windows 10 Home version 1909 with an Intel(R) Core(TM) i7-8565U CPU @ 1.80GHz 1.99 GHz processor. Typical run time per file for generating figures is roughly less than 15 seconds.


distance_dose_figure_v2.R
---------------------------
This code is needed for generating Figure 2. No data are needed since the data needed for plotting are generated using this code.

code for figures 3 and 4.R
------------------
Code needed for generating Figures 3 and 4 in the manuscript
Data needed for code generation can be found in risk_discount_data.csv and figure4_data.csv

risk_discount_data.csv and figure4_data.csv
-------------------------------------------
Data needed to run the code for figures 3 and 4.R file. The risk_discount_data.csv file contains values used to calculate the discount on risk per day post-exposure where it is adjustable based on the assumed fraction of asymptomatics. The figure4_data.csv contains discounts per day with an assumed negative test on the 4th day post-exposure.

evaluation of transmission risk levels_v2.R
--------------------------------------------
This code is needed for generating Figure 5A. This file requires the compare.risk.level.csv file.

compare.risk.level.csv
---------------------------
This file contains days since symptom onset (column 1), the associated risk value (column 2), and the associated risk level (column 3)


# Files for Attenuation Bin Thresholds and Weight Setting

System Requirements
--------------------
To run this code, you will need R and packages indicated in the code files in "require" or "install" statements and the data files described below. Code for generating figures was run using R version 4.0.2 on Windows 10 Home build 18363 with an Intel(R) Core(TM) i7-9750U CPU @ 2.60GHz processor. Typical run for threshold optimization time takes less than 5 minutes.

AttenuationBinFinder.R
----------------------
Code needed to generate optimal partition points for the ranges of attenuation bins

AttenuationBinFinderNotebook.Rmd
----------------------
Suppliment to AttenuationBinFinder.R; follows the psuedo-data filtering and evaluation step by step.

# Files for Supplemental Materials

System Requirements
---------------------
To run this code, you will need R and packages indicated in the code files in "require" or "install" statements and the data files described below. Code for generating figures was run using R version 4.0.2 on Windows 10 Home build 18363 with an Intel(R) Core(TM) i7-9750U CPU @ 2.60GHz processor. Typical run time per file for generating figures is roughly less than 15 seconds.

fEX_vs_EfX-Infection_Prob.R
---------------------------
Generates graphs comparing the result of additional variance on the convexity of the dose response curve under the parameters for the expected dose model.
