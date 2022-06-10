# BSE (Best Scientific Estimates)

This R package is mainly used to prepare purse-seine catch and length composition data for tropical tunas in the eastern Pacific Ocean. Cleridy Lennart-Cody wrote most of the key functions and Haikun Xu wrapped them up into a R package.

How to use this package to extract purse-seine data for the stock assessments of tropical tunas in the eastern Pacific Ocean?

1.  Download raw data from the IATTC database

    A description of how to download raw data using existing SQL code can be found [here](https://github.com/HaikunXu/BSE/blob/main/manual/Extract%20raw%20data.pdf)

2.  Install the BSE package

    devtools::install_github('HaikunXu/BSE',ref='main')

3.  If any fishery definition for the three tropical tunas is changed, modify the species-specific fishery definition function and area substitution matrices. A description of how to change the code for new fishery definition can be found [here](https://github.com/HaikunXu/BSE/blob/main/manual/New_Fishery_Definition.txt). Please contact Haikun Xu (hkxu\@iattc.org) if you need help to update the fishery definition in this package's functions for your assessment.

4.  Run species-specific data extraction R markdown code that can be found [here](https://github.com/HaikunXu/BSE/tree/main/manual)

    Since data sources and consequently data extraction functions are different before and after 2000, the data extraction R markdown code is written separately for 1975-1999 and 2000 onward. The data extraction code extracts fishery-specific catch and length composition for user-specified species and time period, saves them as csv files in user-specified directory, and generates a Word file to document the whole process.

5.  Run species-specific data formatting R markdown code that can be found [here](https://github.com/HaikunXu/BSE/tree/main/manual)

    For the final step, run the data formatting R markdown code (BET/YFT/SKJ_for_SS.Rmd) to edit the saved catch and length composition csv files from the previous step into the Stock Synthesis format.
