# BSE (Best Scientific Estimates)

This R package is mainly used to prepare purse-seine catch and length composition data for tropical tunas in the eastern Pacific Ocean. Cleridy Lennert-Cody wrote most of the key functions and Haikun Xu wrapped them up into a R package. The R functions Cleridy built for the assessments up to SAC-13 are stored in version 1.0.0 (`devtools::install_github('HaikunXu/BSERegressionTree', ref = 'v1.0.0')`). Additional improvement since SAC-13 will be updated in future versions. A document recording the equations used in the method can be found [here](https://github.com/HaikunXu/BSE/blob/main/note/BET-02-06_Summary-of-purse-seine-data-for-bigeye-tuna-in-the-eastern-Pacific-Ocean.pdf).

### How to use this package to extract purse-seine data for the stock assessments of tropical tunas in the eastern Pacific Ocean?

1.  **Download raw data from the IATTC database**

    The data are extracted using the Visual Basic program called ‘DataExtraction.exe’, which can be found in this folder on the Y drive: Y:\\Development\\Executable. Nick Vogel is the person who wrote and maintains these extraction programs. There are three sub-programs to run:

    ‘Length frequency II (May 16 2008)’, which is located under the tab ‘Cleridy data files’ (this program produces the files called LengthFreqyyyy.txt and LengthMMyyyy.txt, where yyyy corresponds to year or years).

    ‘Get prorated UnLoad (June 11 2014)’, which is located under tab ‘Cleridy data files’ (this program produces the file called Unloadingyyyy.txt). Request the "Frozen" option.

    ‘CAE data’, which is located under the tab ‘Data dump files’ (this program produces the file called CAE-LatLonyyyy.txt, where the program should be run requesting the “CAE table” (not the “frozen” option – but try to run this extraction right after the database has been ‘frozen’); and longtitude/longitude, not EEZ.

    *\*\*\* All current raw data (1975-2021) are already available at P:\\hkxu\\BSE \*\*\**

2.  **Install the BSE package**

    `library(devtools)`

    `devtools::install_github('HaikunXu/BSE', ref = 'main')`

3.  **If any fishery definition for the three tropical tunas is changed:**

    Modify the species-specific fishery (create.fishery.flg.f) and strata (create.strat.flg.f) definition functions and the associated area substitution matrices. Also, edit the code that is the last resort for finding a substitution (substitution matrices). Please contact Haikun Xu (hkxu\@iattc.org) if you need help to update the fishery definition in this package's functions for your assessment.

4.  **Run species-specific data extraction R markdown code that can be found [here](https://github.com/HaikunXu/BSE/tree/main/manual)**

    Since data sources and consequently data extraction functions are different before and after 2000, the data extraction R markdown code is written separately for 1975-1999 and 2000 onward. The data extraction code extracts fishery-specific catch and length composition for user-specified species and time period, saves them as csv files in user-specified directory, and generates a Word file to document the whole process.

5.  **Run species-specific data formatting R markdown code that can be found [here](https://github.com/HaikunXu/BSE/tree/main/manual)**

    For the final step, run the data formatting R markdown code (BET/YFT/SKJ_for_SS.Rmd) to edit the saved catch and length composition csv files from the previous step into the Stock Synthesis format.
