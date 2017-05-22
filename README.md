# Filopodyan
An ImageJ/R pipeline for filopodia dynamics analysis

For the accompanying manuscript, see:

Urbancic, V., Butler, R., Richier, B., Peter, M., Mason, J., Holt, C. E., Gallop, J. L. 2017. Filopodyan: An Open-Source Pipeline For The Analysis Of Filopodia. bioRxiv doi: https://doi.org/10.1101/138610

## Fiji plugin for detecting, tracking and quantifying filopodia

The plugin is supplied as a .jar file (**'Filopodyan_.jar'**). Please refer to the associated user guide (**'Filopodyan User Guide.docx'**) for information on plugin installation, instructions for use and practical advice.


## FilopodyanR - downstream analysis of filopodia dynamic properties and fluorescence

To facilitate downstream analysis, we provide our suite of R scripts for downstream analysis of filopodia properties and fluorescence. Familiarity with R is required. Filepaths, imaging parameters and thresholds need adjusting to suit specific needs as indicated in code comments. 

### Filopodia morphodynamics phenotype

**'FilopodyanR_MASTERSCRIPT.R'** coordinates the execution of multiple other scripts for data import and initial processing (Module 1), computation of filopodia properties over time (**'Module 2'**), and comparison of phenotypes (**'Module 3'**). Currently the scripts support comparing filopodia from two conditions (input as separate folders). **'ColourSchemes.R'** and **'GraphingTemplates.R'** are dependencies for Module 3.

### Exploring parameter correlations 

For exploring relationships between parameters using correlation matrices, **'FilopodyanR Correlations_DataInput.R'** performs data import and processing (analogous to Masterscript and the associated modules), and **'FilopodyanR Correlations.R'** performs matrix computation and visualization. This analysis does not handle different conditions (i.e. single folder input).

### Base fluorescence during filopodia formation

Analysis of base fluorescence during filopodia initiation is performed with **“FilopodyanR BaseF.R”**. It supports analysis of two channels from the same dataset. Various options are provided for background subtraction (**'Module 1-2'**). Based on our experience we generally recommend subtracting boundary background, and normalizing to body fluorescence (also background-subtracted using boundary background).

### Tip fluorescence in filopodia tip extension

Cross-correlation analysis between tip fluorescence intensity and filopodia tip movement is performed by **'FilopodyanR CCF.R'**. Code for further analysis of this relationship and how it differs between filopodia within a dataset is provided in three additional modules: **'…CCF_subcluster-analysis.R'**, **'…CCF_Randomisations.R'** and **'…MarkovChains.R'**.
