# Reduced, Reused, and Recycled: The Life of a Machine Learning Dataset
## NeurIPS 2021
### Best Paper NeurIPS Datasets and Benchmarks Track
This repository contains the [datasheet](Datasheet_for_ReducedReusedRecycled.pdf), data, and code to reproduce all the analyses in the paper.
I'm stil organizing things a bit, but if you need something immediately or find it confusing, please open a GitHub issue or email me. I recommend reading the paper, appendix, and datasheet (in that order) thoroughly before sifting through the code.

![](NeurIPS2021Poster.jpg?raw=true)

The Github is organized as follows:

**"Analysis"**: This folder contains 3 Rscripts to reproduce the analyses and figures in the paper. These R scripts should save the figures to the "Figures" or "Appendix" folders as appropriate.

Most of the things you are looking for are in the "Data" folder. 

**Dataset_Curation:** You'll find the main notebook to clean ad curate the data for the whole project "MainDataset.ipynb."

**Dataset_Curation/Data:** You'll find four json files that correspond to the raw data from MAG and PWC. The PWC files are 06/16/21 downloads from [here](https://github.com/paperswithcode/paperswithcode-data). I will try and add some code to show how I got the MAG ID's for the papers in "datasets.json" but given that MAG is offline, I'm not sure it's that helpful.

**Dataset_Curation/Data/Derivative Datasets:** Derivative and intermediary datasets that are created throughout the cleaning process of MainDataset.ipynb are saved here. Some of these datasets take a long time to generate so it's helpful to save them as we go.

**Dataset_Curation/Data/Analysis_Datasets:** The final datasets created by MainDataset.ipyb drawn on by the R scripts in the "Analysis" Folder are saved here.

**Dataset_Curation/Data/Sensitivity_Datasets:**  Datasets created by MainDataset.ipyb drawn on by the R scripts in the "Analysis" Folder are saved here.
