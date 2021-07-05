# Coursera Johns Hopkins Data Science Capstone Milestone Report and Final Project


This repository contains files that served as deliverables for the Johns Hopkins Data Science Capstone Milestone Report and Final Project on Coursera.

The general steps to follow to recreate the analysis are:

1. Download the entire repo.

2. Install the needed libraries.

3. Knit the MilestoneReport.Rmd file. This produces a report that provides and overview of the initial analysis on the data set.

4. Run the PredictionScript.R file. This cleans the data set through various methods (gsub/REGEX and tm_map), creates a quanteda corpus, and then uses the sbo package to created a trained corpus to facilitate prediction. The trained corpus is saved to the "corpus_trained.rda" file.

    * You can modify the prediction files through various means (modifying cleaning, modifying the sample size).
    
5. The Shiny App is simple. Its primary function is to load the trained corpus (corpus_trained.rda) and then accept input through a text box that queries the trained corpus. There are a few simple commands to prevent empty response errors and deal with inputs that don't have a strong prediction. You can adjust this behavior by modifying the if statements from lines 46-67.

    * If you have created your own trained corpus, replace the one that resides in the Shiny App folder.
    
6. The Final Pitch slides provide a brief overview of the App and how it works using the ioslides presentation format.