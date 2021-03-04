# CAISO Data Analysis
This directory scrapes and analyzes hourly electrical usage data from the California Independent Systems Operator (CAISO).

**./CAISO_Analysis.Rmd** contains all code and analysis. It is is a heavily annotated file, so is mostly self explanatory.

**./CAISO_Analysis.html** is the formatted/published version of **./CAISO_Analysis.Rmd**.

**./index.html** is a copy of **./CAISO_Analysis.html**, renamed such that GitHub Pages recognizes and displays it [here](https://noahkouchekinia.github.io/CAISO/).

**./publish.R** a utility script to process  **./CAISO_Analysis.Rmd** into **./CAISO_Analysis.html** and **./index.html**.

**./Data/** contains data used in the analysis. All data is scrapped and deposited here by **./CAISO_Analysis.Rmd**.
