This is a simple R Shiny application designed to explore the properties of the data sources provided.

- The inputs required for this app are the following:
  - Study name
  - 2 input files of TSV format

- The tabs in the tab panel show: 
  - The merged file of interest (in this case, lab values and patient data)
  - Visualization 1: Exploratory bar plots of columns/variables of merged file 
    - Ability to select which columns/variables (minimum 2) to plot 
    - Additional ability to download plots created as a PNG 
  - Visualization 2: Correlation matrix of all columns/variables of merged file
    - Helps to see if there are any relationships among variables in merged file

There is also the capability to download the new merged (or combined) file as a TSV. 
Another capability of this RShiny app is to download a data report, using the Data Reporter package, to further examine the relationships of variables presented in the inputs files. 

Thank you for your time and consideration for this opportunity. I really enjoyed being able to put what I have learned about RShiny apps from my current co-op to this RShiny exercise. Looking forward to hearing from you! 
