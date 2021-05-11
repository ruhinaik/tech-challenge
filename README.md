This is a simple R Shiny application designed to explore the properties of the data sources provided.

- The inputs required for this app are the following:
  - Study name
  - 2 input files of TSV format

- The tabs in the tab panel show: 
  - The merged file of interest (in this case, lab values and patient data)
  - Visualization 1: Exploratory bar plots of columns/variables of merged file 
    - Ability to select which columns/variables (minimum 2) to plot 
  - Visualization 2: Correlation matrix of all columns/variables of merged file, helping to see if there are any relationships among variables 
