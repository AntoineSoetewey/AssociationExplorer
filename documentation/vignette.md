# AssociationExplorer

**AssociationExplorer** is an interactive R Shiny application designed to help non-technical users explore associations within multivariate datasets using visual and statistical tools. This vignette provides a guided walkthrough of its use and core functionality.

---

## Installation and Launch

To use AssociationExplorer, you need R and internet access. The app is hosted on GitHub and can be launched directly from your R console using:

```r
if (!require("shiny")) install.packages("shiny")
shiny::runGitHub("AssociationExplorer", "AntoineSoetewey")
```

Ensure that all required dependencies are installed. The full list is available in the [packages.md file](https://github.com/AntoineSoetewey/AssociationExplorer/blob/main/packages.md) of the repository.

---

## Overview of the Workflow

AssociationExplorer is structured into sequential tabs to guide users through a complete analysis pipeline:

1. **Data Upload (`Data` tab)**  
   - Upload your dataset in `.csv` or `.xlsx` format.  
   - Optionally upload a *description file* with two columns:
     - `Variable`: exact name matching dataset variables.
     - `Description`: human-readable label for interpretation.

   Variables with only one unique value are automatically removed.

2. **Variable Selection (`Variables` tab)**  
   - Choose which variables to include in the analysis.
   - If a description file is provided, see the list of included variables together with the descriptions.

3. **Association Filtering**  
   - The app calculates association measures depending on variable types:
     - **Pearson’s r** for numeric pairs.
     - **Eta** for numeric–categorical pairs.
     - **Cramer’s V** for categorical pairs.
   - Users set a minimum threshold to retain only strong associations.

4. **Correlation Network (`Correlation Network` tab)**  
   - An interactive network plot visualizes filtered associations.  
   - Edge thickness and length represent association strength: 
     - Thicker (thinner) edges = stronger (weaker) associations.
     - Shorter (longer) edges = stronger (weaker) associations.  
   - For numeric pairs, color of the edges indicates direction of the association: red = negative association, blue = positive association.  
   - Hover over the nodes to see variable names or descriptions (if a description file is provided).

5. **Detailed Bivariate Plots (`Pairs Plots` tab)**  
   - Visual summaries for retained associations:
     - Scatter plots (with linear regression line) for numeric pairs.
     - Mean plots (ordered bars) for numeric–categorical pairs.
     - Colored contingency tables for categorical pairs.

---

## Example Dataset: European Social Survey (ESS)

An illustrative example is included based on a subset of the **European Social Survey (ESS11)** dataset, filtered for Belgian respondents and curated for demonstration purposes.

- Raw ESS data and codebook: [https://ess.sikt.no/en/](https://ess.sikt.no/en/)
- Data curation script for the example dataset: [data folder](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/data)

If you use the dataset used in the example, please cite the following:

- European Social Survey European Research Infrastructure (ESS ERIC). (2024). ESS11 integrated file, edition 3.0 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e03_0

---

## Contact

For questions, suggestions, or contributions, feel free to [open an issue](https://github.com/AntoineSoetewey/AssociationExplorer/issues) on the GitHub repository.
