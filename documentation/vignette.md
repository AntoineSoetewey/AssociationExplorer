---
title: "Getting Started with AssociationExplorer"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with AssociationExplorer}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# AssociationExplorer

**AssociationExplorer** is an interactive R Shiny application designed to help non-technical users explore associations within multivariate datasets using visual and statistical tools. This vignette provides a guided walkthrough of its use and core functionality.

---

## Installation and Launch

To use AssociationExplorer, you need R and internet access. The app is hosted on GitHub and can be launched directly from your R console using:

```r
if (!require("shiny")) install.packages("shiny")
shiny::runGitHub("AssociationExplorer", "AntoineSoetewey")
```

Ensure that all required dependencies are installed. The full list is available in the `DESCRIPTION` file of the repository.

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
   - Filter by variable type (quantitative, categorical, etc.).  
   - See summaries and descriptions (if provided).

3. **Association Filtering**  
   - The app calculates association measures depending on variable types:
     - **Pearson’s r** for numeric pairs.
     - **Eta squared** for numeric–categorical pairs.
     - **Cramér’s V** for categorical pairs.
   - Users set a minimum threshold to retain only strong associations.

4. **Correlation Network (`Correlation Network` tab)**  
   - An interactive network plot visualizes filtered associations.  
   - Edge thickness and length represent association strength.  
   - For numeric pairs, color encodes direction: red = negative, blue = positive.  
   - Hover to see variable names or descriptions (if available).

5. **Detailed Bivariate Plots (`Pairs Plots` tab)**  
   - Visual summaries for retained associations:
     - Scatter plots (with linear regression line, no CI).
     - Mean plots (ordered bars, no SE bars).
     - Colored contingency tables.
   - Designed for clarity and readability by non-experts.

---

## Example Dataset: European Social Survey (ESS)

An illustrative example is included based on a curated subset of the **European Social Survey (ESS11)** dataset, filtered for Belgian respondents and selected variables relevant to the ODALON project.

- Raw ESS data and codebook: [https://ess.sikt.no/en/](https://ess.sikt.no/en/)
- Data curation script and example dataset: [`shiny_app/data/`](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/shiny_app/data)

**Note**: Weights are not applied in this example to maintain interface responsiveness and pedagogical simplicity. Support for user-specified weights is planned in a future release.

---

## Planned Extensions

Future enhancements include:

- Support for survey weights.
- Partial correlation analysis.
- 3-variable visualizations (e.g., scatter plots with size or color mapped to a third variable).
- AI-based variable suggestion: users enter a prompt or theme and relevant variables are suggested via a large language model.
- Exportable reports and automatic summaries.
- Longitudinal and panel data support.

---

## Citation

If you use AssociationExplorer in your work, please cite the associated publication (available soon via SoftwareX) and the ESS dataset:

- European Social Survey European Research Infrastructure (ESS ERIC). (2024). *ESS11 integrated file, edition 3.0* [Data set]. Sikt. https://doi.org/10.21338/ess11e03_0  
- European Social Survey European Research Infrastructure (ESS ERIC). (2024). *ESS11 Data Documentation*. Sikt. https://doi.org/10.21338/ess11-2023

---

## License

This project is released under the MIT License.

---

## Contact

For questions, suggestions, or contributions, feel free to open an issue on the [GitHub repository](https://github.com/AntoineSoetewey/AssociationExplorer).
