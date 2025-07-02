# AssociationExplorer: A user-friendly Shiny application for exploring associations and visual patterns


See the paper: https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/paper


**AssociationExplorer** is a user-friendly, open-source R Shiny application that helps non-technical users explore statistical associations within multivariate datasets. It is especially designed for journalists, educators, and engaged citizens interested in uncovering meaningful patterns in survey or public data without requiring programming or statistical expertise.

## Features

- Interactive upload of CSV or Excel datasets
- Optional upload of a variable description file
- Automatic handling of quantitative and qualitative variables
- Computation of association measures: Pearson's *r*, Eta, and Cramer's V
- Dynamic filtering of associations by user-defined thresholds
- Interactive correlation network visualization
- Contextual bivariate plots: scatter plots, mean plots, and contingency tables
- Clean, modern UI with responsive behavior

## Getting Started

You can launch the app directly from R using:

```r
library(shiny)
shiny::runGitHub("AssociationExplorer", "AntoineSoetewey")
```

You will need the `shiny` package and an active internet connection. A fully web-based version will be made publicly available as part of the ODALON project.

You will need the shiny package and an active internet connection. A fully web-based version will be made publicly available as part of the ODALON project.

## Documentation

Full documentation, including variable formats, data requirements, and example workflows, is available in the [docs/ folder](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/documentation) of this repository.

## Example Dataset

The repository includes an illustrative example based on the European Social Survey, restricted to Belgian respondents and curated for demonstration purposes. The code used to generate the curated dataset can be found in shiny_app/data.

## License

This project is licensed under the MIT License.

## Citation

If you use this software, please cite the following:

Soetewey, A., Heuchenne, C., Claes, A. and Descampe, A. (2025). AssociationExplorer: A user-friendly Shiny application for exploring associations and visual patterns [Software]. https://github.com/AntoineSoetewey/AssociationExplorer

For the dataset used in the example, cite:

European Social Survey European Research Infrastructure (ESS ERIC). (2024). ESS11 integrated file, edition 3.0 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e03_0