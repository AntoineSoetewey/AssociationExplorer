# AssociationExplorer: A user-friendly Shiny application for exploring associations and visual patterns

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
runGitHub("AssociationExplorer", "AntoineSoetewey")
```

You will need the `shiny` package and an active internet connection.

## Documentation

Full documentation, including how to use the app, variable formats, data requirements, and example workflows, is available in the [docs/ folder](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/documentation) of this repository.

An accompanying paper can be found in the [paper/ folder](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/paper), which provides a detailed overview of the application, its features, and its intended use cases.

## Example Dataset

The repository includes an illustrative example based on the European Social Survey, restricted to Belgian respondents and curated for demonstration purposes. The code used to generate the curated dataset can be found in the [`data/` folder](https://github.com/AntoineSoetewey/AssociationExplorer/tree/main/data).

If you use the dataset used in the example, please cite the following:

- European Social Survey European Research Infrastructure (ESS ERIC). (2024). ESS11 integrated file, edition 3.0 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e03_0

## License

This project is licensed under the MIT License.

## Contact

For questions, suggestions, or contributions, feel free to [open an issue](https://github.com/AntoineSoetewey/AssociationExplorer/issues) on the GitHub repository.
