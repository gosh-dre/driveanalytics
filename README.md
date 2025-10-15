# driveanalytics - R package containing core PICTURE analytics functionality

[`PICTURE (Personalised Paediatric Informatics ConsulTation using Real-world Evidence)`](https://adc.bmj.com/content/108/Suppl_1/A45.1) is a set of R packages that can be used to analyse data.

For more information or to engage with us, please email [DRESupport@gosh.nhs.uk](mailto:DRESupport@gosh.nhs.uk).

The `driveanalytics` package provides a set of functions for basic analysis that can be applied to 2-dimensional tabular data.

The development was based on datasets that follow the Research Data View (RDV) structure, a GOSH DRE data model.

Currently, it is required that all datasets that are loaded into PICTURE contain an identifier column 'project_id' and event datetime columns '*_datetime'.

Use with the [`picture.platform`]() package to load the interactive PICTURE tool.

## Usage

A PICTURE app is a yaml configuration of an analysis on data loaded to PICTURE.

To run the apps defined in `driveanalytics`, run the following:

``` r
devtools::load_all()
picture.platform::app_picture_run(config_yaml = "./inst/configs/config.yaml")
```

Functions in this package can also be called directly without configuring a PICTURE app or using the other PICTURE packages:

``` r
df_example <- data.frame(
  project_id = c(1, 3),
  cohort = c("A", "B"),
  test = c("test1", "test2"),
  result = c(100, 300)
)
driveanalytics::gen_distribution_plots(df_rdv = df_example,
                                       col = "result",
                                       stratify_by = "cohort")$plot
```

## Package Developer Instructions

### Directory structure for a new module (feature)

-   R and Shiny code
    -   R/name.R
    -   R/name-shiny.R
-   Rmd report code
    -   inst/rmd/name-report.Rmd
-   Testing R code
    -   tests/testthat/test-name.R
-   Example/demo code
    -   vignettes/name.Rmd

To set this up, there is a scaffolding function in `picture.platform`:

``` r
picture.platform::utils_scaffold_new_module(
    "package_code_name",
    "Package normal text name",
    list("df_input", "df_another_input"))
```

## Core Contributors/Developers

-   Stuart Bowyer
-   Jonny Sheldon (Maintainer)
-   Eleni Pissaridou (Maintainer)

## Contributors/Reviewers

-   Alexandros Zenonos

## Contact

To contact the team please email [DRESupport\@gosh.nhs.uk](mailto:DRESupport@gosh.nhs.uk){.email}

## Acknowledgements

-   Great Ormond Street Hospital (GOSH) Data Research, Innovation and Virtual Environments unit (DRIVE)

## Disclaimer

DISCLAIMER – this openly published repository represents a “major version” snapshot of a project that is not actively developed on Github. Therefore, anyone using all or part of this repository must take responsibility for ensuring the use of up-to-date secure versions of relevant packages and code interpreters when running the code. As stated in the license, the software is provided “as is” without any kind of warranty. For more information, please refer to the license file in the repository.

## Licenses

All code is released under the [MIT License][mit]. All documentation is [© Crown copyright][copyright] (2025) and available under the terms
of the [Open Government License 3.0][ogl].

[mit]: LICENSE
[copyright]: http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/
[ogl]: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
