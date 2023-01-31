# Respository RESPOND-SPA

This repository contains the code of the analytic code for the RESPOND
randomised controlled trial conducted on healthcare workers in Spain.
Pseudonymised data will only be shared within the RESPOND Consortium, in
compliance with the GDPR. Deidentified individual participant data (i.e.,
anonymised dataset) supporting the primary analysis of the trial is available
upon reasonable request at the Dataverse repository
(<https://doi.org/10.21950/HN1HNO>), beginning 3 months and ending 5 years
following article publication.

More information about RESPOND: <https://www.respond-project.eu>

# Project installation

## Software components

-   Install [R version
    4.2.2](https://cran.rstudio.com/bin/windows/base/old/4.2.2/): In Windows,
    using the [binary
    installer](https://cran.rstudio.com/bin/windows/base/R-4.2.2-win.exe) is
    recommended.

<!-- -->

-   [Rstudio
    Desktop](https://www.rstudio.com/products/rstudio/download/#download):
    Although not strictly necessary, it is recommended to install the Rstudio
    IDE; for strict reproducibility, use build [2022.07.1+554 for Windows
    10/11](https://download1.rstudio.org/desktop/windows/RStudio-2022.07.2-576.exe).

<!-- -->

-   [Git client](https://git-scm.com/download): Install the Git client in order
    to be able to clone locally the project repository. On Windows, use [the
    64-bit Windows
    installer](https://github.com/git-for-windows/git/releases/download/v2.38.1.windows.1/Git-2.38.1-64-bit.exe).

## Installing the project locally

This project is hosted as a GitHub repository. It can be cloned as a local Git
repository following [these
instructions](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2)
(steps 2 through 7). Note that this will create a local copy of ('clone') the
GitHub repository as an Rstudio project in the folder specified. The URL that
must be entered into the `Repository URL` text box is:

    https://github.com/CCOMS-UAM/respond-spa

After cloning the repository, the Rstudio project will open automatically in the
Rstudio IDE. If it doesn't, or you want to return later to the project in
Rstudio, you can do so by double clicking on the file `rstudio_project.Rproj`
that has been created in the project folder when cloning the repository.

**NOTE:** It is common practice to avoid using and versioning `.Rprofile` files.
However, this project uses [package
`renv`](https://cran.r-project.org/package=renv) to create a reproducible
environment, which needs the `.Rprofile` file that lives in the root directory
of the project. **Please DO NOT delete or edit this file**; it will install and
activate the `renv` package and make it ready for restoring the environment.

## Restoring the environment

The reproducible environment created by `renv` must be restored to install all
the packages this project needs to be built properly. Use the "renv" =\>
"Restore library..." button in Rstudio's "Packages" tab to restore the
environment. Alternatively, you can type in the console:

``` r
renv::restore()
```

# Repository structure

The file structure of this repository is as follows:

    <repository-folder>
    |
    |--- dat       (To store input datasets; must NEVER be checked-in to Github)
    |
    |--- doc       (To store important documentation of the project)
    |    |
    |    |--- minutes (To store meeting minutes)
    |
    |--- notebooks (Notebooks to explore data and test processes live here)
    |
    |--- output    (Processing outputs; files must be individually "checked-in" if necessary)
    |
    |--- R         (R functions created for this project live here)
    |
    |--- renv      (System library necesssary for `renv` to work. DON'T TOUCH)
    |
    |--- src       (Source scripts that implement the main processes)
    |
    |--- www       (Project assets, e.g., images, bibliography files, etc.)

Use the folders as indicated to store the different files and generate the
outputs of the processes.
