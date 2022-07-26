<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# \# biodiversity-detailed-assessment

### Data

The analysis uses data from several sources. Certain datasets need to be
manually downloaded prior to starting processing. Information about
where datasets can be found is in `dataset-info.txt`.

### Targets Workflow

This project leverages the `targets` package, a pipeline toolkit for
data science projects in R. You can install `targets` from CRAN:

``` r
#install.packages("targets")
```

\#Usage Run `targets::tar_make()` to run project. This will run all of
the analysis - no individual scripts are required.

### Required R packages

The packages used in this analysis are catalogued in `packages.R`. The
packages will be loaded automatically with `tar_make()` but some may
need to be installed prior to initiating the workflow.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/protected-lands-and-waters-indicator/issues).

## How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting
BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).
Click [here](https://github.com/bcgov/EnvReportBC) for a complete list
of our repositories on GitHub.
