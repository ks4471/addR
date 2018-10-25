## addR
Collection of quality-of-life functions and wrappers for a number of Bioinformatics tools, designed to streamline omics data quality control, analysis and visualisation. Functions include tools for principal component analysis, hierarchical clustering, covariate influence estimation, linear modelling and over-representation tests.

Also includes custom functions to perform gene set enrichment tests using datasets from publicly available repositories
- cell type enrichment *celltr()*                              https://github.com/ks4471/celltr
- de-novo mutation enrichment *dnmr()*                         https://github.com/ks4471/dnmr
- CMAP drug database enrichment *cmapr()*                      https://github.com/ks4471/cmapr
- wrapper for *webgestaltR* functional enrichment database     https://github.com/ks4471/webg

Please see the other repositories in on this account for details and instructions on running the above functions


### Installation:
```

devtools::install_github("ks4471/addR")
library(adds)

```


If a requred library is not installed, the functions will try to automatically install libraries they need from bioconductor or CRAN. This may not always work, since some libraries are in github, etc. If that is the case, try the command below to install all such dependencies in one go.
```
install.dependencies()
```

