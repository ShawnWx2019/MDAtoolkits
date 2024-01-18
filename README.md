# MDAtoolkits

[![R version](https://img.shields.io/badge/R-v4.1.1-salmon)](https://www.r-project.org) ![lifecycle](https://img.shields.io/badge/lifecycle-Experimental-lightcyan) [![license](https://img.shields.io/badge/license-MIT-red)](https://opensource.org/licenses/MIT) [![Myblog](https://img.shields.io/badge/Blog-ShanwLearnBioinfo-purple)](http://www.shawnlearnbioinfo.top/)

# Getting started

# Dependence

```r
if(!require(devtools)){
install.packages("remotes")
install.packages("devtools")
}
## tidymass
remotes::install_gitlab("tidymass/tidymass")

devtools::install_github("SamGG/ropls")
devtools::
```

# Update information

*Jan 18, 2024*

Version：0.0.1 =\> 0.0.2

1. Delete `mda_CTS_kegg`. It was found that the ID conversion results provided by CTS are *unstable*. For example, converting KEGG ID through INCHIKEY yields inconsistent results across multiple attempts. Suggest using `mda_name2kegg`.

2. Added the `ssl.verifypeer` parameter to function `cbf_wrawler` and observed certificate expiration issues on both the CBF and CTS websites. This may result in content retrieval failure. The default parameter is set to `TRUE`. If no results are returned, it is likely a certificate issue. Manually set the `ssl.verifypeer` parameter to `FALSE` in such cases.

3. Old scripts were archived. 

------------------------------------------------------------------------
