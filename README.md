# Social Account Matrix for France

## Presentation

**samfr** is a small package that use the synthetic tables provided by
the INSEE since 1949 to construct a SAM for a France with a high level
of customization.

The main function is `sam`, which construct the whole SAM for a given
year. The blocks of the SAM are built using the `stbl` function, which
extract the required elements of the TEE and of the TES.

Well formated tables are obtained using the `xtbl` function which
transform wider the initial table.

The package use the **tinytable** package to obtain markdown tables. 

# Installation

The **samfr** package is only available on **github**. To install it,
first install the **remotes** packaget and then write in the consol:

```
remotes::install_github("ycroissant/samfr", build-vignettes = TRUE)
```

The features of the package are described in a vignette that can be
obtained as a pdf file:

```
vignette("samfr", package = "samfr")
```

