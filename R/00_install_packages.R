# Running this R file will ensure all of the required packages and dependencies are installed.
# Using pak so that installation is parallelised and packages are only installed if they are not already installed.

install.packages("pak")

pak::pkg_install(
    c(
        # standard packages for VSCode
        "languageserver", "jsonlite", "lintr", "styler",
        # packages for thesis
        "here", "tidyverse", "patchwork", "ggforce", "ggpp",
        "broom", "ggnewscale", "knitr", "kableExtra", "makeit",
        "tinytex", "scales", "msm", "survival", "matrixStats",
        "conflicted", "ggridges", "readstata13", "gtsummary",
        "arrow", "finalfit", "flexsurv", "janitor", "epitools",
        "roll", "ggh4x", "gghighlight", "Cairo"
    )
)

# install tinytex
tinytex::install_tinytex()

# command to update all packages using pak
if (length(pkgs <- setdiff(rownames(old.packages()), "pak")) > 0) pak::pkg_install(pkgs)
