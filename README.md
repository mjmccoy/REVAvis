# REVAvis
Visualization Software for REVAvis

## Install dependencies
If using conda install, you can install R and Rstudio with the following:
```
conda create -n REVAvis
conda activate REVAvis
conda install -c r r
conda install -c r rstudio 
```
Then, in R, install dependencies:
```
install.packages(c("shiny", "waiter", "ggplot2", "dplyr", "ggrepel"))
```

If (and only if!) there was an error message at the end of this, try the following:
```
install.packages("Rcpp", type = 'source')
install.packages(c("shiny", "waiter", "ggplot2", "dplyr", "ggrepel"))
```

## Install REVAvis
Clone REVAvis from GitHub:
```
git clone https://github.com/mjmccoy/REVAvis.git
```

Checkout the correct branch. Here's an example for the 'annotations' branch:
```
git checkout annotations
```

If using ssh -X, REVAvis can be run with the following:
```
Rscript -e 'library(methods); shiny::runApp("inst/application", launch.browser=TRUE)'
```
