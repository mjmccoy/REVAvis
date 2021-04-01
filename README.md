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
install.packages(c("shiny", "waiter", "ggplot2", "dplyr"))
```

If (and only if!) there was an error message at the end of this, try the following:
```
install.packages("Rcpp", type = 'source')
install.packages(c("shiny", "waiter", "ggplot2", "dplyr"))
```

## Install REVAvis
Clone REVAvis from GitHub:
get clone https://github.com/mjmccoy/REVAvis.git

Alternatively, download REVAvis binary:
https://github.com/mjmccoy/REVAvis/blob/master/REVAvis_0.0.0.9000_R_x86_64-pc-linux-gnu.tar.gz

Then install package from source:
```install.packages("REVAvis_0.0.0.9000_R_x86_64-pc-linux-gnu.tar.gz", repos = NULL, type="source")```

Checkout the correct branch. Here's an example for the 'annotations' branch:
```git checkout annotations```

If using ssh -X, REVAvis can be run with the following:
``` Rscript -e 'library(methods); shiny::runApp("inst/application", launch.browser=TRUE)'```
