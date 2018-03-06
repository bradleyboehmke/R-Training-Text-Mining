###############################
# Setting Up Your Environment #
###############################
# The following packages will be used so please install PRIOR to class. To 
# install the ones you need first run this list.of.packages code.
list.of.packages <- c(
  "tidyverse",      # provides dplyr, tidyr, purrr, ggplot, etc for common data analysis tasks used throughout the class
  "nycflights13",   # provides example data sets for day 1
  "devtools",       # allows you to download the harrypotter package
  "tidytext",       # provides efficient text mining and cleaning capabilities
  "magrittr",       # provides a variety of pipe operators
  "sentimentr",     # provides sentiment analysis by capturing valence shifters
  "tm",             # document term matrix and word association
  "widyr",          # computes word association
  "igraph",         # creates word networks
  "ggraph",         # creates word networks
  "factoextra",     # cluster analysis
  "skmeans",        # cluster analysis
  "cluster",        # cluster analysis
  "clue",           # cluster analysis
  "topicmodels",    # topic modeling
  "ldatuning"       # topic modeling
)

# Next, run the following line of code to install the packages you currently do not have
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

####################################
# Downloading Harry Potter Package #
####################################
# We will also use a package I developed that contains the Harry Potter series.
# Running the following lines of code will download this package from my Github
# repository
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("bradleyboehmke/harrypotter")








