install.packages("tidyverse")
install.packages("geosphere")
install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")

library(tidyverse)
library(ggpattern)
library(readxl)

tripdata <- list.files(path='D:/OneDrive - student.uinsgd.ac.id/Online Class/Google Data Analytics (GDA)/Portofolio/Cyclistic/Data/data_2020-2022') %>%
  lapply(read.csv) %>%
  bind_rows
