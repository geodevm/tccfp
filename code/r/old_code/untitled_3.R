# Packages
library(here)
library(tidyverse)
library(lubridate)

# Load data
biologicals <- read.csv(here("data/biologicals.csv"))

summary(biologicals$weight[biologicals$species == "red fox" & biologicals$sex == "M"])
summary(biologicals$weight[biologicals$species == "red fox" & biologicals$sex == "F"])
summary(biologicals$weight[biologicals$species == "coyote" & biologicals$sex == "M"])
summary(biologicals$weight[biologicals$species == "coyote" & biologicals$sex == "F"])

        