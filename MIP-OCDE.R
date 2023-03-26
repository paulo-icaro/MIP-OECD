# ------------------------------------------- #
# --- Input-Output Tables - OECD Contries --- #
# ------------------------------------------- #

# Packages
install.packages('OECD')

# Libraries
library(OECD)     # OECD API



# Data Extraction
get_dataset("IOTS_2021",
            filter = list(c("TTL"), c('BRA')),
            start_time = 1998, end_time = 1998)

