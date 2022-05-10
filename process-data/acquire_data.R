options("icpsr_email" = "",
        "icpsr_password" = "")

options(cache = TRUE, warning = FALSE,  message = FALSE, cache.lazy = FALSE)

suppressMessages(library(icpsrdata))
suppressMessages(library(tidyverse))
suppressMessages(library(here))

# ----

# LEAIC
cat('\n-- Download LEAIC\n')
leaic_items <- c(35158, 4634)

leaic_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))

# NCVS
cat('\n-- Download NCVS\n')
ncvs_items <- c(38136#, 
                #37198
                )

ncvs_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))

# NCVS year data
# ncvs_items <- c(
#   # add remaining files manually
#   25461, # 2008
#   28543,
#   31202, # 2010
#   34061,
#   34650,
#   35164,
#   36142, # 2014
#   36448, # 2015
#   37296 # 2016
# )

# ncvs_items %>%
#   walk(~ .x %>% icpsr_download(
#     file_id = .,
#     download_dir = here("downloads")
#   ))




# study numbers for NIBRS from icpsr
cat('\n-- Download NIBRS\n')
nibrs_items <- c(
  # 4292, # 2003
  # 4468, # 2004
  # 4720, # 2005
  22407, # 2006
  25113, # 2007
  27647, # 2008
  30770, # 2009
  33530, # 2010
  34585, # 2011
  35035, # 2012
  36120, # 2013
  36398, # 2014
  36795#, # 2015
  # 37065 # 2016
)

nibrs_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))

# study numbers for leoka from icpsr
cat('\n-- Download LEOKA\n')
leoka_items <- c(
  # 4269, # 2003
  # 4462, # 2004
  # 4719, # 2005
  22402, # 2006
  25104, # 2007
  27646, # 2008
  30765, # 2009
  33525, # 2010
  34584, # 2011
  35020, # 2012
  36119, # 2013
  36395, # 2014
  36791, # 2015
  37062 # 2016
)

leoka_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))





