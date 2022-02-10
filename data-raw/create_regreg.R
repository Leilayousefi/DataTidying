# some code to go from raw data csv to a nice RDA object


# create_regreg

regreg <- readr::read_csv("./data-raw/register.csv")

# as factor
regreg$phase <- forcats::as_factor(regreg$phase)

#give nice name
regreg <- dplyr::rename(regreg, date = `entry-timestamp`)

# copyright looks empty
regreg <- dplyr::select(regreg, -copyright)

# overwrite old data
usethis::use_data(regreg, overwrite = TRUE)

rm(regreg)

