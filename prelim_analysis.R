library(readr)

options(device = "CairoWin")

survey_df <- read_csv("Citizen initiative.csv")
survey_df

header_lookup <- read_delim("header_lookup.txt", delim = ";")
names(header_lookup)


names(survey_df) <- header_lookup$df_header

survey_df

