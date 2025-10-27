# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(here)
library(tbeptools)

#Setup========================================================================
sgs_keep <- c("FLUCCSCODE", "acres", "areas", "mngacre")
tbni_keep <- c("TBNI_Score", "ScoreNumTaxa", "ScoreShannon", "ScoreTaxaSelect", 
"ScoreTaxaBenthic", "ScoreNumGuilds")

#Import data==================================================================
# TBNI data
# Download the file
download.file(
  url = "https://raw.githubusercontent.com/username/repo/main/data/TampaBay_NektonIndexData.csv",
  destfile = here("Data/TampaBay_NektonIndexData.csv")
)
# Read it in
tbni <- read.csv("Data/TampaBay_NektonIndexData.csv")
arrange(reference)

# Seagrass data
# Download the file
download.file(
  url = "https://github.com/tbep-tech/fim-seagrass/raw/refs/heads/main/data/fimsgdat.RData",
  destfile = here("Data/fimsgdat.RData")
)

seagrass <- fimsgdat %>%
  arrange(reference)

# MERGE FIM COMMUNITY DATA WITH TBNI AND SEAGRASS DATA============================================

tbni2 <- fld2 %>%
  left_join(tbni, by = "reference") %>%
  left_join(seagrass, by = "reference")

# Check for missing TBNI scores
tbni_ck <- tbni2 %>%
  group_by(reference) %>%
  slice(1) %>%
  filter(is.na(TBNI_Score))

# Export TBNI check
write_csv(tbni_ck, paste0(out, "../Checks/tbni_check.csv"))

# Check for missing seagrass management area data
sg_chk <- tbni2 %>%
  group_by(reference) %>%
  slice(1) %>%
  filter(is.na(sgyear)) %>%
  select(reference, latitude, longitude, year, sgyear, acres, mngacre, 
         FLUCCSCODE, month, gear, TB_seg, season, TBNI_Score, areas)

# Export seagrass check
write_csv(sg_chk, paste0(out, "../Checks/sg_chk.csv"))

# Final field dataset with TBNI and seagrass management zones
fld <- tbni2 %>%
  filter(!is.na(TBNI_Score), !is.na(sgyear))

# Select variables to export
fld_exp <- fld %>%
  select(all_of(c(var0, var1, var2)[c(var0, var1, var2) %in% names(fld)]))

# Export combined field, TBNI, and seagrass data
write_csv(fld_exp, paste0(out, "phy_tbni_sgrs.csv"))
