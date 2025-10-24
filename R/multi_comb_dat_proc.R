# =============================================================================
# Program: multi_comb_dat_proc.R
# Programmer: Converted from SAS by Assistant, checked by Kerry Flaherty Walia
# Original Author: Tim MacDonald, Kerry Flaherty Walia
# Date: 27 January 2017
# 
# Program Summary:
# Used to subset the FIM program data when looking at multiple species.
# Two datasets are output: one contains length data for each record within 
# a field number (bio_reference, sl, nl, and count). The second dataset 
# contains the number of fish data combined with the physical data.
# =============================================================================

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(here)
library(usethis)
library(tbeptools)
library(googledrive)
drive_auth()

# SETUP SECTION===============================================================

# Bay to analyze
b <- "tb"

# Data type to analyze
t <- "m"

# Pathways to databases
# Location of multi-year historic data for all estuaries
# Google Drive path = TBEP_General/09_Tech_Projects/Gulf Ecosystem Initiative/Data/FIM/FIM_Data/
in1 <- drive_get("https://drive.google.com/drive/u/0/folders/1pAuF3vakLXWw8wvpMgkl9YU6TvhJkKiJ")
#Location of local data
local <-here("Data/")
# Location of processed data
out <- here("Output/")

# Select the years to be retained
b_yr <- 1999
e_yr <- 2023

# Select the projects to be retained
p1 <- "AM"
p2 <- "XX"
p3 <- "XX"
p4 <- "XX"

# Select the gears to be retained
g1 <- 20
g2 <- 19
g3 <- -1
g4 <- -1
g5 <- -1
g6 <- -1

# Select zones to be retained (all zones)
# Zone filter will be applied as: zone <= "Z"

# Variable lists
var0 <- c("reference", "bio_reference", "species", "number")

var1 <- c("date", "starttime", "gear", "rep", "latitude", "longitude", "zone", "grid",
          "Project_1", "Project_2", "Project_3", "historic_reference",
          "secchi_depth", "secchi_on_bottom", "stratum",
          "startdepth", "bottomvegcover", "bycatchquantity",
          "totalshorecover", "shoredistance", "bankdistance",
          "distance_to_edge", "seagrass_habitat_descriptor",
          "dist_to_MHTM", "dist_to_ShoreType", "dist_to_shore",
          "intermittent_land", "total_over_site", "TotalShoreCover", "SAM", "tide", "CloudCover",
          "FLUCCSCODE", "acres", "areas", "mngacre")

var2 <- c("month", "year", "gr", "effort", "cf", "slope", "TBEP_seg",
          "temperature", "salinity", "dissolvedO2", "sal_sd",
          "temp_surf", "sal_surf", "do2_surf",
          "bmud", "bsan", "bstr", "bunk", "bottom", "season", "sgyear",
          "Man", "Ter", "Str", "Eme", "shore", "ovr_wtf", "ind_wtf", "ovr_per", "ind_per",
          "SAV", "Alg", "Non", "HA", "TH", "RU", "bveg", "DominantVeg",
          "TBNI_Score", "ScoreNumTaxa", "ScoreShannon", "ScoreTaxaSelect", 
          "ScoreTaxaBenthic", "ScoreNumGuilds")

# Assign bay names
bay_names <- list(
  AP = "Apalachicola Bay", BB = "Big Bend", CK = "Cedar Key",
  CH = "Charlotte Harbor", EB = "Estero Bay", FB = "Florida Bay",
  HI = "Honeymoon Island", IR = "Northern Indian River lagoon",
  JX = "Northeast Florida", KY = "Florida Keys", LB = "Lemon Bay",
  SA = "St. Andrews Bay", SB = "Sarasota Bay",
  TQ = "Southern Indian River Lagoon", TB = "Tampa Bay"
)

bay <- bay_names[[toupper(b)]]

# Assign sampling type
type_names <- list(M = "stratified-random sampling", 
                   D = "directed sampling", 
                   F = "fixed station sampling")
type <- type_names[[toupper(t)]]
bay_typ <- paste(bay, type)

# IMPORT DATA==================================================================

# Downloading FIM RData from Google Drive folder
# List all RData files (should include the following: FIM_BiologyCounts, FIM_BiologyLengths, 
# FIM_Habitat, FIM_Hydrolab, FIM_PhysicalMaster, FIM_ReferenceCodes)
files <- drive_ls(in1)
rdata_files <- files[grepl("\\.RData$|\\.rda$", files$name, ignore.case = TRUE), ]
# Create directory
dir.create("data", showWarnings = FALSE)

# Download all RData files
for(i in 1:nrow(rdata_files)) {
  drive_download(
    file = rdata_files$id[i],
    path = file.path("data", rdata_files$name[i]),
    overwrite = TRUE
  )
  
  # Optionally load immediately
  load(file.path("data", rdata_files$name[i]))
}

# Import biology number data
num <- FIM_BiologyCounts
               
# Import biology length data
lng <- FIM_BiologyLengths
                
# Import physical data
fld1 <- FIM_PhysicalMaster
                
# Import habitat data
hab <- FIM_Habitat
             
# Import hydrolab data
hyd <- FIM_HydroLab

# Import FIM codes
fim_codes <- FIM_ReferenceCodes
                     
# PROCESS PHYSICAL DATA========================================================

# Create valid gear list
valid_gears <- c(g1, g2, g3, g4, g5, g6)
valid_gears <- valid_gears[valid_gears > 0]

fld <- fld1 %>%
  #Filter by bay
  filter(bay==b)%>%
  
  #Filter by type
  filter(type==t)%>%
  
  # Filter for designated years
 # filter(year %in% c(b_yr:e_yr))
  
  # Filter for designated projects
  filter(Project_1 %in% c(p1, p2, p3, p4) |
           Project_2 %in% c(p1, p2, p3, p4) |
           Project_3 %in% c(p1, p2, p3, p4)) %>%
  
  # Filter for zones (all zones <= "Z")
#  filter(zone <= "Z") %>%
  
  # Combine similar gear types
  mutate(
    gr = case_when(
      gear %in% c(1, 2) ~ 2,                                    # 6.1-m seine
      gear == 5 ~ 5,                                            # 9.1-m seine
      gear %in% c(11, 13, 20, 21, 25, 26, 29) | 
        (gear >= 100 & gear <= 107) ~ 20,                       # 21-m offshore seines
      gear == 20 & stratum == 'S' ~ 19,                        # 21-m offshore seine - beach stratum
      gear %in% c(12, 22, 24, 27, 28) ~ 22,                    # 21-m beach seines
      gear %in% c(10, 23) ~ 23,                                # 21-m boat seines
      gear %in% c(153, 154) ~ 153,                             # 61-m blocknets
      gear %in% c(155, 157, 158, 159, 160) ~ 160,              # 183-m haul seines
      gear %in% c(170, 431) ~ 170,                             # 183-m purse seine
      gear == 180 ~ 180,                                        # 61-m haul seine
      (gear >= 204 & gear <= 209) | 
        (gear >= 403 & gear <= 406) | 
        (gear >= 409 & gear <= 410) ~ 207,                      # gillnets
      gear %in% c(300, 301, 306) ~ 300,                        # 6.1-m otter trawls
      gear %in% c(350, 351, 354) ~ 350,                        # 1m roving dropnets
      gear == 436 ~ 436,                                        # 40m seine
      TRUE ~ NA_real_
    ),
    
    # Calculate effort for each gear type
    effort = case_when(
      gr == 2 ~ 31.17/100,                                      # 6.1-m seine
      gr == 5 ~ 10.1/100,                                       # 9.1-m seine
      gr %in% c(19, 20) ~ 140/100,                             # 21-m offshore seines
      gr == 22 ~ 338/100,                                       # 21-m beach seines
      gr == 23 ~ 68/100,                                        # 21-m boat seines
      gr == 160 ~ 4120/100,                                     # 183-m haul seine
      gr == 170 ~ 2209/100,                                     # 183-m purse seine
      gr == 180 ~ 465/100,                                      # 61-m haul seine
      gr == 207 ~ soakhr + soakmin/60,                         # gillnets
      gr == 300 ~ (dist_tow * 4 * 1853)/100,                   # 6.1-m otter trawls
      gr == 350 ~ 1/100,                                        # 1m roving dropnets
      gr == 436 ~ 180/100,                                      # 40m seine
      TRUE ~ NA_real_
    ),
    
    # Create unique zone identifiers
    z = paste0(bay, zone),
    
    # Calculate slope
    slope = case_when(
      gr %in% c(19, 20, 23, 160) ~ abs(startdepth - wng_dpth),
      gr == 300 ~ abs(startdepth - enddepth),
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Filter for designated gears
  filter(gr %in% valid_gears) %>%
  
  arrange(reference)

# Create retention list
ret <- fld %>%
  select(reference)

# PROCESS HYDROLAB DATA=========================================================

hyd1 <- hyd %>%
  arrange(reference)

hyd_out <- hyd1 %>%
  inner_join(ret, by = "reference")

# Calculate mean temperature, salinity, and dissolved O2 by reference
hyd_mean <- hyd_out %>%
  filter(!is.na(temperature) | !is.na(salinity)) %>%
  group_by(reference) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    salinity = mean(salinity, na.rm = TRUE),
    dissolvedO2 = mean(dissolvedO2, na.rm = TRUE),
    .groups = "drop"
  )

# Extract surface measurements (depth = 0.2)
hyd3 <- hyd_out %>%
  filter(depth == 0.2) %>%
  select(reference, 
         temp_surf = temperature, 
         sal_surf = salinity, 
         do2_surf = dissolvedO2)

# Merge hydrolab data with field data
hyd_fld <- hyd_mean %>%
  left_join(hyd3, by = "reference") %>%
  left_join(select(fld, reference, zone), by = "reference") %>%
  arrange(zone)

# Calculate salinity standard deviation by zone
sal_sd <- hyd_fld %>%
  group_by(zone) %>%
  summarise(sal_sd = sd(salinity, na.rm = TRUE), .groups = "drop")

# Merge salinity sd back
hyd <- hyd_fld %>%
  left_join(sal_sd, by = "zone") %>%
  select(reference, temperature, salinity, dissolvedO2, 
         temp_surf, sal_surf, do2_surf, sal_sd) %>%
  arrange(reference)

# PROCESS HABITAT DATA - BOTTOM TYPES===========================================

# Extract bottom type codes
bcodes <- fim_codes %>%
  filter(fieldname == "BottomType") %>%
  mutate(bcodes = code) %>%
  select(code = bcodes, description, category) %>%
  arrange(code)

# Process bottom types
btype1 <- hab %>%
  inner_join(ret, by = "reference") %>%
  mutate(code = bottomtype) %>%
  filter(!is.na(code), !code %in% c('N', '', '.')) %>%
  mutate(habitat_record_id = as.numeric(habitat_record_id)) %>%
  select(reference, habitat_record_id, code) %>%
  arrange(code)

btype2 <- btype1 %>%
  left_join(bcodes, by = "code") %>%
  mutate(
    bottom_cat = ifelse(code == 'U', "bUnk", 
                        paste0("b", substr(category, 1, 3))),
    habitat_record_id = case_when(
      habitat_record_id == 1 ~ 6,
      habitat_record_id == 2 ~ 5,
      habitat_record_id == 3 ~ 4,
      habitat_record_id == 4 ~ 3,
      habitat_record_id == 5 ~ 2,
      habitat_record_id >= 6 ~ 1,
      TRUE ~ habitat_record_id
    )
  ) %>%
  arrange(reference, bottom_cat)

# Keep only first occurrence of each bottom category per reference
btype3 <- btype2 %>%
  group_by(reference, bottom_cat) %>%
  slice(1) %>%
  mutate(k = 1) %>%
  ungroup()

# Transpose to wide format
bstats1 <- btype3 %>%
  select(reference, bottom_cat, k) %>%
  pivot_wider(names_from = bottom_cat, values_from = k, values_fill = 0)

# Create bottom type summary
btype4 <- bstats1 %>%
  mutate(
    bmud = if("bmud" %in% names(.)) bmud else 0,
    bsan = if("bsan" %in% names(.)) bsan else 0,
    bstr = if("bstr" %in% names(.)) bstr else 0,
    bunk = if("bunk" %in% names(.)) bunk else 0,
    
    bottom = case_when(
      bmud == 1 & bsan == 1 & bstr == 1 & bunk == 1 ~ 'MudSanStrUnk',
      bmud == 1 & bsan == 1 & bstr == 1 & bunk == 0 ~ 'MudSanStr',
      bmud == 1 & bsan == 1 & bstr == 0 & bunk == 0 ~ 'MudSan',
      bmud == 1 & bsan == 0 & bstr == 1 & bunk == 0 ~ 'MudStr',
      bmud == 1 & bsan == 0 & bstr == 0 & bunk == 1 ~ 'MudUnk',
      bmud == 1 & bsan == 0 & bstr == 0 & bunk == 0 ~ 'Mud',
      bmud == 0 & bsan == 1 & bstr == 0 & bunk == 0 ~ 'San',
      bmud == 0 & bsan == 1 & bstr == 1 & bunk == 0 ~ 'SanStr',
      bmud == 0 & bsan == 1 & bstr == 1 & bunk == 1 ~ 'SanStrUnk',
      bmud == 0 & bsan == 0 & bstr == 1 & bunk == 0 ~ 'Str',
      bmud == 0 & bsan == 0 & bstr == 1 & bunk == 1 ~ 'StrUnk',
      bmud == 0 & bsan == 0 & bstr == 0 & bunk == 1 ~ 'Unk',
      TRUE ~ NA_character_
    )
  ) %>%
  select(reference, bmud, bsan, bstr, bunk, bottom)

# PROCESS HABITAT DATA - BOTTOM VEGETATION=====================================

# Extract bottom vegetation codes
bvcodes <- fim_codes %>%
  filter(fieldname == "BottomVeg") %>%
  mutate(bvcodes = code) %>%
  select(code = bvcodes, description, category) %>%
  arrange(code)

# Get bottom cover from field data
bcover <- fld %>%
  mutate(bottomvegcover = as.numeric(bottomvegcover),
         bottomvegcover = ifelse(bottomvegcover == 101, 1, bottomvegcover)) %>%
  select(reference, bottomvegcover)

# Process bottom vegetation types
bvtype1 <- hab %>%
  inner_join(ret, by = "reference") %>%
  mutate(
    code = bottomveg,
    habitat_record_id = as.numeric(habitat_record_id),
    BottomVegRatio = as.numeric(BottomVegRatio)
  ) %>%
  filter(!is.na(code), !code %in% c("", " ", "  ", ".")) %>%
  mutate(
    code = ifelse(code == "UN", "NO", code),
    BottomVegRatio = case_when(
      code == "NO" ~ 10,
      code != "NO" & (is.na(BottomVegRatio) | BottomVegRatio == 0) ~ 0.1,
      TRUE ~ BottomVegRatio
    )
  ) %>%
  filter(!(habitat_record_id >= 2 & code == "NO")) %>%
  select(reference, code, BottomVegRatio) %>%
  arrange(code)

bvtype2 <- bvtype1 %>%
  left_join(bvcodes, by = "code") %>%
  mutate(
    bveg_cat = substr(category, 1, 3),
    bveg_cat = ifelse(code %in% c('HA', 'TH', 'RU'), code, bveg_cat)
  ) %>%
  arrange(reference, desc(BottomVegRatio))

# Keep dominant vegetation by reference
bvtype_SAV <- bvtype2 %>%
  group_by(reference) %>%
  slice(1) %>%
  mutate(
    DominantVeg = ifelse(BottomVegRatio > 5, description, "SAV")
  ) %>%
  ungroup() %>%
  select(reference, DominantVeg)

# Aggregate by vegetation category
bvtype3 <- bvtype2 %>%
  arrange(reference, bveg_cat) %>%
  group_by(reference, bveg_cat) %>%
  summarise(k = sum(BottomVegRatio, na.rm = TRUE), .groups = "drop") %>%
  arrange(reference, BottomVegRatio = k, bveg_cat)

# Transpose to wide format
bvstats1 <- bvtype3 %>%
  pivot_wider(names_from = bveg_cat, values_from = k, values_fill = 0)

# Merge and process vegetation data
bvtype4 <- bvstats1 %>%
  left_join(bcover, by = "reference") %>%
  left_join(bvtype_SAV, by = "reference") %>%
  mutate(
    # Initialize columns if they don't exist
    SAV = if("SAV" %in% names(.)) SAV else 0,
    Alg = if("Alg" %in% names(.)) Alg else 0,
    Non = if("Non" %in% names(.)) Non else 0,
    TH = if("TH" %in% names(.)) TH else 0,
    HA = if("HA" %in% names(.)) HA else 0,
    RU = if("RU" %in% names(.)) RU else 0,
    
    # Convert to presence/absence and handle 101 values
    SAV = case_when(
      SAV == 0 ~ 0,
      SAV < 1 & SAV > 0 ~ 1,
      SAV == 101 ~ 1,
      TRUE ~ SAV
    ),
    Alg = case_when(
      Alg == 0 ~ 0,
      Alg < 1 & Alg > 0 ~ 1,
      Alg == 101 ~ 1,
      TRUE ~ Alg
    ),
    TH = case_when(
      TH == 0 ~ 0,
      TH < 1 & TH > 0 ~ 1,
      TH == 101 ~ 1,
      TRUE ~ TH
    ),
    HA = case_when(
      HA == 0 ~ 0,
      HA < 1 & HA > 0 ~ 1,
      HA == 101 ~ 1,
      TRUE ~ HA
    ),
    RU = case_when(
      RU == 0 ~ 0,
      RU < 1 & RU > 0 ~ 1,
      RU == 101 ~ 1,
      TRUE ~ RU
    ),
    
    # Handle missing bottom veg cover
    bottomvegcover = case_when(
      bottomvegcover < 0 & (SAV > 0 | Alg > 0 | TH > 0 | HA > 0 | RU > 0) ~ 1,
      bottomvegcover < 0 & SAV == 0 & Alg == 0 & TH == 0 & HA == 0 & RU == 0 ~ 0,
      TRUE ~ bottomvegcover
    ),
    
    # Calculate weighted percentages
    SAV = SAV/10 * bottomvegcover,
    Alg = Alg/10 * bottomvegcover,
    Non = Non/10 * bottomvegcover,
    TH = TH/10 * bottomvegcover,
    HA = HA/10 * bottomvegcover,
    RU = RU/10 * bottomvegcover,
    
    # Categorize bottom vegetation
    bveg = case_when(
      (SAV > 0 | TH > 0 | HA > 0 | RU > 0) & Alg == 0 & Non == 0 ~ "SAV",
      SAV == 0 & TH == 0 & HA == 0 & RU == 0 & Alg > 0 & Non == 0 ~ "Alg",
      SAV == 0 & HA == 0 & TH == 0 & RU == 0 & Alg == 0 & Non > 0 ~ "Non",
      SAV == 0 & Alg == 0 & HA == 0 & TH == 0 & RU == 0 & Non == 0 ~ "Non",
      (SAV > 0 | TH > 0 | HA > 0 | RU > 0) & Alg > 0 & Non == 0 ~ "SAVAlg",
      (SAV > 0 | TH > 0 | HA > 0 | RU > 0) & Alg == 0 & Non > 0 ~ "SAVNon",
      (SAV > 0 | TH > 0 | HA > 0 | RU > 0) & Alg > 0 & Non > 0 ~ "SAVAlgNon",
      (SAV == 0 | TH == 0 | HA == 0 | RU == 0) & Alg > 0 & Non > 0 ~ "AlgNon",
      TRUE ~ NA_character_
    ),
    
    # Reset Non if bveg is "Non"
    Non = ifelse(bveg == "Non", 0, Non),
    
    # Clean up DominantVeg
    DominantVeg = case_when(
      DominantVeg == "Mixed" ~ bveg,
      DominantVeg %in% c("Acetabularia spp.", "Alg", "Algae: Mixed", 
                         "Algae: Unidentified", "Batophora spp.",
                         "Algae: Filamentous red", "Algae: Filamentous Green",
                         "Gracillaria", "Sargassum spp") ~ "Algae",
      DominantVeg %in% c("Halophila englemanii (star grass)", 
                         "Halophila decipiens (paddle grass)") ~ "Halophila spp.",
      DominantVeg %in% c("Seagrasses: Mixed", "Seagrasses: Unidentified") ~ "SAV",
      TRUE ~ DominantVeg
    )
  ) %>%
  select(reference, SAV, Alg, Non, HA, TH, RU, bveg, DominantVeg)

# PROCESS HABITAT DATA - SHORE TYPES============================================

# Extract shore type codes
scodes <- fim_codes %>%
  filter(fieldname == "ShoreType") %>%
  mutate(scodes = code) %>%
  select(code = scodes, description, category) %>%
  arrange(code)

# Get shore cover from field data
scover <- fld %>%
  mutate(TotalShoreCover = as.numeric(TotalShoreCover)) %>%
  select(reference, TotalShoreCover)

# Process shore types
stype1 <- hab %>%
  inner_join(ret, by = "reference") %>%
  mutate(
    code = shoretype,
    habitat_record_id = as.numeric(habitat_record_id),
    ShoreTypeRatio = as.numeric(ShoreTypeRatio)
  ) %>%
  filter(!is.na(code), !code %in% c("  ", " ", ".", "UN", "NO")) %>%
  mutate(
    ShoreTypeRatio = case_when(
      code != "NO" & (is.na(ShoreTypeRatio) | ShoreTypeRatio == 0) ~ 0.1,
      TRUE ~ ShoreTypeRatio
    ),
    Inund = ifelse(Inund %in% c('1','2','3','4','5','6','7','8','9','10','YES'), 
                   'YES', 'NO'),
    Over = ifelse(Over %in% c('1','2','3','4','5','6','7','8','9','10','YES'), 
                  'YES', 'NO')
  ) %>%
  select(reference, habitat_record_id, code, ShoreTypeRatio, Inund, Over, level) %>%
  arrange(code)

stype2 <- stype1 %>%stype2 <- stype1 %>%
  left_join(scodes, by = "code") %>%
  mutate(
    shore_cat = substr(category, 1, 3),
    shore_cat = ifelse(category == "Manmade", "Str", shore_cat),  # Lump manmade with structure
    inr = ifelse(Inund == "YES", 1, 0),
    ovr = ifelse(Over == "YES", 1, 0),
    ovr_wt = ovr * ShoreTypeRatio,
    ind_wt = inr * ShoreTypeRatio
  ) %>%
  arrange(reference)

# Calculate inundated/overhanging percentages
stype3 <- stype2 %>%
  group_by(reference) %>%
  summarise(
    str = sum(ShoreTypeRatio, na.rm = TRUE),
    ovr_wt1 = sum(ovr_wt, na.rm = TRUE),
    ind_wt1 = sum(ind_wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ref_year = as.numeric(substr(reference, 4, 7)),
    ovr_wtf = ifelse(ref_year < 2001, NA, round(ovr_wt1/str, 3)),
    ind_wtf = ifelse(ref_year < 2001, NA, round(ind_wt1/str, 3))
  ) %>%
  select(reference, ovr_wtf, ind_wtf)

stype4 <- stype3 %>%
  left_join(scover, by = "reference") %>%
  mutate(
    ref_year = as.numeric(substr(reference, 4, 7)),
    ovr_per = ifelse(ref_year >= 2001, TotalShoreCover * ovr_wtf, NA),
    ind_per = ifelse(ref_year >= 2001, TotalShoreCover * ind_wtf, NA)
  ) %>%
  select(reference, ovr_wtf, ind_wtf, ovr_per, ind_per)

# Aggregate shore types by category
ststats_r <- stype2 %>%
  group_by(reference, shore_cat) %>%
  summarise(count = sum(ShoreTypeRatio, na.rm = TRUE), .groups = "drop")

# Transpose to wide format
ststats_r2 <- ststats_r %>%
  pivot_wider(names_from = shore_cat, values_from = count, values_fill = 0)

# Define basic shore types
stype5 <- ststats_r2 %>%
  left_join(scover, by = "reference") %>%
  mutate(
    # Initialize columns if they don't exist
    Man = if("Man" %in% names(.)) Man else 0,
    Ter = if("Ter" %in% names(.)) Ter else 0,
    Str = if("Str" %in% names(.)) Str else 0,
    Eme = if("Eme" %in% names(.)) Eme else 0,
    
    # Cap values at 10
    Man = ifelse(Man > 10, 10, Man),
    Ter = ifelse(Ter > 10, 10, Ter),
    Str = ifelse(Str > 10, 10, Str),
    Eme = ifelse(Eme > 10, 10, Eme),
    
    # Categorize shore type
    shore = case_when(
      Man > 0 & Eme == 0 & Str == 0 & Ter == 0 ~ "Man",
      Man == 0 & Eme > 0 & Str == 0 & Ter == 0 ~ "Eme",
      Man == 0 & Eme == 0 & Str > 0 & Ter == 0 ~ "Str",
      Man == 0 & Eme == 0 & Str == 0 & Ter > 0 ~ "Ter",
      Man == 0 & Eme == 0 & Str == 0 & Ter == 0 ~ "Non",
      Man > 0 & Eme > 0 & Str == 0 & Ter == 0 ~ "ManEme",
      Man > 0 & Eme == 0 & Str > 0 & Ter == 0 ~ "ManStr",
      Man > 0 & Eme == 0 & Str == 0 & Ter > 0 ~ "ManTer",
      Man > 0 & Eme > 0 & Str > 0 & Ter == 0 ~ "ManEmeStr",
      Man > 0 & Eme == 0 & Str > 0 & Ter > 0 ~ "ManStrTer",
      Man > 0 & Eme > 0 & Str == 0 & Ter > 0 ~ "ManEmeTer",
      Man > 0 & Eme > 0 & Str > 0 & Ter > 0 ~ "ManEmeStrTer",
      Man == 0 & Eme > 0 & Str > 0 & Ter == 0 ~ "EmeStr",
      Man == 0 & Eme > 0 & Str == 0 & Ter > 0 ~ "EmeTer",
      Man == 0 & Eme > 0 & Str > 0 & Ter > 0 ~ "EmeStrTer",
      Man == 0 & Eme == 0 & Str > 0 & Ter > 0 ~ "StrTer",
      TRUE ~ NA_character_
    )
  ) %>%
  select(reference, Man, Ter, Str, Eme, shore, TotalShoreCover)

# Combine all habitat data
hab_combined <- btype4 %>%
  full_join(bvtype4, by = "reference") %>%
  full_join(stype5, by = "reference") %>%
  full_join(stype4, by = "reference") %>%
  select(reference, bmud, bsan, bstr, bunk, bottom, 
         SAV, Alg, Non, HA, TH, RU, bveg, DominantVeg,
         Man, Ter, Str, Eme, shore, ovr_per, ind_per)

# PROCESS WEATHER DATA==========================================================

wth_processed <- wth %>%
  filter(Beg_end == "B") %>%
  arrange(reference) %>%
  inner_join(ret, by = "reference") %>%
  select(reference, CloudCover, Tide)

# PROCESS BIOLOGY NUMBER DATA===================================================

fish1 <- num %>%
  arrange(reference) %>%
  inner_join(ret, by = "reference") %>%
  mutate(
    bay = substr(reference, 1, 2),
    bio_reference = toupper(paste0(reference, SC, splitlevel)),
    
    # Clean NODCCODE
    nodccode = str_replace_all(NODCCODE, " ", ""),
    
    # Define taxonomic levels
    subspecies = ifelse(nchar(nodccode) > 10, nodccode, NA_character_),
    species = substr(nodccode, 1, 10),
    genus = substr(nodccode, 1, 8),
    family = substr(nodccode, 1, 6),
    order = substr(nodccode, 1, 4),
    class = substr(nodccode, 1, 2),
    
    # Apply splitter data
    Cells = ifelse(Splittype == 2, 1, Cells),
    Number = ifelse(!is.na(Splittype), 
                    Number * (Splittype^Splitlevel) / Cells, 
                    Number),
    
    # Combine certain genera
    species = case_when(
      genus %in% c('61890213', '87470104') ~ paste0(genus, "00"),
      genus == '88050203' & bay != 'JX' ~ paste0(genus, "00"),
      TRUE ~ species
    )
  ) %>%
  select(reference, bio_reference, species, number = Number, species_record_id = Species_record_id) %>%
  arrange(reference, species)

# Total number of fish from each collection
num_totals <- fish1 %>%
  arrange(bio_reference, species) %>%
  group_by(bio_reference, species) %>%
  summarise(n = sum(number, na.rm = TRUE), .groups = "drop") %>%
  left_join(select(fish1, bio_reference, reference) %>% distinct(), 
            by = "bio_reference") %>%
  arrange(reference)

# PROCESS LENGTH DATA===========================================================

len1 <- lng %>%
  mutate(
    sl = as.numeric(length),
    species_record_id = as.numeric(species_record_id),
    length_record_id = as.numeric(length_record_id)
  ) %>%
  filter(!is.na(sl)) %>%
  arrange(reference, species_record_id, length_record_id)

# Merge with fish data
len_merged <- fish1 %>%
  select(reference, species_record_id, bio_reference, species, number) %>%
  inner_join(len1, by = c("reference", "species_record_id")) %>%
  filter(!is.na(number), number != 0) %>%
  select(reference, bio_reference, species_record_id, length_record_id, species, sl)

# Count frequency of each length
len_frq <- len_merged %>%
  group_by(bio_reference, species, sl) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(bio_reference, species, sl)

# Count number of animals measured
nl <- len_frq %>%
  group_by(bio_reference, species) %>%
  summarise(nl = sum(count), .groups = "drop")

# Create final length dataset
len_final <- len_frq %>%
  left_join(nl, by = c("bio_reference", "species")) %>%
  select(bio_reference, species, nl, count, sl) %>%
  arrange(bio_reference, species)

# CREATE COMBINED DATA==========================================================

com <- fld %>%
  left_join(hab_combined, by = "reference") %>%
  left_join(hyd, by = "reference") %>%
  left_join(wth_processed, by = "reference") %>%
  left_join(num_totals, by = "reference") %>%
  mutate(
    number = n,
    # Avoid upweighting 101 records
    bottomvegcover = as.numeric(bottomvegcover),
    bottomvegcover = ifelse(bottomvegcover == 101, 51, bottomvegcover)
  ) %>%
  select(-ovr_per, -ind_per, -n) %>%
  arrange(bio_reference, species)

# Select variables to keep
keep_vars <- c(var0, var1, var2)
keep_vars <- keep_vars[keep_vars %in% names(com)]
com <- com %>% select(all_of(keep_vars))

# Export combined data
write_csv(com, paste0(out, tolower(b), tolower(t), "_c.csv"))

# CREATE FINAL LENGTH DATASET==================================================

len_with_com <- len_frq %>%
  left_join(nl, by = c("bio_reference", "species")) %>%
  inner_join(select(com, bio_reference, species), by = c("bio_reference", "species")) %>%
  select(bio_reference, species, nl, count, sl)

# Export length data
write_csv(len_with_com, paste0(out, tolower(b), tolower(t), "_l.csv"))

# DATA CHECKS==================================================================

# Setup species code database
spp <- species_codes %>%
  mutate(species = substr(NODCCODE, 1, 10)) %>%
  select(species, scientificname = Scientificname) %>%
  distinct() %>%
  arrange(species)

# Merge with combined data for checking
chck <- com %>%
  left_join(spp, by = "species") %>%
  arrange(reference)

# Check for missing numbers or effort
chck1 <- chck %>%
  filter(is.na(number) | is.na(effort))

if(nrow(chck1) > 0) {
  cat("\n===========================================\n")
  cat(bay_typ, "\n")
  cat("Why do these entries not have valid values for number or effort?\n")
  cat("===========================================\n")
  print(chck1 %>% select(reference, historic_reference, gr, effort, number, cf))
}

# Define formats for output
gear_labels <- c(
  "19" = "offshr sn - shr strat", "20" = "offshr sn - off strat",
  "22" = "beach seine", "23" = "boat seine",
  "160" = "183-m haul seine", "170" = "183-m purse seine", "153" = "61-m blocknet",
  "180" = "61-m haul seine", "207" = "gillnet", "300" = "trawl", "350" = "dropnet"
)

month_labels <- c(
  "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
  "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
  "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"
)

# Check number of samples by gear, year, and month
chk2_samples <- chck %>%
  group_by(reference) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    gr_label = gear_labels[as.character(gr)],
    month_label = month_labels[as.character(month)]
  ) %>%
  group_by(gr_label, year, month_label) %>%
  summarise(n_collections = n(), .groups = "drop") %>%
  arrange(gr_label, year, month_label)

cat("\n===========================================\n")
cat("Number of collections each month and year, by gear\n")
cat("===========================================\n")
print(chk2_samples)

# Check number of fish by gear, year, month, and species
chk2_fish <- chck %>%
  filter(species != "9998000000") %>%
  mutate(
    gr_label = gear_labels[as.character(gr)],
    month_label = month_labels[as.character(month)],
    px = case_when(
      Project_1 %in% c('AM','AS') | Project_2 %in% c('AM','AS') | Project_3 %in% c('AM','AS') ~ 
        "Stratified-random sampling",
      Project_1 == 'AR' | Project_2 == 'AR' | Project_3 == 'AR' ~ "River Study",
      Project_1 == 'WI' | Project_2 == 'WI' | Project_3 == 'WI' ~ 
        "Inshore - West Florida Shelf",
      TRUE ~ "Other"
    )
  )

# Summary by gear, year, month, and species
fish_by_gear_month <- chk2_fish %>%
  group_by(gr_label, year, month_label, scientificname) %>%
  summarise(total_number = sum(number, na.rm = TRUE), .groups = "drop") %>%
  arrange(gr_label, year, scientificname, month_label)

cat("\n===========================================\n")
cat("Number of each species collected each month and year, by gear\n")
cat("Terminal bag seine correction factor has not been applied\n")
cat("===========================================\n")
print(head(fish_by_gear_month, 50))  # Show first 50 rows

# Summary by project and species
fish_by_project <- chk2_fish %>%
  group_by(px, year, scientificname) %>%
  summarise(total_number = sum(number, na.rm = TRUE), .groups = "drop") %>%
  arrange(px, scientificname, year)

cat("\n===========================================\n")
cat("Number of each species collected each year, by project\n")
cat("Terminal bag seine correction factor has not been applied\n")
cat("===========================================\n")
print(head(fish_by_project, 50))  # Show first 50 rows

# Export check files
write_csv(chck, paste0(out, "check_combined_data.csv"))
write_csv(chk2_samples, paste0(out, "check_samples_summary.csv"))
write_csv(fish_by_gear_month, paste0(out, "check_fish_by_gear_month.csv"))
write_csv(fish_by_project, paste0(out, "check_fish_by_project.csv"))

cat("\n===========================================\n")
cat("Processing complete!\n")
cat("Output files created in:", out, "\n")
cat("===========================================\n")