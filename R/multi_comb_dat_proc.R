# =============================================================================
# Program: multi_comb_dat_proc.R
# Programmer: Converted from SAS by AI (Claude), checked by Kerry Flaherty Walia
# Original Author: Tim MacDonald, Kerry Flaherty Walia
# Date: 27 January 2017
# 
# Program Summary:
# Used to subset the FIM program data when looking at multiple species.
# Two datasets are output: one contains length data for each record within 
# a field number (reference, species, sl, nl, and count, and weighting factor (wf)). The second dataset 
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

# SETUP SECTION===============================================================

# Bay to analyze
b <- "TB"

# Data type to analyze, "_" to include all data types
t <- "_"

# Filename suffix - to identify subset of data pulled (example: all, zone, gr, baysegment)
s <- "all"

# Select the years to be retained
b_yr <- 2000
e_yr <- 2024

# Select the projects to be retained, if not filtered "XX"
p <- c("XX")

# Select the gears to be retained, if not filtered "XXX"
g <- c("XXX")

# Select zones to be retained ("Z" all zones, or select specific zones)
# Zone filter will be applied as: 
z <- c("Z")

## Variable lists
var0 <- c("Reference", "Scientificname","Commonname","species", "number","family","order","class","Taxa_Type")

var1 <- c("Date", "StartTime", "Gear", "Rep", "Latitude", "Longitude", "Zone", "Grid",
          "Project_1", "Project_2", "Project_3", "Secchi_depth", "Secchi_on_bottom", "Stratum",
          "StartDepth", "BottomVegCover", "BycatchQuantity",
          "TotalShoreCover", "ShoreDistance", "Dist_to_MHTM", "Dist_to_ShoreType", "Dist_to_Shore",
          "Intermittent_land", "Total_Over_Site", "TotalShoreCover")

var2 <- c("Month", "Year", "gr", "effort", "cf", "slope", "TBEP_seg",
          "Temperature", "Salinity", "DissolvedO2", "sal_sd",
          "temp_surf", "sal_surf", "do2_surf","bmud", "bsan", "bstr", "bunk", "bottom", 
          "Man", "Ter", "Str", "Eme", "shore", "ovr_wtf", "ind_wtf", "ovr_per", "ind_per",
          "SAV", "Alg", "Non", "HA", "TH", "RU", "bveg", "DominantVeg")

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

# IMPORT DATA==================================================================
#Bring in Tampa Bay subset populated to repository from tb_subset.R
# Import biology number data
num <- TB_FIM_BiologyCounts

# Import biology length data
lng <- TB_FIM_BiologyLengths

# Import physical data
fld1 <- TB_FIM_PhysicalMaster

# Import habitat data
hab <- TB_FIM_Habitat

# Import hydrolab data
hyd <- TB_FIM_Hydrolab

# Import FIM codes
fim_codes <- FIM_ReferenceCodes

#Import species codes
spp_codes <- FIM_SpeciesCodes
                     
# PROCESS PHYSICAL DATA========================================================

fld <- fld1 %>%
  #Filter by bay, no location, zone, type, project
  filter(Bay==b, 
         !is.na(Latitude) & !is.na(Longitude),
         if(t == "_") TRUE else Type == t,
         if(z == "Z") TRUE else Zone %in% z,
         if(p == "XX") TRUE else any(c(Project1, Project2, Project3) %in% p),
         Year >= b_yr & Year <= e_yr,
         ) %>%
  
  # Combine similar gear types
  mutate(
    gr = case_when(
      Gear %in% c('001', '002') ~ '002',                                    # 6.1-m seine
      Gear %in% c('005') ~ '005',                                            # 9.1-m seine
      Gear %in% c('011', '013', '020', '021', '025', '026', '029',
                  '100', '101', '102', '103', '104', '105', '106', '107') ~ '020', # 21-m offshore seines
      Gear == '020' & Stratum == 'S' ~ '019',                        # 21-m offshore seine - beach stratum
      Gear %in% c('012', '022', '024', '027', '028') ~ '022',                    # 21-m beach seines
      Gear %in% c('010', '023') ~ '023',                                # 21-m boat seines
      Gear %in% c('153', '154') ~ '153',                             # 61-m blocknets
      Gear %in% c('155', '157', '158', '159', '160') ~ '160',              # 183-m haul seines
      Gear %in% c('170', '431') ~ '170',                             # 183-m purse seine
      Gear == '180' ~ '180',                                        # 61-m haul seine
      Gear %in% c('204','205','206','207','208','209',
                  '403','404','405','406','409','410') ~ '207',        # gillnets
      Gear %in% c('300', '301', '306') ~ '300',                        # 6.1-m otter trawls
      Gear %in% c('350', '351', '354') ~ '350',                        # 1m roving dropnets
      Gear == '436' ~ '436',                                        # 40m seine
      .default = Gear),
    
    # Calculate effort (in 100m2) for each gear type
    effort = case_when(
      gr == '002' ~ 31.17/100,                                      # 6.1-m seine
      gr == '005' ~ 10.1/100,                                       # 9.1-m seine
      gr %in% c('019', '020') ~ 140/100,                             # 21-m offshore seines
      gr == '022' ~ 338/100,                                       # 21-m beach seines
      gr == '023' ~ 68/100,                                        # 21-m boat seines
      gr == '160' ~ 4120/100,                                     # 183-m haul seine
      gr == '170' ~ 2209/100,                                     # 183-m purse seine
      gr == '180' ~ 465/100,                                      # 61-m haul seine
      #     gr == '207' ~ Soakhr + Soakmin/60,                         # gillnets
      gr == '300' ~ (Dist_tow * 4 * 1853)/100,                   # 6.1-m otter trawls
      gr == '350' ~ 1/100,                                        # 1m roving dropnets
      gr == '436' ~ 180/100,                                      # 40m seine
      TRUE ~ NA_real_
    ),
    
    # Calculate slope
    slope = case_when(
      gr %in% c('019', '020', '023', '160') ~ abs(StartDepth - Wng_dpth),
      gr == '300' ~ abs(StartDepth - Enddepth),
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Filter for designated gears
  filter(if(g == "XXX") TRUE else gr %in% g) %>%
  
  arrange(Reference)

# Create retention list
ret <- fld %>%
  select(Reference)

#Create GIS reference
gis <- fld %>%
  select(Reference,gr,Latitude,Longitude)
# Export station location data
write_csv(gis, here("Output", paste0(tolower(b),tolower(t), tolower(s), "_gis.csv")))

# PROCESS HYDROLAB DATA=========================================================

hyd1 <- hyd %>%
  arrange(Reference)

hyd_out <- hyd1 %>%
  inner_join(ret, by = "Reference")

# Calculate mean temperature, salinity, and dissolved O2 by reference
hyd_mean <- hyd_out %>%
  filter(!is.na(Temperature) | !is.na(Salinity)) %>%
  group_by(Reference) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    Salinity = mean(Salinity, na.rm = TRUE),
    DissolvedO2 = mean(DissolvedO2, na.rm = TRUE),
    .groups = "drop"
  )

# Extract surface measurements (depth = 0.2)
hyd3 <- hyd_out %>%
  filter(Depth == 0.2) %>%
  select(Reference, 
         temp_surf = Temperature, 
         sal_surf = Salinity, 
         do2_surf = DissolvedO2)

# Merge hydrolab data with field data
hyd_fld <- hyd_mean %>%
  left_join(hyd3, by = "Reference") %>%
  left_join(select(fld, Reference, Zone), by = "Reference") %>%
  arrange(Zone)

# Calculate salinity standard deviation by zone
sal_sd <- hyd_fld %>%
  group_by(Zone) %>%
  summarise(sal_sd = sd(Salinity, na.rm = TRUE), .groups = "drop")

# Merge salinity sd back
hyd <- hyd_fld %>%
  left_join(sal_sd, by = "Zone") %>%
  select(Reference, Temperature, Salinity, DissolvedO2, 
         temp_surf, sal_surf, do2_surf, sal_sd) %>%
  arrange(Reference)

# PROCESS HABITAT DATA - BOTTOM TYPES===========================================

# Extract bottom type codes
bcodes <- fim_codes %>%
  filter(FieldName == "BottomType") %>%
  mutate(bcodes = Code) %>%
  select(code = bcodes, Description, Category) %>%
  arrange(code)

# Process bottom types
btype1 <- hab %>%
  inner_join(ret, by = "Reference") %>%
  mutate(code = BottomType) %>%
  filter(!is.na(code), !code %in% c('N', '', '.')) %>%
  mutate(Habitat_record_id = as.numeric(Habitat_record_id)) %>%
  select(Reference, Habitat_record_id, code) %>%
  arrange(code)

btype2 <- btype1 %>%
  left_join(bcodes, by = "code") %>%
  mutate(
    bottom_cat = ifelse(code == 'U', "bUnk", 
                        paste0("b", substr(Category, 1, 3))),
    Habitat_record_id = case_when(
      Habitat_record_id == 1 ~ 6,
      Habitat_record_id == 2 ~ 5,
      Habitat_record_id == 3 ~ 4,
      Habitat_record_id == 4 ~ 3,
      Habitat_record_id == 5 ~ 2,
      Habitat_record_id >= 6 ~ 1,
      TRUE ~ Habitat_record_id
    )
  ) %>%
  arrange(Reference, bottom_cat)

# Keep only first occurrence of each bottom category per reference
btype3 <- btype2 %>%
  group_by(Reference, bottom_cat) %>%
  slice(1) %>%
  mutate(k = 1) %>%
  ungroup()

# Transpose to wide format
bstats1 <- btype3 %>%
  select(Reference, bottom_cat, k) %>%
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
  select(Reference, bmud, bsan, bstr, bunk, bottom)

# PROCESS HABITAT DATA - BOTTOM VEGETATION=====================================

# Extract bottom vegetation codes
bvcodes <- fim_codes %>%
  filter(FieldName == "BottomVeg") %>%
  mutate(bvcodes = Code) %>%
  select(code = bvcodes, Description, Category) %>%
  arrange(code)

# Get bottom cover from field data
bcover <- fld %>%
  mutate(BottomVegCover = as.numeric(BottomVegCover),
         BottomVegCover = ifelse(BottomVegCover == 101, 1, BottomVegCover)) %>%
  select(Reference, BottomVegCover)

# Process bottom vegetation types
bvtype1 <- hab %>%
  inner_join(ret, by = "Reference") %>%
  mutate(
    code = BottomVeg,
    Habitat_record_id = as.numeric(Habitat_record_id),
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
  filter(!(Habitat_record_id >= 2 & code == "NO")) %>%
  select(Reference, code, BottomVegRatio) %>%
  arrange(code)

bvtype2 <- bvtype1 %>%
  left_join(bvcodes, by = "code") %>%
  mutate(
    bveg_cat = substr(Category, 1, 3),
    bveg_cat = ifelse(code %in% c('HA', 'TH', 'RU'), code, bveg_cat)
  ) %>%
  arrange(Reference, desc(BottomVegRatio))

# Keep dominant vegetation by reference
bvtype_SAV <- bvtype2 %>%
  group_by(Reference) %>%
  slice(1) %>%
  mutate(
    DominantVeg = ifelse(BottomVegRatio > 5, Description, "SAV")
  ) %>%
  ungroup() %>%
  select(Reference, DominantVeg)

# Aggregate by vegetation category
bvtype3 <- bvtype2 %>%
  arrange(Reference, bveg_cat) %>%
  group_by(Reference, bveg_cat) %>%
  summarise(k = sum(BottomVegRatio, na.rm = TRUE), .groups = "drop") %>%
  arrange(Reference, BottomVegRatio = k, bveg_cat)

# Transpose to wide format
bvstats1 <- bvtype3 %>%
  pivot_wider(names_from = bveg_cat, values_from = k, values_fill = 0)

# Merge and process vegetation data
bvtype4 <- bvstats1 %>%
  left_join(bcover, by = "Reference") %>%
  left_join(bvtype_SAV, by = "Reference") %>%
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
    BottomVegCover = case_when(
      BottomVegCover < 0 & (SAV > 0 | Alg > 0 | TH > 0 | HA > 0 | RU > 0) ~ 1,
      BottomVegCover < 0 & SAV == 0 & Alg == 0 & TH == 0 & HA == 0 & RU == 0 ~ 0,
      TRUE ~ BottomVegCover
    ),
    
    # Calculate weighted percentages
    SAV = SAV/10 * BottomVegCover,
    Alg = Alg/10 * BottomVegCover,
    Non = Non/10 * BottomVegCover,
    TH = TH/10 * BottomVegCover,
    HA = HA/10 * BottomVegCover,
    RU = RU/10 * BottomVegCover,
    
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
  select(Reference, SAV, Alg, Non, HA, TH, RU, bveg, DominantVeg)


# PROCESS HABITAT DATA - SHORE TYPES============================================

# Extract shore type codes
scodes <- fim_codes %>%
  filter(FieldName == "ShoreType") %>%
  mutate(scodes = Code) %>%
  select(code = scodes, Description, Category) %>%
  arrange(code)

# Get shore cover from field data
scover <- fld %>%
  mutate(TotalShoreCover = as.numeric(TotalShoreCover)) %>%
  select(Reference, TotalShoreCover)

# Process shore types
stype1 <- hab %>%
  inner_join(ret, by = "Reference") %>%
  mutate(
    code = ShoreType,
    Habitat_record_id = as.numeric(Habitat_record_id),
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
  select(Reference, Habitat_record_id, code, ShoreTypeRatio, Inund, Over, Level) %>%
  arrange(code)

stype2 <- stype1 %>%
  left_join(scodes, by = "code") %>%
  mutate(
    shore_cat = substr(Category, 1, 3),
    shore_cat = ifelse(Category == "Manmade", "Str", shore_cat),  # Lump manmade with structure
    inr = ifelse(Inund == "YES", 1, 0),
    ovr = ifelse(Over == "YES", 1, 0),
    ovr_wt = ovr * ShoreTypeRatio,
    ind_wt = inr * ShoreTypeRatio
  ) %>%
  arrange(Reference)

# Calculate inundated/overhanging percentages
stype3 <- stype2 %>%
  group_by(Reference) %>%
  summarise(
    str = sum(ShoreTypeRatio, na.rm = TRUE),
    ovr_wt1 = sum(ovr_wt, na.rm = TRUE),
    ind_wt1 = sum(ind_wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ref_year = as.numeric(substr(Reference, 4, 7)),
    ovr_wtf = ifelse(ref_year < 2001, NA, round(ovr_wt1/str, 3)),
    ind_wtf = ifelse(ref_year < 2001, NA, round(ind_wt1/str, 3))
  ) %>%
  select(Reference, ovr_wtf, ind_wtf)

stype4 <- stype3 %>%
  left_join(scover, by = "Reference") %>%
  mutate(
    ref_year = as.numeric(substr(Reference, 4, 7)),
    ovr_per = ifelse(ref_year >= 2001, TotalShoreCover * ovr_wtf, NA),
    ind_per = ifelse(ref_year >= 2001, TotalShoreCover * ind_wtf, NA)
  ) %>%
  select(Reference, ovr_wtf, ind_wtf, ovr_per, ind_per)

# Aggregate shore types by category
ststats_r <- stype2 %>%
  group_by(Reference, shore_cat) %>%
  summarise(count = sum(ShoreTypeRatio, na.rm = TRUE), .groups = "drop")

# Transpose to wide format
ststats_r2 <- ststats_r %>%
  pivot_wider(names_from = shore_cat, values_from = count, values_fill = 0)

# Define basic shore types
stype5 <- ststats_r2 %>%
  left_join(scover, by = "Reference") %>%
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
  select(Reference, Man, Ter, Str, Eme, shore, TotalShoreCover)

# Combine all habitat data
hab_combined <- btype4 %>%
  full_join(bvtype4, by = "Reference") %>%
  full_join(stype5, by = "Reference") %>%
  full_join(stype4, by = "Reference") %>%
  select(Reference, bmud, bsan, bstr, bunk, bottom, 
         SAV, Alg, Non, HA, TH, RU, bveg, DominantVeg,
         Man, Ter, Str, Eme, shore, ovr_per, ind_per)

# PROCESS BIOLOGY NUMBER DATA===================================================

num_totals <- num %>%
  arrange(Reference) %>%
  inner_join(ret, by = "Reference") %>%
  mutate(
    bay = substr(Reference, 1, 2),
    bio_reference = toupper(paste0(Reference, SC)),
    
    # Clean NODCCODE
    nodccode = str_replace_all(NODCCODE, " ", ""),
    
    # Define taxonomic levels
    subspecies = ifelse(nchar(nodccode) > 10, nodccode, NA_character_),
    species = substr(nodccode, 1, 10),
    genus = substr(nodccode, 1, 8),
    family = substr(nodccode, 1, 6),
    order = substr(nodccode, 1, 4),
    class = substr(nodccode, 1, 2),
    
    # Combine certain genera
    species = case_when(
      genus %in% c('61890213', '87470104') ~ paste0(genus, "00"),
      genus == '88050203' & bay != 'JX' ~ paste0(genus, "00"),
      TRUE ~ species
    )
  ) %>%
  arrange(Reference, species)

# PROCESS LENGTH DATA===========================================================

# Subset length data
len_final <- lng %>%
  arrange(Reference) %>%
  inner_join(ret, by = "Reference") 


# CREATE COMBINED DATA==========================================================

com <- fld %>%
  left_join(hab_combined, by = "Reference") %>%
  left_join(hyd, by = "Reference") %>%
  # left_join(wth_processed, by = "Reference") %>%
  left_join(num_totals, by = "Reference","species") %>%
  mutate(
    number = N_Total,
    # Avoid upweighting 101 records
    BottomVegCover = as.numeric(BottomVegCover),
    BottomVegCover = ifelse(BottomVegCover == 101, 51, BottomVegCover)
  ) %>%
  select(-ovr_per, -ind_per, -N_Total) %>%
  arrange(Reference, species)

# Select variables to keep
keep_vars <- c(var0, var1, var2)
keep_vars <- keep_vars[keep_vars %in% names(com)]
com <- com %>% select(all_of(keep_vars))

# Export combined data
save(com, file = here("Output", paste0(tolower(b), tolower(t), s, "_c.RData")))

# CREATE FINAL LENGTH DATASET==================================================

# Export length data
save(len_final, file=here("Output",paste0(tolower(b),tolower(t), s,  "_l.RData")))

# DATA CHECKS - not complete==================================================================

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