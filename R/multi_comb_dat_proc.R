# Load required libraries
library(googledrive)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)
library(purrr)

# Header ========================================================================

# Program:       multi_comb_dat_proc.R
# Programmer:    Kerry Flaherty-Walia (converted to R)
# Date:          27 january 2017 (original SAS)

# Program summary===============================================================

# Use to subset the FIM program data when looking at multiple species.   
# Two datasets are output. One dataset contains the length data for      
# each record within a field number (bio_reference, sl, nl, and count).  
# The second dataset contains the number of fish data combined with      
# the physical data. The two datasets can be merged by bio_reference.    
# The first eleven characters of bio_reference represents reference      
# while the remaining represent the splitter data within that reference. 

# SETUP FUNCTION===============================================================
setup <- function() {
  
  # Create a list to store all parameters
  params <- list()
  
  # Bay to analyze
  params$b <- "tb"
  
  # Data type to analyze
  params$t <- "m"
  
  # Pathways to databases
  # Location of multi-year historic data for all estuaries
  #Work on replacing in1 with Google Drive path = TBEP_General/09_Tech_Projects/Gulf Ecosystem Initiative/Data/FIM/FIM_Data/
  #params$in1 <- "https://drive.google.com/drive/folders/1pAuF3vakLXWw8wvpMgkl9YU6TvhJkKiJ?usp=drive_link"
  #for now, downloaded to local folder manually
  params$in1 <- here("Data/")
 
  # Location where you want the data subset to go to
  params$out <- here("Data/")
  
  # Select the years to be retained in the database subset
  params$b_yr <- 1999
  params$e_yr <- 2023
  
  # Select the projects to be retained in the database subset
  params$p1 <- "AM"  # first project - must be selected
  params$p2 <- "XX"  # second project - use XX if only one project
  params$p3 <- "XX"  # third project - use XX if two or fewer projects
  params$p4 <- "XX"  # fourth project - use XX if three or fewer projects
  
  # Select the gears to be retained in the database subset
  params$g1 <- 20   # first gear type - must be selected
  params$g2 <- 19   # second gear type - negative 1 (-1) if only one gear
  params$g3 <- -1   # third gear type - negative 1 (-1) if two or fewer gears
  params$g4 <- -1   # fourth gear type - negative 1 (-1) if three or fewer gears
  params$g5 <- -1   # fifth gear type - negative 1 (-1) if four or fewer gears
  params$g6 <- -1   # sixth gear type - negative 1 (-1) if five or fewer gears
  
  # Select the zones to be retained in the database subset
  params$z <- "zone <= 'Z'"  # all zones
  # Alternative: params$z <- "zone %in% c('A','B','C','D','E')"  # select zones
  # Alternative: params$m <- "month %in% c(4,10)"
  
  # Variables to retain
  # Variables that should always be retained
  params$var0 <- c("reference", "bio_reference", "species", "number")
  
  # Physical data (add to this list as necessary)
  params$var1 <- c(
    "date", "starttime", "gear", "rep", "latitude", "longitude", "zone", "grid",
    "Project_1", "Project_2", "Project_3", "historic_reference",
    "secchi_depth", "secchi_on_bottom", "stratum",
    "startdepth", "bottomvegcover", "bycatchquantity",
    "totalshorecover", "shoredistance", "bankdistance",
    "distance_to_edge", "seagrass_habitat_descriptor",
    "dist_to_MHTM", "dist_to_ShoreType", "dist_to_shore",
    "intermittent_land", "total_over_site", "TotalShoreCover", "SAM", "tide", "CloudCover"
     )
  
  # Generated variables (add to this list as necessary)
  params$var2 <- c(
    "month", "year", "gr", "effort", "cf", "slope", "TBEP_seg",
    "temperature", "salinity", "dissolvedO2", "sal_sd",
    "temp_surf", "sal_surf", "do2_surf",
    "bmud", "bsan", "bstr", "bunk", "bottom", "season", "sgyear",
    "Man", "Ter", "Str", "Eme", "shore", "ovr_wtf", "ind_wtf", "ovr_per", "ind_per",
    "SAV", "Alg", "Non", "HA", "TH", "RU", "bveg", "DominantVeg",
    )
  
  # Return the parameters list
  return(params)
}

# Run the setup function and store parameters
config <- setup()

# ASSIGN DATABASES FUNCTION=====================================================

assign_databases <- function(config) {
  
  # Create a list to store database paths and names
  db <- list()
  
  # Assign library paths (in R, we use these as base paths)
  db$in1 <- config$in1
  db$out <- config$out
  
  # Macro substitution for the FIM database(s) to subset - automatically assigned
  # Base databases
  db$num <- paste0(here(config$in1,"FIM_BiologyCounts.RData"))
  db$lng <- paste0(here(config$in1,"FIM_BiologyLengths.RData"))
  db$fld <- paste0(here(config$in1,"FIM_PhysicalMaster.RData"))
  db$hab <- paste0(here(config$in1,"FIM_Habitat.RData"))
  db$hyd <- paste0(here(config$in1,"FIM_Hydrolab.RData"))
  db$fim_codes <- paste0(here(config$in1,"FIM_ReferenceCodes.RData"))
 
  # Macro substitutions that name the subsetted output databases - automatically assigned
  db$len <- paste0(config$out, config$b, config$t, "_l")
  db$com <- paste0(config$out, config$b, config$t, "_c")
  db$hyd_out <- paste0(config$out, config$b, config$t, "_hyd")
  db$hab_out <- paste0(config$out, config$b, config$t, "_hab")
  
  # Assign estuary names based upon macro substitution b
  bay_names <- list(
    AP = "Apalachicola Bay",
    BB = "Big Bend",
    CK = "Cedar Key",
    CH = "Charlotte Harbor",
    EB = "Estero Bay",
    FB = "Florida Bay",
    HI = "Honeymoon Island",
    IR = "Northern Indian River lagoon",
    JX = "Northeast Florida",
    KY = "Florida Keys",
    LB = "Lemon Bay",
    SA = "St. Andrews Bay",
    SB = "Sarasota Bay",
    TQ = "Southern Indian River Lagoon",
    TB = "Tampa Bay"
  )
  
  db$bay <- bay_names[[toupper(config$b)]]
  if (is.null(db$bay)) db$bay <- "Unknown Bay"
  
  # Assign sampling type based upon macro substitution t
  type_names <- list(
    M = "stratified-random sampling",
    D = "directed sampling",
    F = "fixed station sampling"
  )
  
  db$type <- type_names[[toupper(config$t)]]
  if (is.null(db$type)) db$type <- "Unknown sampling type"
  
  db$bay_typ <- paste(db$bay, db$type)
  
  # Return the database list
  return(db)
}

databases <- setup(assign_databases)

# IMPORT DATA FUNCTION==========================================================

import_data <- function(config) {
  
  cat("Importing data files...\n")
  
  data_list <- list()
  
  # Number of fish database
  cat("Reading biology number data...\n")
  data_list$NUM <- here(config$in1,"FIM_biology_number.RData")
  
  # Fish lengths database
  cat("Reading biology length data...\n")
  data_list$lng <- here(config$in1, config$b, config$t, "_biology_length.RData"),
    guess_max = 32767
  )
  
  # Physical database
  cat("Reading physical data...\n")
  data_list$fld1 <- read_csv(
    paste0(config$in1, config$b, config$t, "_physical.csv"),
    guess_max = 32767
  )
  
  # Habitat database
  cat("Reading habitat data...\n")
  data_list$hab <- read_csv(
    paste0(config$in1, config$b, config$t, "_habitat.csv"),
    guess_max = 32767
  )
  
  # Hydrolab database
  cat("Reading hydrolab data...\n")
  data_list$hyd <- read_csv(
    paste0(config$in1, config$b, config$t, "_hydrolab.csv"),
    guess_max = 32767
  )
  
 # Species codes
  cat("Reading species codes...\n")
  data_list$species_codes <- read_csv(
    paste0(config$in1, "species_codes.csv"),
    col_types = cols(
      NODCCODE = col_character(),
      TSN = col_double(),
      spp_code = col_character(),
      Scientificname = col_character(),
      Commonname = col_character(),
      Manufactured_nodccode = col_double(),
      Manufactured_tsn = col_double(),
      spp_code_old = col_character(),
      datatable = col_character(),
      database = col_character()
    )
  )
  
  # FIM codes data
  cat("Reading FIM codes...\n")
  data_list$fim_codes <- read_csv(
    paste0(config$in1, "fim_codes.csv"),
    guess_max = 1000
  )
  
  cat("Data import complete.\n")
  return(data_list)
}


# PROCESS DATA FUNCTION=========================================================

process_data <- function(data_list, config) {
  
  cat("Processing data...\n")
  
  # Process physical dataset
  cat("Processing physical data...\n")
  fld2 <- data_list$fld1 %>%
    mutate(
      # Identify estuary and sampling type
      bay = substr(reference, 1, 2),
      typ = substr(reference, 3, 1),
      trip = substr(reference, 10, 2),
      
      # Create month and year variables
      month = month(date),
      year = year(date)
    ) %>%
    #Subset for bay
    filter(bay==config$b,config$t)%>%
    #Subset for type
    filter(typ==config$t)%>%
    # Subset for designated years
    filter(year >= config$b_yr & year <= config$e_yr) %>%
    # Subset for designated projects
    filter(
      Project_1 %in% c(config$p1, config$p2, config$p3, config$p4) |
        Project_2 %in% c(config$p1, config$p2, config$p3, config$p4) |
        Project_3 %in% c(config$p1, config$p2, config$p3, config$p4)
    ) %>%
    # Apply zone filter (eval/parse to handle the string expression)
    filter(eval(parse(text = config$z))) %>%
    # Combine similar gear types
    mutate(
      gr = case_when(
        gear %in% c(1, 2) ~ 2,
        gear %in% c(5) ~ 5,
        gear %in% c(11, 13, 20, 21, 25, 26, 29) | (gear >= 100 & gear <= 107) ~ 20,
        gear == 20 & stratum == 'S' ~ 19,
        gear %in% c(12, 22, 24, 27, 28) ~ 22,
        gear %in% c(10, 23) ~ 23,
        gear %in% c(153, 154) ~ 153,
        gear %in% c(155, 157, 158, 159, 160) ~ 160,
        gear %in% c(170, 431) ~ 170,
        gear == 180 ~ 180,
        (gear >= 204 & gear <= 209) | (gear >= 403 & gear <= 406) | (gear >= 409 & gear <= 410) ~ 207,
        gear %in% c(300, 301, 306) ~ 300,
        gear %in% c(350, 351, 354) ~ 350,
        gear %in% c(436) ~ 436,
        TRUE ~ NA_real_
      ),
      # Calculate effort for each gear type
      effort = case_when(
        gr == 2 ~ 31.17/100,
        gr == 5 ~ 10.1/100,
        gr %in% c(19, 20) ~ 140/100,
        gr == 22 ~ 338/100,
        gr == 23 ~ 68/100,
        gr == 160 ~ 4120/100,
        gr == 170 ~ 2209/100,
        gr == 180 ~ 465/100,
        gr == 207 ~ (soakhr + soakmin/60),
        gr == 300 ~ (dist_tow * 4 * 1853)/100,
        gr == 350 ~ 1/100,
        gr == 436 ~ 180/100,
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
    # Subset for designated gears
    filter(gr %in% c(config$g1, config$g2, config$g3, config$g4, config$g5)) %>%
    arrange(reference)
  
  # Keep only reference for merging
  ret <- fld %>%
    select(reference)
  
  # Process hydrolab dataset
  cat("Processing hydrolab data...\n")
  hyd1 <- data_list$hyd %>%
    arrange(reference)
  
  hyd_out <- hyd1 %>%
    inner_join(ret, by = "reference")
  
  hyd2 <- hyd_out %>%
    select(reference, temperature, salinity, dissolvedO2) %>%
    filter(!is.na(temperature) | !is.na(salinity))
  
  hyd <- hyd2 %>%
    group_by(reference) %>%
    summarise(
      temperature = mean(temperature, na.rm = TRUE),
      salinity = mean(salinity, na.rm = TRUE),
      dissolvedO2 = mean(dissolvedO2, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Surface measurements
  hyd3 <- hyd_out %>%
    filter(depth == 0.2) %>%
    mutate(
      temp_surf = temperature,
      sal_surf = salinity,
      do2_surf = dissolvedO2
    ) %>%
    select(reference, temp_surf, sal_surf, do2_surf)
  
  # Merge hydrolab with field data
  hyd_fld <- data_list$hyd %>%
    left_join(hyd3, by = "reference") %>%
    left_join(fld, by = "reference") %>%
    select(reference, temperature, salinity, dissolvedO2, zone, 
           temp_surf, sal_surf, do2_surf) %>%
    arrange(zone)
  
  # Calculate salinity standard deviation by zone
  sal_sd <- hyd_fld %>%
    group_by(zone) %>%
    summarise(sal_sd = sd(salinity, na.rm = TRUE), .groups = 'drop')
  
  hyd_final <- hyd_fld %>%
    left_join(sal_sd, by = "zone") %>%
    arrange(reference)
  
  # Process habitat data
  cat("Processing habitat data...\n")
  hab_processed <- process_habitat(data_list$hab, data_list$fim_codes, ret, fld)
  
  # Process weather data
  cat("Processing weather data...\n")
  wth1 <- data_list$wth %>%
    filter(Beg_end == "B") %>%
    arrange(reference)
  
  wth2 <- wth1 %>%
    inner_join(ret, by = "reference")
  
  wth <- wth2 %>%
    select(reference, CloudCover, Tide)
  
  # Process biology number data
  cat("Processing biology number data...\n")
  fish1 <- data_list$NUM %>%
    arrange(reference) %>%
    inner_join(ret, by = "reference") %>%
    mutate(
      bay = substr(reference, 1, 2),
      bio_reference = toupper(paste0(reference, SC, Splitlevel)),
      NODCCODE = str_replace_all(NODCCODE, " ", ""),
      subspecies = ifelse(nchar(NODCCODE) > 10, NODCCODE, NA_character_),
      species = substr(NODCCODE, 1, 10),
      genus = substr(NODCCODE, 1, 8),
      family = substr(NODCCODE, 1, 6),
      order = substr(NODCCODE, 1, 4),
      class = substr(NODCCODE, 1, 2),
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
    select(reference, bio_reference, species, Number, Species_record_id) %>%
    arrange(reference, species)
  
  # Total number of fish by bio_reference and species
  num <- fish1 %>%
    group_by(bio_reference, species) %>%
    summarise(n = sum(Number, na.rm = TRUE), .groups = 'drop') %>%
    left_join(fish1 %>% select(bio_reference, reference) %>% distinct(), 
              by = "bio_reference") %>%
    arrange(reference)
  
  # Process length data
  cat("Processing length data...\n")
  len1 <- data_list$lng %>%
    rename(sl = length) %>%
    filter(!is.na(sl)) %>%
    arrange(reference, Species_record_id, Length_record_id)
  
  len <- fish1 %>%
    select(reference, bio_reference, Species_record_id, species, Number) %>%
    inner_join(len1, by = c("reference", "Species_record_id")) %>%
    filter(!is.na(Number) & Number != 0) %>%
    select(reference, bio_reference, Species_record_id, Length_record_id, species, sl) %>%
    arrange(bio_reference, species)
  
  # Count lengths
  len_frq <- len %>%
    group_by(bio_reference, species, sl) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Number measured per bio_reference, species
  nl <- len_frq %>%
    group_by(bio_reference, species) %>%
    summarise(nl = sum(count), .groups = 'drop')
  
  len_final <- len_frq %>%
    left_join(nl, by = c("bio_reference", "species")) %>%
    select(bio_reference, species, nl, count, sl) %>%
    arrange(bio_reference, species)
  
  # Create combined dataset
  cat("Creating combined dataset...\n")
  com <- fld %>%
    left_join(hab_processed, by = "reference") %>%
    left_join(hyd_final, by = "reference") %>%
    left_join(wth, by = "reference") %>%
    left_join(num, by = "reference") %>%
    mutate(
      number = n,
      BottomVegCover = ifelse(BottomVegCover == 101, 51, BottomVegCover)
    ) %>%
    select(all_of(c(config$var0, config$var1, config$var2))) %>%
    select(-ovr_per, -ind_per) %>%
    arrange(bio_reference, species)
  
  # Export combined data
  write_csv(com, paste0(config$out, "tbm_c.csv"))
  
  # Final length dataset
  len_output <- len_final %>%
    inner_join(com %>% select(bio_reference, species), 
               by = c("bio_reference", "species")) %>%
    select(bio_reference, species, nl, count, sl)
  
  # Export length data
  write_csv(len_output, paste0(config$out, "tbm_l.csv"))
  
  cat("Data processing complete.\n")
  
  return(list(com = com, len = len_output, fld = fld))
}


# PROCESS HABITAT FUNCTION (Helper function)====================================

process_habitat <- function(hab, fim_codes, ret, fld) {
  
  # This is a simplified version - the full habitat processing is very complex
  # You'll need to adapt this based on your specific needs
  
  cat("Processing habitat data (simplified)...\n")
  
  # Process codes
  codes <- fim_codes %>%
    mutate(
      bcodes = ifelse(fieldname == "BottomType", code, "."),
      bvcodes = ifelse(fieldname == "BottomVeg", code, " ."),
      scodes = ifelse(fieldname == "ShoreType", code, " .")
    ) %>%
    select(fieldname, description, bcodes, bvcodes, scodes, category)
  
  bcodes <- codes %>%
    filter(fieldname == "BottomType") %>%
    mutate(code = bcodes) %>%
    select(-bcodes, -bvcodes, -scodes) %>%
    arrange(code)
  
  bvcodes <- codes %>%
    filter(fieldname == "BottomVeg") %>%
    mutate(code = bvcodes) %>%
    select(-bcodes, -bvcodes, -scodes) %>%
    arrange(code)
  
  scodes <- codes %>%
    filter(fieldname == "ShoreType") %>%
    mutate(code = scodes) %>%
    select(-bcodes, -bvcodes, -scodes) %>%
    arrange(code)
  
  # Process habitat
  hab_proc <- hab %>%
    mutate(
      Inund = case_when(
        Inund %in% c('1','2','3','4','5','6','7','8','9','10','YES') ~ 'YES',
        Inund %in% c('0','NO') ~ 'NO',
        TRUE ~ Inund
      ),
      Over = case_when(
        Over %in% c('1','2','3','4','5','6','7','8','9','10','YES') ~ 'YES',
        Over %in% c('0','NO') ~ 'NO',
        TRUE ~ Over
      )
    )
  
  hab_out <- hab_proc %>%
    inner_join(ret, by = "reference")
  
  # Simplified habitat processing - return basic structure
  # Full implementation would include bottom type, vegetation, and shore type categorization
  
  return(hab_out %>% select(reference))
}


# CHECK FUNCTION================================================================

check_data <- function(data_list, processed_data, databases) {
  
  cat("Running data checks...\n")
  
  # Set up species codes
  spp <- data_list$species_codes %>%
    mutate(species = substr(NODCCODE, 1, 10)) %>%
    select(species, Scientificname) %>%
    arrange(species)
  
  # Merge with combined data
  chck <- processed_data$com %>%
    left_join(spp, by = "species") %>%
    arrange(reference)
  
  # Check for missing values
  chck1 <- chck %>%
    filter(is.na(number) | is.na(effort))
  
  if (nrow(chck1) > 0) {
    cat("\nWarning: Found", nrow(chck1), "entries with missing number or effort values\n")
    print(chck1 %>% select(reference, historic_reference, gr, effort, number, cf))
  }
  
  # Check number of samples by gear and time
  chk2 <- chck %>%
    group_by(reference) %>%
    slice(1) %>%
    ungroup()
  
  samples_summary <- chk2 %>%
    group_by(gr, year, month) %>%
    summarise(n_samples = n(), .groups = 'drop') %>%
    arrange(gr, year, month)
  
  cat("\nNumber of collections by gear, year, and month:\n")
  print(samples_summary)
  
  # Check number of fish
  chk3 <- chck %>%
    filter(species != "9998000000")
  
  fish_summary <- chk3 %>%
    group_by(gr, year, Scientificname) %>%
    summarise(total_number = sum(number, na.rm = TRUE), .groups = 'drop') %>%
    arrange(gr, year, Scientificname)
  
  cat("\nNumber of each species collected by gear and year:\n")
  print(head(fish_summary, 20))
  
  cat("\nData checks complete.\n")
  
  return(list(
    missing_values = chck1,
    samples_summary = samples_summary,
    fish_summary = fish_summary
  ))
}

# MAIN EXECUTION FUNCTION=======================================================

run_fim_analysis <- function() {
  
  cat("=== Starting FIM Data Analysis ===\n\n")
  
  # Step 1: Setup configuration
  cat("Step 1: Setting up configuration...\n")
  config <- setup()
  
  # Step 2: Assign databases
  cat("\nStep 2: Assigning database paths...\n")
  databases <- assign_databases(config)
  cat("Bay:", databases$bay_typ, "\n")
  
  # Step 3: Import data
  cat("\nStep 3: Importing data...\n")
  data_list <- import_data(config)
  
  # Step 4: Process data
  cat("\nStep 4: Processing data...\n")
  processed_data <- process_data(data_list, config)
  
  # Step 5: Run checks
  cat("\nStep 5: Running data quality checks...\n")
  checks <- check_data(data_list, processed_data, databases)
  
  cat("\n=== FIM Data Analysis Complete ===\n")
  
  # Return all results
  return(list(
    config = config,
    databases = databases,
    data_list = data_list,
    processed_data = processed_data,
    checks = checks
  ))
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================

# To run the full analysis:
results <- run_fim_analysis()

# To access specific outputs:
combined_data <- results$processed_data$com
length_data <- results$processed_data$len
quality_checks <- results$checks