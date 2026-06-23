###Subset all FIM data from GEI project data to selected estuaries

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
b <- "TB"

# Pathways to databases
# Location of multi-year historic data for all estuaries
# Google Drive path = TBEP_General/09_Tech_Projects/Gulf Ecosystem Initiative/Data/FIM/FIM_Data/
in1 <- drive_get("https://drive.google.com/drive/u/0/folders/1pAuF3vakLXWw8wvpMgkl9YU6TvhJkKiJ")

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
fim_num <- FIM_BiologyCounts

# Import biology length data
fim_lng <- FIM_BiologyLengths

# Import physical data
fim_fld <- FIM_PhysicalMaster

# Import habitat data
fim_hab <- FIM_Habitat

# Import hydrolab data
fim_hyd <- FIM_HydroLab

#Subset data to Tampa Bay

assign(paste0(b, "_FIM_PhysicalMaster"), fim_fld %>%
         filter(Bay == b))

# Create retention list
  ret <- get(paste0(b, "_FIM_PhysicalMaster")) %>%
  select(Reference)

# Subset datasets by TB reference
assign(paste0(b, "_FIM_BiologyCounts"), fim_num %>%
    arrange(Reference) %>%
    inner_join(ret, by = "Reference")) 

assign(paste0(b, "_FIM_Hydrolab"), fim_hyd %>%
  arrange(Reference)%>%
  inner_join(ret, by = "Reference"))  

assign(paste0(b, "_FIM_Habitat"), fim_hab %>%
  arrange(Reference)%>%
  inner_join(ret, by = "Reference"))  

# Subset length data
assign(paste0(b, "_FIM_BiologyLengths"), fim_lng %>%
  arrange(Reference) %>%
  inner_join(ret, by = "Reference"))

#output subsets

save(list = paste0(b, "_FIM_PhysicalMaster"), file = here("Data", paste0(b, "_FIM_PhysicalMaster.RData")))
save(list = paste0(b, "_FIM_BiologyCounts"),  file = here("Data", paste0(b, "_FIM_BiologyCounts.RData")))
save(list = paste0(b, "_FIM_BiologyLengths"), file = here("Data", paste0(b, "_FIM_BiologyLengths.RData")))
save(list = paste0(b, "_FIM_Habitat"),        file = here("Data", paste0(b, "_FIM_Habitat.RData")))
save(list = paste0(b, "_FIM_Hydrolab"),       file = here("Data", paste0(b, "_FIM_Hydrolab.RData")))
save(FIM_ReferenceCodes, file = here("Data", "FIM_ReferenceCodes.RData"))
 