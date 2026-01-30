# Processing FWC Fisheries-Independent Monitoring (FIM) Data

The following code has been revised from SAS into R (see [notes](https://github.com/kflahertywalia/FIM_data_proc/blob/main/SAS_to_R_conversion_notes)) to process FIM data into multi-species (community data) and single-species datasets for analyses. Two different pathways are used to process each type of dataset. Each processing pathway has options to subset the data further by monitoring type (pulled out as a integer (x) in the data set name; m for standard monitoring, d for directed sampling, null for all monitoring, etc.), gear type, location (zone, bay segment, river), years or months sampled, etc. Use the most appropriate subset for the data analyses you are conducting.

Before processing, download the most current FIM data from () and, if needed, run [***tb_subset.R***](https://github.com/kflahertywalia/FIM_data_proc/blob/main/R/tb_subset.R) to subset all FIM data into Tampa Bay only.

## Multi-species data (in progress):

1.  Run [***multi_comb_data_proc.R***](https://github.com/kflahertywalia/FIM_data_proc/blob/main/R/multi_comb_dat_proc.R) to process and subset the Tampa Bay data into two files:

    -   Count data (tbx_c.RData)

    -   Length data (tbx_l.RData):

2.  Run ***matrix_out.R*** to format data for multivariate analyses packages such as PRIMER by creating pseudospecies (juveniles/adults, etc.) based on length data and transposing into three tables:

    -   A species list defining column abbreviations: tbx_spp.R -

    -   Samples as rows, species and factors as columns (tbx_num.R)

    -   Samples as rows, environmental data and factors as columns (tbx_env.R)

## Single-species data (to do):

Run ***spp_comb_data_proc.R*** to process and subset the Tampa Bay data for one species and desired length into two files:

-   Count data (tbx_c.R)

    -   Subset of the count data

    -   Fills no catch samples with zeros

-   length data (tbx_l.R) - subset the length data
