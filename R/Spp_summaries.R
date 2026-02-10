
spp <- com %>%
  group_by(Scientificname,Commonname,Taxa_Type)%>%
  summarise(total_count = sum(number, na.rm = TRUE),
            n_observations=n())
write_csv(spp, here("Output", "tb_fim_species.csv"))

gr_summary_data <- com %>%
  group_by(gr, Scientificname) %>%
  summarise(
    total_count = sum(number, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = gr,
    values_from = c(total_count, n_observations),
    values_fill = 0,
    names_sep = "_"
  ) %>%
  arrange(Scientificname)
# Get the gear types
gear_types <- sort(unique(spp$gr[spp$gr != "000"]))

# Reorder columns to group by gear
new_order <- c("Scientificname")
for (gr in gear_types) {
  new_order <- c(new_order, 
                 paste0("total_count_", gr),
                 paste0("n_observations_", gr))
}

tc_summary_data <- tc_summary_data %>%
  select(all_of(new_order[new_order %in% names(.)]))

write_csv(tc_summary_data, here("Output", "tc_gr_species.csv"))