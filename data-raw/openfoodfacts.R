## code to prepare `openfoodfacts` dataset goes here
col_names <- vroom::vroom("https://fr.openfoodfacts.org/data/fr.openfoodfacts.org.products.csv", delim="\t", n_max = 1) %>%
  names

withr::with_options(list(timeout = 1800), {
  off <-  vroom::vroom(
    "https://fr.openfoodfacts.org/data/fr.openfoodfacts.org.products.csv",
    delim = "\t",
    col_names = col_names,
    col_types = "__f_T_Tc_f___f_f__f__f__f_f_________cc_c__fd_dlf_dlfdlfdfdff___ff_______ddd_dd______________dd________________dddd________d__d___dddd____dddd_dd_____d_d_d___d________d_______d____d___",
    trim_ws = TRUE,
    na = c("", "NA", "?"),
    progress = TRUE
  )}
)
openfoodfacts <- off %>%
  filter(!is.na(nutriscore_grade)) %>%
  group_by(nutriscore_grade) %>% mutate(n=n()) %>%
  sample_n(200, weight = 1/n) %>%
  select(-n, -creator) %>%
  ungroup %>%
  mutate_if(is.factor,fct_lump_min,1)
usethis::use_data(openfoodfacts, overwrite = TRUE)
