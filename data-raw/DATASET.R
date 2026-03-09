filename_cytometry <- file.path(path, "inst", "data", "20250701_cytometry.xlsx")

header <- read_excel(
  filename_cytometry,
  n_max  = 1
)

cytometry_raw <- read_excel(
  filename_cytometry,
  skip = 1
) %>% rename(`CD3 / tot` = "CD3...213") %>%
  rename(CD3 = "CD3...212")

cytometry_raw[cytometry_raw == "NA"] <- NA


header_clean <- colnames(header) %>%
  str_remove_all("\\.\\.\\.[0-9]+") %>%
  str_remove_all("\\s*\\([^\\)]+\\)") %>%
  str_trim()

original_names <- colnames(cytometry_raw)
group_labels <- na_if(header_clean, "") %>%
  zoo::na.locf(na.rm = FALSE)

new_colnames <- original_names
new_colnames <- if_else(
  !is.na(group_labels) & seq_along(original_names) > 3,
  paste0(original_names, " (", group_labels, ")"),
  original_names
)

colnames(cytometry_raw) <- new_colnames

cytometry_raw <- cytometry_raw %>%
  group_by(is_p00 = str_starts(id2, "P00")) %>%
  mutate(
    p00_seq = ifelse(is_p00, cumsum(is_p00), NA)
  ) %>%
  ungroup() %>%
  mutate(
    timepoint = case_when(
      p00_seq %in% 1:10 ~ "100",
      p00_seq %in% 11:20 ~ "365",
      p00_seq %in% 21:30 ~ "730",
      TRUE ~ timepoint
    )
  ) %>%
  select(-is_p00, -p00_seq)

use_data(cytometry_raw,  overwrite = TRUE)

cytometry2 <- cytometry_raw
colnames(cytometry2) <- format_cytometry(colnames(cytometry2))

group_labels <- str_extract(colnames(cytometry2), "\\(([^)]+)\\)") %>%
  str_remove_all("[()]")

group_labels %>%
  na.omit() %>%
  format_cytometry() %>%
  fct_inorder() %>%
  fct_count() %>%
  kable0()

celltype_lists <- split(colnames(cytometry2)[!is.na(group_labels)],
                        group_labels[!is.na(group_labels)])


###


data("cytometry_raw")

cytometry <- filter(cytometry_raw, timepoint %in% c("C1", "100")) %>%
  select(-c(patient, timepoint)) %>%
  as.data.frame() %>%
  column_to_rownames("id2") %>%
  set_colnames(colnames(.) %>% format_cytometry()) %>%
  mutate(across(all_of(colnames(.)), ~ as.numeric(.)))

use_data(cytometry, overwrite = TRUE)


###


data("clinic")
data("cytometry_raw")
clinic <- filter(clinic, Group == "ER")

X_raw0 <- left_join(
  rownames_to_column(clinic, "id2"),
  cytometry_raw,
  by = "id2"
) %>%
  mutate(id = paste0(id2, ".", timepoint))

cytometry_ER <- select(X_raw0, -all_of(c("id2", "patient", "timepoint", colnames(clinic)))) %>%
  as.data.frame() %>%
  column_to_rownames("id") %>%
  set_colnames(colnames(.) %>% format_cytometry()) %>%
  mutate(across(all_of(colnames(.)), ~ as.numeric(.)))

ER_timepoint <- pull(X_raw0, timepoint) %>% as.factor() %>% set_names(rownames(X_raw))

ER_survival <- pull(X_raw0, "Days to last follow-up") %>% set_names(rownames(X_raw))

use_data(cytometry_ER, overwrite = TRUE)
use_data(ER_timepoint, overwrite = TRUE)
use_data(ER_survival, overwrite = TRUE)


####


cytometry_group <- left_join(
  rownames_to_column(cytometry) %>% select(rowname),
  rownames_to_column(clinic) %>% select("rowname", "Group")
) %>%
  mutate(Group = as.character(Group)) %>%
  mutate(Group = ifelse(
    str_starts(rowname, "P11") & is.na(Group),
    "ER",
    Group
  )) %>%
  mutate(Group = replace_na(Group, "HS")) %>%
  mutate(Group = str_replace_all(Group, "FR", "NR")) %>%
  pull(Group) %>%
  as.factor()

use_data(cytometry_group, overwrite = TRUE)

rownames(cytometry) %>%
  str_remove_all("\\..*") %>%
  table() %>%
  as.data.frame() %>%
  kable0()
# Si tous les ER et les LR avaient ete mesure a la meme date, il y aurait eu un effet difference de temporalite entre ceux ci et FR, mais vu qu'ils ont ete mesures a une date differences, on peut logiquement les pooler avec les FR qui ont tous ete mesures a 100j. Même si theoriquement il y a un petit effet du fait que tous les FR ont ete mesures a la même date, il devrait y avoir moins d'heterogenite au niveau de la date


####


data("cytometry_raw")

cytometry_fr <- filter(cytometry_raw, !str_detect(timepoint, "C")) %>%
  mutate(id2 = paste0(timepoint, ".", str_remove_all(id2, "P00\\."))) %>%
  select(-c(patient, timepoint)) %>%
  as.data.frame() %>%
  column_to_rownames("id2") %>%
  set_colnames(colnames(.) %>% format_cytometry()) %>%
  mutate(across(all_of(colnames(.)), ~ as.numeric(.)))

use_data(cytometry_fr, overwrite = TRUE)

###
library("janitor")
library("readxl")
library("openxlsx")
path_data <- file.path("C:", "Users", "etien", "DATA", "hlebuhanec")
clinic2 <- read_xlsx(
  file.path(path_data, "recueil Patients pruriseq a jour 2.xlsx"),
  .name_repair = make_clean_names
  ) %>%
  select(-all_of(c("telephone", "mail", "adresse", "nom", "prenom", "x"))) %>%
  mutate(
    date_naissance = case_when(
      date_naissance == "1" | date_naissance == 1 ~ NA_Date_,
      grepl("^[0-9]+$", date_naissance) ~ convertToDate(as.numeric(date_naissance)),
      grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", date_naissance) ~ as.Date(date_naissance, format = "%d/%m/%Y"),

      grepl("^[0-9]{4}/[0-9]{4}$", date_naissance) ~ as.Date(paste0(substr(date_naissance, 1, 2), "/",
                                                              substr(date_naissance, 3, 4), "/",
                                                              substr(date_naissance, 6, 9)),
                                                       format = "%d/%m/%Y"),

      TRUE ~ NA_Date_
    )
  ) %>%
  remove_empty() %>%
  filter(str_detect(patient, "^(DA|HD|PN)")) %>%
  mutate(groupe = str_extract(patient, "^.{2}")) %>%
  relocate(groupe, .before = patient) %>%
  mutate(across(everything(), ~ type.convert(., as.is = TRUE))) %>%
  mutate(
    sexe = factor(sexe, levels = c(0, 1), labels = c("M", "F")),
    age = case_when(
      age == 24645 ~ 67,
      age == 28520 ~ 78,
      TRUE ~ age
    )
  )

use_data(clinic2, overwrite = TRUE)
