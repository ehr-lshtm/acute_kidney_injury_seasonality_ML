#' ---
#' title: K-means clustering
#' date: "`r format(Sys.time(), '%d %B %Y %H:%M')`"
#' author: Hikaru Bolt
#' output: word_document
#' 
#' ---
#'
#'
# To run this
# rmarkdown::render("09_supplement_tables.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("supplement_figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      ft.align="left",
                      fig.width = 10,
                      fig.asp = 0.8,
                      out.width = "100%")

fpp <- fp_par(text.align = "left", padding = 3)

#+ table AKI ICD10 codes , results = 'asis'

# Supplementary table 1: Acute kidney injury ICD10 codes

icd_10_codes <- fread("data/ICD10_codes_list_v2.txt")

icd_10_codes %>% 
  filter(group == "Acute Kidney Injury") %>% 
  select(icd, description) %>% 
  rename("ICD10 code" = icd,
         "Code description" = description) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 10, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 1: Acute kidney injury ICD10 codes"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

# Supplementary table 2: Diagnosis Read codes included for cluster classification

chapter_read_codes <- fread("data/chapter_read_codes.csv") %>%
  mutate(chapter_read_str = str_sub(chapter_read, 1, 1))


diagnosis_chapters <- c("A", "B", "C", "D", "E", "F", "G", "H","I", "J",
                        "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                        "U", "V", "W", "X", "Y") # exlude "Z"

chapter_read_codes %>%
  filter(chapter_read_str %in% diagnosis_chapters) %>% 
  select(chapter_read, Description) %>% 
  rename("Chapter Read code" = chapter_read) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 10, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 2: Diagnosis Read codes included for cluster classification"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

# Supplementary table 3: Diagnosis Read codes excluded for cluster classification

chapter_read_codes <- fread("data/chapter_read_codes.csv") %>%
  mutate(chapter_read_str = str_sub(chapter_read, 1, 1))


diagnosis_chapters <- c("A", "B", "C", "D", "E", "F", "G", "H","I", "J",
                        "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                        "U", "V", "W", "X", "Y") # exlude "Z"

chapter_read_codes %>%
  filter(!chapter_read_str %in% diagnosis_chapters) %>% 
  select(chapter_read, Description) %>% 
  rename("Chapter Read code" = chapter_read) %>%
  mutate(Description = tolower(Description),
         Description = str_to_sentence(Description)) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>%
  fontsize(size = 10, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 3: Diagnosis Read codes excluded for cluster classification"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

#+ table ICD10, results = 'asis'

# Supplementary table 4: Top 20 primary diagnostic ICD-10 codes of cohort 

table1 <- top_ICD %>%
  count(ICD3) %>%
  mutate(total = sum(n),
         percentage = percent((n/total), accuracy = 1, scale = 100, suffix = "%")) %>%
  arrange(-n) %>%
  slice(1:20) %>%
  mutate(ICD_diagnosis = case_when(
    ICD3 == "J18" ~ "Bronchopneumonia, unspecified",
    ICD3 == "N39" ~ "Urinary tract infection, site not specified",
    ICD3 == "A41" ~ "Other sepsis",
    ICD3 == "I50" ~ "Heart failure",
    ICD3 == "J44" ~ "Other chronic obstructive pulmonary disease",
    ICD3 == "N18" ~ "Chronic kidney disease",
    ICD3 == "E87" ~ "Other disorders of fluid, electrolyte and acid-base balance",
    ICD3 == "E86" ~ "Volume depletion",
    ICD3 == "A09" ~ "Other gastroenteritis and colitis of infectious and unspecified origin",
    ICD3 == "J22" ~ "Unspecified acute lower respiratory infection",
    ICD3 == "R29" ~ "Other symptoms and signs involving the nervous and musculoskeletal systems",
    ICD3 == "L03" ~ "Cellulitis",
    ICD3 == "B96" ~ "Other specified bacterial agents as the cause of diseases classified to other chapters",
    ICD3 == "I48" ~ "Atrial fibrillation and flutter",
    ICD3 == "J96" ~ "Respiratory failure, not elsewhere classified",
    ICD3 == "I21" ~ "Acute myocardial infarction",
    ICD3 == "E11" ~ "Non-insulin-dependent diabetes mellitus",
    ICD3 == "S72" ~ "Fracture of femur",
    ICD3 == "N13" ~ "Obstructive and reflux uropathy",
    ICD3 == "Y95" ~ "Nosocomial condition",
    ICD3 == "J69" ~ "Pneumonitis due to solids and liquids",
    ICD3 == "I63" ~ "Cerebral infarction",
    ICD3 == "K92" ~ "Haematemesis",
    ICD3 == "E10" ~ "Type 1 diabetes mellitus",
    ICD3 == "K70" ~ "Alcoholic liver disease"
  )) %>%
  select(ICD_diagnosis, ICD3, n, percentage) %>%
  rename("ICD10 chapter diagnosis" = ICD_diagnosis,
         "ICD10 chapter code" = ICD3) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 10, part = "all") %>% 
  fit_to_width(8.0) %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 4: Top 20 primary diagnostic ICD-10 codes where AKI was a secondary code during the admission"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )


table1
cat("\n")

#+ admissions time series, results = 'asis'

#### AKI monthly time series with smoothing (generalized additive model)

#' Supplementary figure 1. Normalised AKI admissions in HES-linked CPRD 2015-2029. Time series of weekly AKI admissions, 2015 - 2019, England total and by A) sex of patients B) age group C) day AKI code was recorded during the admission D) diagnostic position of AKI record E) primary diagnosis where AKI code was recorded during the admission (primary diagnoses displayed make up 36% of all primary diagnoses recorded).

ggarrange(norm_p3, norm_p5, norm_p4, norm_p2, norm_p6, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E"))

#+ aki time series and heat waves, results = 'asis'

#' Supplementary figure 2. Time series of weekly AKI admissions, 2015 - 2019, England by A) number of admissions where AKI was recorded in a primary diagnostic position B) number of admissions where AKI was recorded in a secondary diagnostic position C) diagnostic position of AKI record and number of admissions on a normalised scale. Red shaded areas denote the periods in which heat waves were declared by the Met Office in the UK.

ggarrange(primary_aki_heatwave, secondary_aki_heatwave, normalised_aki_heatwave, ncol = 1, nrow = 3, labels = c("A", "B", "C"))

#+ scree plot, results = 'asis'

#' Supplementary figure 3. Scree plot of number of dimensions and percentage of explained variance.

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 2.0))

#+ 30 indices plot, results = 'asis' 

#' Supplementary figure 4. NBClust package output of five runs. All indices were run except GAP, Gamma, Gplus and Tau.

knitr::include_graphics("outputs/indices_plot.png")

#+ cluster visualisation plot, results = 'asis'

#' Supplementary figure 5. 2D visualization of cluster assignment k = 6

print(figure_2d_plot)

cat("\n")

#+ normalised time series by cluster plot, results = 'asis'

#' Supplementary figure 6. Normalised time series of weekly AKI admissions, 2015 - 2019, England, by assigned cluster

norm_cluster_admissions

#+ time series of cluster by epi week, results = 'asis'

#' Supplementary figure 7. Time series of weekly AKI admissions, 2015 - 2019, England, by assigned cluster and epidemiological week

epi_week_cluster_figure

#+ time series of cluster by epi week by year, results = 'asis'

#' Supplementary figure 8. Time series of weekly AKI admissions, 2015 - 2019, England, by year and assigned cluster, epidemiological week 

ggarrange(
  k1_year,
  k2_year,
  k3_year,
  k4_year,
  k5_year,
  k6_year,
  ncol = 2,
  nrow = 3,
  common.legend = TRUE,
  hjust = -0.8
) %>% 
  annotate_figure(left = text_grob("Number of admissions", rot = 90))

#+ sankey diagram of sensitivity analysis of dimensions, results = 'asis'

#' Supplementary figure 9. Sankey diagram of clustering assignment by k-means for 6 clusters for 5 dimensions and 461 dimensions to show the movement of assignments. As the dimensions increased, the mental health cluster was re-assigned to other groups and replaced with a small cluster of patients with musculoskeletal or connective tissue diseases.

sankey_sensitivity_analysis

#+ table sex, results = 'asis'

# Supplementary table 5. Summary table of cluster characteristics (Percentage with the characteristic in the cohort/cluster unless otherwise specified)

sex_table <- aki_cohort_demograph %>%
  mutate(cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  )) %>%
  tabyl(gender, cluster) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  # adorn_ns() %>%
  left_join(cohort_gender, by = "gender") %>%
  select(gender, cohort, everything()) %>%
  rename("Sex" = gender) 

cat("\n")

cluster_median_age <- aki_cohort_demograph %>% 
  mutate(cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  )) %>%
  group_by(cluster) %>% 
  summarise(median = median(age),
            q25 = quantile(age, 0.25),
            q75 = quantile(age, 0.75)) %>% 
  mutate(iqr = paste0(q25, "-", q75),
         median_iqr = paste0(median, " ", paste0("(", iqr, ")")),
         age_group = "Median (IQR)") %>% 
  select(age_group, cluster, median_iqr) %>%
  pivot_wider(names_from = cluster, values_from = median_iqr)

summary_chp_interest_table <- chapter_read2_percentage %>%
  mutate(
    cluster = case_when(
      cluster == "1" ~ "4",
      cluster == "2" ~ "3",
      cluster == "3" ~ "1",
      cluster == "4" ~ "5",
      cluster == "5" ~ "6",
      cluster == "6" ~ "2",
    ),
    cluster = factor(cluster, levels = c("1", "2", "3", "4", "5", "6")),
    chapter = paste0(chapter_read2, " ", paste0("(", desc_chapter2, ")")),
    freq = paste0(freq, "", paste0("%")
                  # freq = paste0(" ", paste0("(", n, ")"))
                  )
  ) %>%
  filter(chapter_read2 %in% interest_chapters) %>%
  arrange(cluster) %>% 
  select(cluster, chapter, freq) %>%
  pivot_wider(names_from = cluster, values_from = freq) %>%
  left_join(cohort_chap2 %>% mutate(cohort = paste0(cohort, "", paste0("%"))), by = "chapter") %>%
  select(chapter, cohort, everything()) %>%
  add_row(chapter = "Chapter headings") %>% 
  mutate(chapter = factor(chapter, levels = c( "Chapter headings",
           "N2 (Rheumatism, excluding the back)",
           "F4 (Disorders of eye and adnexa)",
           "G2 (Hypertensive disease)",
           "G3 (Ischaemic heart disease)",
           "G5 (Other forms of heart disease)",
           "G8 (Vein, lymphatic and circulatory diseases NOS)",
           "M0 (Skin and subcutaneous tissue infections)",
           "M1 (Other skin and subcutaneous tissue inflammatory conditions)",
           "M2 (Other skin and subcutaneous tissue disorders)",
           "E2 (Neurotic, personality and other nonpsychotic disorders)",
           "C3 (Other metabolic and immunity disorders)",
           "C1 (Other endocrine gland diseases)",
           "H3 (Chronic obstructive pulmonary disease)",
           "K1 (Other urinary system diseases)",
           "K5 (Other female genital tract disorders)",
           "K0 (Nephritis, nephrosis and nephrotic syndrome)",
           "J6 (Liver, biliary, pancreas + gastrointestinal diseases NEC)"
         ))) %>% 
  arrange(chapter) %>% 
  rename("Variable" = "chapter")

summary_table <- sex_table %>%
  rename("Variable" = "Sex") %>%
  add_row(Variable = "Sex") %>%
  mutate(Variable = factor(Variable, levels = c("Sex", "Female", "Male"))) %>% 
  arrange(Variable) %>% 
  bind_rows(cluster_median_age %>% left_join(cohort_age %>%
                                               filter(age_group == "Median (IQR)"), by = "age_group") %>%
              rename( "Variable" = "age_group") %>% add_row(Variable = "Age") %>%
              mutate(Variable = factor(Variable, levels = c("Age", "Median (IQR)"))) %>%
              arrange(Variable)) %>% 
  bind_rows(summary_chp_interest_table) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 2.0, part = "all") %>% 
  flextable::autofit() %>%
  fontsize(size = 8, part = "all") %>% 
  fit_to_width(7.0) %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 5. Summary table of cluster characteristics (Percentage with the characteristic in the cohort/cluster unless otherwise specified)"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )
  
# call summary table

summary_table

cat("\n")

#+ table age, results = 'asis'

# Supplementary table 6: Age breakdown of cohort and clusters 

age_table <- aki_cohort_demograph %>%
  mutate(cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  )) %>%
  tabyl(age_group, cluster) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns() %>%
  bind_rows(cluster_median_age) %>% 
  left_join(cohort_age, by = "age_group") %>%
  select(age_group, cohort, everything()) %>%
  rename("Age group" = age_group) %>% 
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit() %>% 
  fit_to_width(6.5) %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 5. Summary table of cluster characteristics (Percentage with the characteristic in the cohort/cluster unless otherwise specified)."),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

age_table

#+ table chapter 2 percentage top 15, results = 'asis'

# Supplementary table 7: Top 15 codes of cohort and clusters at Read code chapter level 2 (Percentage with the code in the cohort/cluster)

chp2_table_top15 <- chapter_read2_percentage %>%
  mutate(
    cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  ), 
  cluster = factor(cluster, levels = c("1", "2", "3", "4", "5", "6"))) %>%
  group_by(cluster) %>% 
  arrange(desc(freq)) %>% 
  mutate(row = 1:n()) %>% 
  ungroup() %>% 
  filter(row < 16) %>% 
  mutate(freq = paste0(freq, "", paste0("%")),
         chapter_percentage = paste0(desc_chapter2, " ", paste0("(", freq, ")"))) %>% 
  select(cluster, row, chapter_percentage) %>%
  arrange(cluster) %>% 
  pivot_wider(names_from = cluster, values_from = chapter_percentage) %>%
  left_join(cohort_chap2_top15, by = "row") %>% 
  select(row, cohort, everything()) %>% 
  rename(" " = row) %>% 
  flextable::flextable() %>%
  fontsize(size = 8, part = 'all') %>%
  padding(padding = 2.0, part = "all") %>% 
  flextable::autofit() %>% 
  set_table_properties(layout = "autofit") %>% 
  align_text_col(align = "left") %>% 
  valign(valign = "top") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 7: Top 15 codes of cohort and clusters at Read code chapter level 2 (Percentage with the code in the cohort/cluster)."),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

chp2_table_top15
cat("\n")

#+ table chapter 3 percentage top 15, results = 'asis'

# Supplementary table 8: Top 15 codes of cohort and clusters at Read code chapter level 3 (Percentage with the code in the cohort/cluster)

chp3_table_top15 <- chapter_read3_percentage %>%
  mutate(cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  )) %>%
  group_by(cluster) %>% 
  arrange(desc(freq)) %>% 
  mutate(row = 1:n()) %>% 
  ungroup() %>% 
  filter(row < 16) %>% 
  mutate(freq = paste0(freq, "", paste0("%")),
         chapter_percentage = paste0(desc_chapter3, " ", paste0("(", freq, ")"))) %>% 
  select(cluster, row, chapter_percentage) %>%
  arrange(cluster) %>% 
  pivot_wider(names_from = cluster, values_from = chapter_percentage) %>%
  left_join(cohort_chap3_top15, by = "row") %>% 
  select(row, cohort, everything()) %>%
  rename(" " = row) %>% 
  flextable::flextable() %>%
  fontsize(size = 8, part = 'all') %>% 
  padding(padding = 2.0, part = "all") %>% 
  flextable::autofit() %>% 
  set_table_properties(layout = "autofit") %>% 
  align_text_col(align = "left") %>% 
  valign(valign = "top") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 8: Top 15 codes of cohort and clusters at Read code chapter level 3 (Percentage with the code in the cohort/cluster)."),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

chp3_table_top15
cat("\n")

#+ table chapter 2 percentage, results = 'asis'

# Supplementary table 9: Code characteristics of cohort and clusters at Read code chapter level 2 (Percentage with the code in the cohort/cluster). Presenting 100 of 164 codes total.

chp2_table <- chapter_read2_percentage %>%
  mutate(
    cluster = case_when(
      cluster == "1" ~ "4",
      cluster == "2" ~ "3",
      cluster == "3" ~ "1",
      cluster == "4" ~ "5",
      cluster == "5" ~ "6",
      cluster == "6" ~ "2",
    ),
    cluster = factor(cluster, levels = c("1", "2", "3", "4", "5", "6")),
    chapter = paste0(chapter_read2, " ", paste0("(", desc_chapter2, ")")),
    freq = paste0(freq, "", paste0("%"))
    ) %>%
  arrange(cluster) %>% 
  select(cluster, chapter, freq) %>%
  pivot_wider(names_from = cluster, values_from = freq) %>%
  left_join(cohort_chap2, by = "chapter") %>%
  select(chapter, cohort, everything()) %>%
  arrange(desc(cohort)) %>%
  mutate(cohort = paste0(cohort, "", paste0("%"))) %>% 
  slice(1:100) %>% 
  flextable::flextable() %>%
  padding(padding = 2.0, part = "all") %>% 
  flextable::autofit() %>% 
  fit_to_width(6.5) %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 9: Code characteristics of cohort and clusters at Read code chapter level 2 (Percentage with the code in the cohort/cluster). Presenting 100 of 164 codes total."),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

chp2_table
cat("\n")



#+ table chapter 3 percentage, results = 'asis'

# Supplementary table 10: Code characteristics of cohort and clusters at Read code chapter level 3 (Percentage with the code in the cohort/cluster). Presenting 100 of 736 codes total.

chp3_table <- chapter_read3_percentage %>%
  mutate(
    cluster = case_when(
      cluster == "1" ~ "4",
      cluster == "2" ~ "3",
      cluster == "3" ~ "1",
      cluster == "4" ~ "5",
      cluster == "5" ~ "6",
      cluster == "6" ~ "2",
    ),
    chapter = paste0(chapter_read3, " ", paste0("(", desc_chapter3, ")")),
    freq = paste0(freq, "", paste0("%"))
  ) %>%
  arrange(cluster) %>% 
  select(cluster, chapter, freq) %>%
  pivot_wider(names_from = cluster, values_from = freq) %>%
  left_join(cohort_chap3, by = "chapter") %>%
  select(chapter, cohort, everything()) %>%
  arrange(desc(cohort)) %>%
  mutate(cohort = paste0(cohort, "", paste0("%"))) %>% 
  slice(1:100) %>% 
  flextable::flextable() %>%
  padding(padding = 2.0, part = "all") %>% 
  flextable::autofit() %>% 
  fit_to_width(6.5) %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 10: Code characteristics of cohort and clusters at Read code chapter level 3 (Percentage with the code in the cohort/cluster). Presenting 100 of 736 codes total."),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )


chp3_table
cat("\n")

