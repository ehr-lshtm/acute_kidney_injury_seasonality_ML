#' ---
#' title: K-means clustering
#' date: "`r format(Sys.time(), '%d %B %Y %H:%M')`"
#' author: Hikaru Bolt
#' output:
#'   html_document:
#'     df_print: paged
#'     highlight: kate
#'     theme: spacelab
#'     toc: yes
#'     toc_float: yes
#'     fig_height: 6
#'     fig_width: 10
#'     mathjax: null
#' ---
#'
#'
# To run this
# rmarkdown::render("07_figures.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)
#+ setup

### load data

source("load_data.R")

ind.coord <- ind.coord %>%
  select(1:5)

#+ descriptive data, results = 'asis'

#' Total number of admissions

g_hes_aki_spell %>% 
  count()

#' Total number of patients included

g_hes_aki_spell %>%
  distinct(patid) %>% 
  count()

#' Total number of admissions

g_hes_aki_spell %>%
  group_by(epiyear) %>% 
  count()

#' Age and sex of patient cohort

g_hes_aki_spell_full %>%
  distinct(patid, gender) %>% 
  mutate(total = n()) %>% 
  count(gender, total) %>%
  mutate(prop = round((n/total)*100, 2))

g_hes_aki_spell_full %>%
  distinct(patid, age_at_admission) %>%
  summarise(median = median(age_at_admission),
            q25 = quantile(age_at_admission, probs = 0.25),
            q75 = quantile(age_at_admission, probs = 0.75))

#+ descriptive time series, results = 'asis', figure.height = 12

#' # Descriptive time series

#### AKI monthly time series with smoothing (generalized additive model)

p1 <- g_hes_aki_spell %>%
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ggplot(aes(week_date, n)) +
  geom_line() +
  theme_classic() +
  labs( y = "Number of admissions") +
  scale_y_continuous(limits=c(0,1200), breaks=seq(0,1200, by = 200)) +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0),
        ) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2016-01-01", "%Y-%m-%d"),
    xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2018-01-01", "%Y-%m-%d"),
    xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  )

p2 <- g_hes_aki_spell %>%
  mutate(
    year = year(admidate),
    age_at_admission = year - yob,
    age_group = case_when(
      age_at_admission < 65 ~ "<65",
      age_at_admission >= 65 &
        age_at_admission <= 74 ~ "65-74",
      age_at_admission >= 75 &
        age_at_admission <= 84 ~ "75-84",
      age_at_admission >= 85 ~ ">84"
    ),
    age_group = factor(age_group, levels = c("<65", "65-74", "75-84", ">84"))) %>% 
  group_by(age_group) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = age_group), size = 0.5) +
  theme_classic() +
  labs(color = "Age group") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits=c(0,400), breaks=seq(0,400, by = 100)) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
        ) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2016-01-01", "%Y-%m-%d"),
    xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2018-01-01", "%Y-%m-%d"),
    xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  )

total_incidence <- g_hes_aki_spell %>%
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>% 
  mutate(gender = "Total")

p3 <- g_hes_aki_spell %>%
  group_by(gender) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ungroup() %>%
  bind_rows(total_incidence) %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = gender)) +
  theme_classic() +
  labs(color = "Sex") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits=c(0,1200), breaks=seq(0,1200, by = 200)) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
        ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

p4 <- g_hes_aki_spell_full %>%
  mutate(
    admiyear = year(admidate),
    d_order_2 = case_when(d_order < 2 ~ "Primary",
                          d_order > 1 ~ "Secondary"),
    d_order_2 = factor(d_order_2, levels = c("Primary", "Secondary"))
  ) %>%
  group_by(patid, spno) %>%
  arrange(epistart) %>% 
  mutate(id = row_number()) %>%         # selecting first record of AKI in the admission
  ungroup() %>% 
  filter(id == 1) %>% 
  distinct(patid, spno, d_order_2, admidate) %>%
  group_by(d_order_2) %>%
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = d_order_2)) +
  theme_classic() +
  labs(color = "Diagnostic position") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1000), breaks = seq(0, 1000, by = 200)) +
  scale_colour_manual(values = cbbPalette) +
  theme(
    legend.position = 'top',
    legend.justification = 'right',
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.margin=margin(0,0,0,0)
  ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

p5 <- g_hes_aki_spell_full %>%
  mutate(admiyear = year(admidate),
         aki_day_4 = case_when(
           aki_day <2 ~ "0-1",
           aki_day > 1 & aki_day < 5 ~ "2-4",
           aki_day > 4 ~ ">4"),
         aki_day_4 = factor(aki_day_4, levels = c("0-1", "2-4", ">4"))) %>% 
  group_by(patid, spno) %>%
  arrange(epistart) %>% 
  mutate(id = row_number()) %>%         # selecting first record of AKI in the admission
  ungroup() %>% 
  filter(id == 1, 
         !is.na(aki_day_4)) %>%
  distinct(patid, spno, aki_day_4, admidate) %>% 
  group_by(aki_day_4) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = aki_day_4)) +
  theme_classic() +
  labs(color = "Day of AKI code") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits=c(0,1200), breaks=seq(0,1200, by = 200)) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
        ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

# select patients where aki was a secondary code

g_hes_secondary_aki <- g_hes_aki_spell_full %>%
  mutate(
    admiyear = year(admidate),
    d_order_2 = case_when(d_order < 2 ~ "Primary",
                          d_order > 1 ~ "Secondary"),
    d_order_2 = factor(d_order_2, levels = c("Primary", "Secondary"))
  ) %>%
  group_by(patid, spno) %>%
  arrange(epistart) %>% 
  mutate(id = row_number()) %>%         # selecting first record of AKI in the admission
  ungroup() %>% 
  filter(id == 1) %>% 
  distinct(patid, spno, d_order_2, admidate) %>% 
  filter(d_order_2 == "Secondary")
  
# join with diagnosis data to find what the primary diagnosis was for those with secondary aki

top_ICD <- g_hes_diagnosis_epi %>%
  inner_join(g_hes_aki_spell %>% distinct(patid, spno, admidate),
            by = c("patid", "spno")) %>% 
  mutate(d_order_2 = case_when(d_order < 2 ~ "Primary",
                                   d_order > 1 ~ "Secondary"),
         ICD3 = substr(ICD, 1, 3),
         epistart = dmy(epistart)) %>%
  filter(d_order_2 == "Primary") %>% 
  group_by(patid, spno) %>%
  arrange(epistart) %>% 
  mutate(id = row_number()) %>%         # selecting first record of AKI in the admission
  ungroup() %>% 
  filter(id == 1) %>% 
  distinct() %>% 
  inner_join(g_hes_secondary_aki %>% select(patid, spno), by = c("patid", "spno"))
  
p6 <- top_ICD %>%
  filter(  ICD3 == "J18" |
           ICD3 == "N39" |
           ICD3 == "A41" |
           ICD3 == "I50" 
           # ICD3 == "J44"
         ) %>%
  mutate(ICD_diagnosis = case_when(
    ICD3 == "J18" ~ "Pneumonia",
    ICD3 == "N39" ~ "UTI",
    ICD3 == "A41" ~ "Sepsis",
    ICD3 == "I50" ~ "Heart failure",
    # ICD3 == "J44" ~ "COPD",
  ),
  ICD_diagnosis = factor(ICD_diagnosis, levels = c("Pneumonia", "Sepsis", "UTI", "Heart failure"))) %>% 
  group_by(ICD_diagnosis) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = ICD_diagnosis)) +
  theme_classic() +
  labs(color = "ICD10") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits=c(0,200), breaks=seq(0,200, by = 50)) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

ggarrange(p3, p5, p4, p2, p6, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E"))

#+ cohort summary processing

#########################################
########## Cohort summary ############
#########################################

set.seed(123)
km.res <- kmeans(ind.coord, 1, nstart = 25)

# Add clusters obtained using the K-means algorithm

ind.coord$cluster <- factor(km.res$cluster)
ind.coord1 <- ind.coord               # This step is to combine the cluster changes per patient over time. Combined at end of script.

# cluster groups

aki_cohort_demograph$cluster <- factor(km.res$cluster)
aki_cohort_chapter3$cluster <- factor(km.res$cluster)
aki_cohort_chapter2$cluster <- factor(km.res$cluster)

# prepare table percentages

source("08_k_means_results_chapter3.R")

# prepare table of characteristics of cohort

cohort_gender <- aki_cohort_demograph %>%
  tabyl(gender, cluster) %>%
  rename(cohort = `1`) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0)

cohort_gender_figure <- aki_cohort_demograph %>% 
  mutate(total = n()) %>% 
  count(gender, total) %>%
  mutate(prop = round((n/total)*100, 0),
         cluster = "cohort")

cohort_median_age <- aki_cohort_demograph %>% 
  group_by(cluster) %>% 
  summarise(median = median(age),
            q25 = quantile(age, 0.25),
            q75 = quantile(age, 0.75)) %>% 
  mutate(iqr = paste0(q25, "-", q75),
         median_iqr = paste0(median, " ", paste0("(", iqr, ")")),
         age_group = "Median (IQR)") %>% 
  select(age_group, median_iqr) %>% 
  rename(cohort = median_iqr)

cohort_age <- aki_cohort_demograph %>%
  tabyl(age_group, cluster) %>%
  rename(cohort = `1`) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns() %>% 
  bind_rows(cohort_median_age)

cohort_age_figure <- aki_cohort_demograph %>% 
  mutate(total = n()) %>% 
  count(age_group, total) %>%
  mutate(prop = round((n/total)*100, 0),
         cluster = "cohort")

#+ cohort diagnoses

# prepare table of characteristics of cohort

cohort_chap3 <- chapter_read3_percentage %>%
  mutate(
    chapter = paste0(chapter_read3, " ", paste0("(", desc_chapter3, ")")),
    # freq = paste0(freq, "", paste0("%"))
    ) %>%
  select(cluster, chapter, freq) %>%
  pivot_wider(names_from = cluster, values_from = freq) %>%
  rename(cohort = `1`)

cohort_chap3_top15 <- chapter_read3_percentage %>% 
  arrange(desc(freq)) %>% 
  mutate(row = 1:n()) %>% 
  filter(row < 16) %>% 
  mutate(
    freq = paste0(freq, "", paste0("%")),
    chapter_percentage = paste0(desc_chapter3, " ", paste0("(", freq, ")"))) %>% 
  select(cluster, row, chapter_percentage) %>%
  arrange(cluster) %>% 
  pivot_wider(names_from = cluster, values_from = chapter_percentage) %>%
  rename(cohort = `1`)

cohort_chap2 <- chapter_read2_percentage %>%
  mutate(
    chapter = paste0(chapter_read2, " ", paste0("(", desc_chapter2, ")")),
    # freq = paste0(freq, "", paste0("%"))
    ) %>%
  select(cluster, chapter, freq) %>%
  pivot_wider(names_from = cluster, values_from = freq) %>%
  rename(cohort = `1`)


cohort_chap2_top15 <- chapter_read2_percentage %>%
  arrange(desc(freq)) %>%
  mutate(row = 1:n()) %>%
  filter(row < 16) %>%
  mutate(
    freq = paste0(freq, "", paste0("%")),
    chapter_percentage = paste0(desc_chapter2, " ", paste0("(", freq, ")"))) %>% 
  select(cluster, row, chapter_percentage) %>%
  arrange(cluster) %>% 
  pivot_wider(names_from = cluster, values_from = chapter_percentage) %>%
  rename(cohort = `1`)


####################################################################################
# k-means clustering = 6
####################################################################################

#+ k-means clustering

for (i in 2:6)
  
{
  ind.coord <- ind.coord %>% select(1:5)
  set.seed(123)
  km.res <- kmeans(ind.coord, i, nstart = 25, iter.max = 100, algorithm = "MacQueen")
  
  # Add clusters obtained using the K-means algorithm
  
  ind.coord$cluster <- factor(km.res$cluster)
  assign(paste0("ind.coord", i), ind.coord)
  
}

# cluster groups

aki_cohort_demograph$cluster <- factor(km.res$cluster)
aki_cohort_chapter3$cluster <- factor(km.res$cluster)
aki_cohort_chapter2$cluster <- factor(km.res$cluster)

# prepare table percentages

source("08_k_means_results_chapter3.R")

#' # Clustering characteristics

#+ sankey diagram, results = 'asis', fig.fullwidth = TRUE

#' number of patients included in clustering

ind.coord %>% count()

## 

ind.coord1$k2 <- factor(ind.coord2$cluster)
ind.coord1$k3 <- factor(ind.coord3$cluster)
ind.coord1$k4 <- factor(ind.coord4$cluster)
ind.coord1$k5 <- factor(ind.coord5$cluster)
ind.coord1$k6 <- factor(ind.coord6$cluster)

# cluster count

ind.coord %>% 
  count(cluster)

# sankey diagram

df_ind_k6 <- ind.coord1 %>%
  mutate(
    cluster = case_when(cluster == 1 ~ "Cohort"),
    k2 = case_when(
      k2 == 1 ~ "1. Less multimorbid",
      k2 == 2 ~ "2. Multimorbid"
    ),
    k3 = case_when(
      k3 == 1 ~ "3. Female multimorbid",
      k3 == 2 ~ "2. General AKI",
      k3 == 3 ~ "1. Less multimorbid"
    ),
    k4 = case_when(
      k4 == 1 ~ "4. High level coding",
      k4 == 2 ~ "3. Female multimorbid",
      k4 == 3 ~ "2. General AKI",
      k4 == 4 ~ "1. Less multimorbid"
    ),
    k5 = case_when(
      k5 == 1 ~ "3. General AKI",
      k5 == 2 ~ "4. Female multimorbid",
      k5 == 3 ~ "5. High level coding",
      k5 == 4 ~ "1. Less multimorbid",
      k5 == 5 ~ "2. Mental health"
    ),
    k6 = case_when(
      k6 == 1 ~ "4. General AKI (n = 30,654)",
      k6 == 2 ~ "3. Established risk factors (n = 20,098)",
      k6 == 3 ~ "1. Less multimorbid (n = 59,586)",
      k6 == 4 ~ "5. Female multimorbid (n = 11,609)",
      k6 == 5 ~ "6. High level coding (n = 811)",
      k6 == 6 ~ "2. Mental health (n = 7,867)"
    )
  ) %>%
  make_long(cluster, k2, k3, k4, k5, k6)                          # add k7

df_ind_k6 %>%
mutate(x = case_when(
  x == "cluster" ~ "k = 1",
  x == "k2" ~ "k = 2",
  x == "k3" ~ "k = 3",
  x == "k4" ~ "k = 4",
  x == "k5" ~ "k = 5",
  x == "k6" ~ "k = 6"
)) %>%
ggplot( aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  scale_fill_viridis_d(option = "D", alpha = 0.95) +
  theme_sankey(base_size = 18) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
        ) +
  geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE) +
  geom_sankey_label(size = 3, color = "black", fill= "white", hjust = 0.7)

ggsave("cluster_sankey.jpg")


#' Cluster characteristics

#+ k means visualisations, results = 'asis', figure.height = 10

interest_chapters <- c("N2", "G2", "C1", "G5", "K1", "H3", "G3", "K5","K0", "C3",
                        "F4", "M2", "M1", "M0", "G8", "J6", "E2")

figure_chapters <- chapter_read2_percentage %>%
  left_join(chapter_read2_percentage_overall %>% 
              select(chapter_read2, freq) %>% 
              rename(cohort_freq = freq), by = "chapter_read2") %>% 
  filter(chapter_read2 %in% interest_chapters) %>%
  mutate(relative_freq = freq/cohort_freq) %>% 
  group_by(cluster) %>% 
  arrange(desc(freq)) %>% 
  mutate(row = 1:n()) %>% 
  ungroup() %>% 
  filter(row < 40) %>%
  mutate(
    cluster = case_when(
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  ),
  cluster = factor(cluster, levels = c("cohort", "1", "2", "3", "4", "5", "6")),
         desc_chapter2 = factor(desc_chapter2, levels = c(
           "Rheumatism, excluding the back",
           "Disorders of eye and adnexa",
           "Hypertensive disease",
           "Ischaemic heart disease",
           "Other forms of heart disease",
           "Vein, lymphatic and circulatory diseases NOS",
           "Skin and subcutaneous tissue infections",
           "Other skin and subcutaneous tissue inflammatory conditions",
           "Other skin and subcutaneous tissue disorders",
           "Neurotic, personality and other nonpsychotic disorders",
           "Other metabolic and immunity disorders",
           "Other endocrine gland diseases",
           "Chronic obstructive pulmonary disease",
           "Other urinary system diseases",
           "Other female genital tract disorders",
           "Nephritis, nephrosis and nephrotic syndrome",
           "Liver, biliary, pancreas + gastrointestinal diseases NEC"
         ))) %>% 
  ggplot(aes(x = cluster, y = relative_freq, fill = desc_chapter2)) +
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() +
  scale_fill_manual(values = c("#A6CEE3",
                               "#1F78B4",
                               "#990000",
                               "#D7301F",
                               "#EF6548",
                               "#FC8D59",
                               "#FA9FB5",
                               "#F768A1",
                               "#DD3497",
                               "#B2DF8A",
                               "#FFD92F",
                               "#FB9A99",
                               "#CAB2D6",
                               "#80CDC1",
                               "#35978F",
                               "#01665E",
                               "#FEC44F"
  )) +
  guides(fill=guide_legend(title="Read code description")) +
  theme(legend.key.size = unit(0.3, 'cm')) +
  geom_hline(yintercept=1, linetype='dotted', col = 'red') +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,4), breaks=seq(0,4, by = 1)) +
  labs(y = "Relative frequency",
       x = "Cluster")

figure_gender <- aki_cohort_demograph %>% 
  group_by(cluster) %>%
  mutate(total = n()) %>% 
  count(gender, total) %>%
  mutate(prop = round((n/total)*100, 2)) %>% 
  ungroup() %>%
  bind_rows(cohort_gender_figure) %>%
  mutate(
    cluster = case_when(
    cluster == "cohort" ~ "cohort",
    cluster == "1" ~ "4",
    cluster == "2" ~ "3",
    cluster == "3" ~ "1",
    cluster == "4" ~ "5",
    cluster == "5" ~ "6",
    cluster == "6" ~ "2",
  ),
  cluster = factor(cluster, levels = c("cohort", "1", "2", "3", "4", "5", "6"))) %>%                                                        # add k7
  ggplot(aes(x = cluster, y = prop, fill = gender)) +
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() +
  scale_fill_manual(values = c("#FDE725FF", "#238A8DFF")) +
  guides(fill=guide_legend(title="Sex")) +
  theme(legend.key.size = unit(0.5, 'cm')) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks=seq(0,100, by = 20)) +
  labs(y = "Percentage",
       x = "Cluster")

figure_age_group <- aki_cohort_demograph %>% 
  group_by(cluster) %>%
  mutate(total = n()) %>% 
  count(age_group, total) %>%
  mutate(prop = round((n/total)*100, 2)) %>% 
  ungroup() %>% 
  bind_rows(cohort_age_figure) %>%
  drop_na(age_group) %>% 
  mutate(
    cluster = case_when(
      cluster == "cohort" ~ "cohort",
      cluster == "1" ~ "4",
      cluster == "2" ~ "3",
      cluster == "3" ~ "1",
      cluster == "4" ~ "5",
      cluster == "5" ~ "6",
      cluster == "6" ~ "2",
    ),
    cluster = factor(cluster, levels = c("cohort", "1", "2", "3", "4", "5", "6"))) %>%                                                         
  ggplot(aes(x = cluster, y = prop, fill = age_group)) +
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() +
  scale_fill_viridis_d(option = "D", alpha = 0.95) +
  guides(fill=guide_legend(title="Age group")) +
  theme(legend.key.size = unit(0.5, 'cm')) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks=seq(0,100, by = 20)) +
  labs(y = "Percentage",
       x = "Cluster")

ggarrange(figure_chapters,                                                 
          ggarrange(figure_gender, figure_age_group, ncol = 2, labels = c("B", "C")), 
          nrow = 2, 
          labels = "A"                                        
) 

#+ time series of clusters, results = 'asis'

# cluster count

aki_cohort_chapter3$cluster <- factor(ind.coord1$k6)

cluster_count_k <- g_hes_aki_spell %>%
  mutate(year = year(admidate)) %>% 
  left_join(aki_cohort_chapter3 %>% 
              select(patid, cluster), by = "patid") %>%                
  distinct(spno, cluster, admidate) %>%
  group_by(spno) %>% 
  mutate(first_ep = min(admidate)) %>%
  ungroup() %>% 
  filter(admidate == first_ep) %>% 
  select(-first_ep) %>%                       
  filter(!is.na(cluster))

#' Time series of clusters

figure <- cluster_count_k %>%
  group_by(cluster) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>% 
  ungroup() %>% 
  mutate(cluster = case_when(
    cluster == "1" ~ "4. General AKI",
    cluster == "2" ~ "3. Established risk factors",
    cluster == "3" ~ "1. Less multimorbid",
    cluster == "4" ~ "5. Female multimorbid",
    cluster == "5" ~ "6. High level coding",
    cluster == "6" ~ "2. Mental health",
  )) %>%
  ggplot(aes(x = week_date, y = n, color = cluster)) +
  geom_line(stat = "identity") +
  theme_classic() +
  labs(y = "Number of admissions",
       colour = "Cluster phenotype") +
  scale_y_continuous(
    expand = c(0, 0), limits=c(0,500), breaks=seq(0,500, by = 100)
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_colour_manual(values=cbbPalette) +
  theme(text = element_text(size = 14),
        axis.title.x = element_blank()) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2016-01-01", "%Y-%m-%d"),
    xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = as.Date("2018-01-01", "%Y-%m-%d"),
    xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
    ymin = -Inf,
    ymax = Inf
  )
print(figure)

ggsave("cluster_time_series.pdf")

# for sensitivity analysis

ind.coord_sensitivity <- ind.coord
