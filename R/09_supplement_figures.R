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
# rmarkdown::render("09_supplement_figures.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("supplement_figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)

#' # Supplementary figures

#+ admissions time series, results = 'asis'

#### AKI monthly time series with smoothing (generalized additive model)

norm_p2 <- g_hes_aki_spell %>%
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
  mutate_at('n', normalize) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = age_group), size = 0.5) +
  theme_classic() +
  labs(color = "Age group") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
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


norm_p3 <- g_hes_aki_spell %>%
  group_by(gender) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  mutate_at('n', normalize) %>%
  ungroup() %>%
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = gender)) +
  theme_classic() +
  labs(color = "Sex") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
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

norm_p4 <- g_hes_aki_spell_full %>%
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
  mutate_at('n', normalize) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = d_order_2)) +
  theme_classic() +
  labs(color = "Diagnostic position") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values = cbbPalette) +
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


norm_p5 <- g_hes_aki_spell_full %>%
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
  mutate_at('n', normalize) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = aki_day_4)) +
  theme_classic() +
  labs(color = "Day of AKI code") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
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

norm_p6 <- top_ICD %>%
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
  mutate_at('n', normalize) %>%
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = ICD_diagnosis)) +
  theme_classic() +
  labs(color = "ICD10") +
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
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

ggarrange(norm_p3, norm_p5, norm_p4, norm_p6, norm_p2, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E"))

#+ time series with heatwave annotation

df_heatwave <- data.frame(
  xmin = c("2019-06-28", "2019-07-21", "2019-08-23", "2018-06-25", 
           "2018-06-30", "2018-07-21", "2018-08-01", "2017-06-16",
           "2017-07-05", "2016-07-18", "2016-08-22", "2016-09-12",
           "2015-07-01"),
  xmax = c("2019-06-30", "2019-07-28", "2019-08-29", "2018-06-27",
           "2018-07-10", "2018-07-29", "2018-08-09", "2017-06-23",
           "2017-07-07", "2016-07-22", "2016-08-26", "2016-09-17",
           "2015-07-02"
           ),
  ymin = -Inf,
  ymax = Inf,
  fill = paste0("red")
  )

primary_aki_heatwave <- g_hes_aki_spell_full %>%
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
  filter(d_order_2 == "Primary") %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = d_order_2), size = 1) +
  theme_classic() +
  labs(color = "Diagnostic position",
       y = "Number of admissions") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  scale_colour_manual(values = "black") +
  theme(
    legend.position = 'top',
    legend.justification = 'right',
    axis.title.x = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.margin=margin(0,0,0,0)
  ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-06-28", "%Y-%m-%d"), xmax = as.Date("2019-06-30",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-07-21", "%Y-%m-%d"), xmax = as.Date("2019-07-28",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-08-23", "%Y-%m-%d"), xmax = as.Date("2019-08-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-25", "%Y-%m-%d"), xmax = as.Date("2018-06-27",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-30", "%Y-%m-%d"), xmax = as.Date("2018-07-10",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-07-21", "%Y-%m-%d"), xmax = as.Date("2018-07-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-08-01", "%Y-%m-%d"), xmax = as.Date("2018-08-09",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-06-16", "%Y-%m-%d"), xmax = as.Date("2017-06-23",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-07-05", "%Y-%m-%d"), xmax = as.Date("2017-07-07",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-07-18", "%Y-%m-%d"), xmax = as.Date("2016-07-22",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-08-22", "%Y-%m-%d"), xmax = as.Date("2016-08-26",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-09-12", "%Y-%m-%d"), xmax = as.Date("2016-09-17",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2015-07-01", "%Y-%m-%d"), xmax = as.Date("2015-07-03",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

secondary_aki_heatwave <- g_hes_aki_spell_full %>%
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
  filter(d_order_2 == "Secondary") %>% 
  ggplot(aes(week_date, n)) +
  geom_line(aes(color = d_order_2), size = 1) +
  theme_classic() +
  labs(color = "Diagnostic position",
       y = "Number of admissions") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1000), breaks = seq(0, 1000, by = 200)) +
  scale_colour_manual(values = "black") +
  theme(
    legend.position = 'top',
    legend.justification = 'right',
    axis.title.x = element_blank(),
    legend.box.spacing = unit(0, "pt"),
    legend.margin=margin(0,0,0,0)
  ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-06-28", "%Y-%m-%d"), xmax = as.Date("2019-06-30",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-07-21", "%Y-%m-%d"), xmax = as.Date("2019-07-28",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-08-23", "%Y-%m-%d"), xmax = as.Date("2019-08-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-25", "%Y-%m-%d"), xmax = as.Date("2018-06-27",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-30", "%Y-%m-%d"), xmax = as.Date("2018-07-10",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-07-21", "%Y-%m-%d"), xmax = as.Date("2018-07-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-08-01", "%Y-%m-%d"), xmax = as.Date("2018-08-09",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-06-16", "%Y-%m-%d"), xmax = as.Date("2017-06-23",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-07-05", "%Y-%m-%d"), xmax = as.Date("2017-07-07",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-07-18", "%Y-%m-%d"), xmax = as.Date("2016-07-22",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-08-22", "%Y-%m-%d"), xmax = as.Date("2016-08-26",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-09-12", "%Y-%m-%d"), xmax = as.Date("2016-09-17",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2015-07-01", "%Y-%m-%d"), xmax = as.Date("2015-07-03",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

normalised_aki_heatwave <- g_hes_aki_spell_full %>%
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
  mutate_at('n', normalize) %>%
  ungroup() %>% 
  ggplot(aes(week_date, n)) +
  # geom_rect(aes(xmin = as.Date(xmin, "%Y-%m-%d"), xmax = as.Date(xmax, "%Y-%m-%d"), ymin = ymin, ymax = ymax, fill = fill), data = df_heatwave)
  geom_line(aes(color = d_order_2), size = 1) +
  theme_classic() +
  labs(color = "Diagnostic position",
       y = "Normalised scale") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values = cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
             xmin = as.Date("2019-06-28", "%Y-%m-%d"), xmax = as.Date("2019-06-30",  "%Y-%m-%d"),
             ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-07-21", "%Y-%m-%d"), xmax = as.Date("2019-07-28",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-08-23", "%Y-%m-%d"), xmax = as.Date("2019-08-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-25", "%Y-%m-%d"), xmax = as.Date("2018-06-27",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-30", "%Y-%m-%d"), xmax = as.Date("2018-07-10",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-07-21", "%Y-%m-%d"), xmax = as.Date("2018-07-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-08-01", "%Y-%m-%d"), xmax = as.Date("2018-08-09",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-06-16", "%Y-%m-%d"), xmax = as.Date("2017-06-23",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-07-05", "%Y-%m-%d"), xmax = as.Date("2017-07-07",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-07-18", "%Y-%m-%d"), xmax = as.Date("2016-07-22",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-08-22", "%Y-%m-%d"), xmax = as.Date("2016-08-26",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-09-12", "%Y-%m-%d"), xmax = as.Date("2016-09-17",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2015-07-01", "%Y-%m-%d"), xmax = as.Date("2015-07-03",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)



# aki incidence and diagnostic postion superimposed with heatwave data

ggarrange(primary_aki_heatwave, secondary_aki_heatwave, normalised_aki_heatwave, ncol = 1, nrow = 3, labels = c("A", "B", "C"))

#+ epi week cluster figure

epi_week_cluster_figure <- cluster_count_k %>%
  mutate(week = epiweek(admidate),
         year = epiyear(admidate),
         cluster = case_when(
           cluster == "1" ~ "4. General AKI",
           cluster == "2" ~ "3. Established risk factors",
           cluster == "3" ~ "1. Less multimorbid",
           cluster == "4" ~ "5. Female multimorbid",
           cluster == "5" ~ "6. High level coding",
           cluster == "6" ~ "2. Mental health",
           )) %>% 
  count(week, cluster) %>% 
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"))) %>%
  ggplot(aes(x = week, y = n, color = cluster, group = cluster)) +
  geom_line(stat = "identity") +
  theme_classic() +
  labs(x = "Epidemiological week of admission",
       y = "Number of admissions") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 2000),
    breaks = seq(0, 2000, by = 500)
  ) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) 

epi_week_cluster_figure

#' Cluster by year

epi_week_cluster <- cluster_count_k %>%
  mutate(week = epiweek(admidate),
         year = epiyear(admidate),
    cluster = case_when(
      cluster == "1" ~ "4. General AKI",
      cluster == "2" ~ "3. Established risk factors",
      cluster == "3" ~ "1. Less multimorbid",
      cluster == "4" ~ "5. Female multimorbid",
      cluster == "5" ~ "6. High level coding",
      cluster == "6" ~ "2. Mental health",
    )
  )

k1_year <- epi_week_cluster %>% 
  filter(cluster == "1. Less multimorbid") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
                       year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  labs(title = "1.  Less multimorbid")

k2_year <- epi_week_cluster %>% 
  filter(cluster == "2. Mental health") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
         year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  labs(title = "2. Mental health")


k3_year <- epi_week_cluster %>% 
  filter(cluster == "3. Established risk factors") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
         year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  labs(title = "3. Established risk factors")

k4_year <- epi_week_cluster %>% 
  filter(cluster == "4. General AKI") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
         year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
    theme(legend.position='top', 
          legend.justification='right',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.box.spacing = unit(0, "pt"),
          legend.margin=margin(0,0,0,0)
    ) +
  labs(title = "4. General AKI")

k5_year <- epi_week_cluster %>%
  mutate(week = epiweek(admidate),
         year = epiyear(admidate)) %>% 
  filter(cluster == "5. Female multimorbid") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
         year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  labs(title = "5. Female multimorbid")

k6_year <- epi_week_cluster %>%
  mutate(week = epiweek(admidate),
         year = epiyear(admidate)) %>% 
  filter(cluster == "6. High level coding") %>% 
  count(week, year, cluster) %>%
  mutate(week = factor(week, levels = c("26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                                        "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", 
                                        "46", "47", "48", "49", "50", "51", "52", "1", "2", "3", "4",
                                        "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")),
         year = factor(year, levels = c("2015", "2016", "2017", "2018", "2019"))) %>%
  ggplot(aes(x = week, y = n, color = year, group = year)) +
  geom_line(stat = "identity") +
  theme_classic() +
  labs(x = "Week of admission",
       y = "Number of admissions") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  scale_x_discrete(expand = c(0,0), breaks = everyother) +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position='top', 
        legend.justification='right',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.box.spacing = unit(0, "pt"),
        legend.margin=margin(0,0,0,0)
  ) +
  labs(title = "6. High level coding")


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

norm_cluster_admissions <- cluster_count_k %>%
  group_by(cluster) %>% 
  count(week_date = floor_date(admidate, "week", week_start = 7)) %>%
  mutate_at('n', normalize) %>%
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
  facet_wrap(~ cluster, scales = "free", ncol = 2) +
  theme_classic() +
  labs(y = "Normalised number of admissions",
       colour = "Cluster phenotype") +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_colour_manual(values=cbbPalette) +
  theme(text = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.position = "none") +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)

norm_cluster_admissions

#' 2D visualization of cluster assignment k = 6

figure_2d_plot <- ind.coord6 %>%
  mutate(
    cluster = case_when(
      cluster == "1" ~ "4. General AKI",
      cluster == "2" ~ "3. Established risk factors",
      cluster == "3" ~ "1. Less multimorbid",
      cluster == "4" ~ "5. Female multimorbid",
      cluster == "5" ~ "6. High level coding",
      cluster == "6" ~ "2. Mental health",
    )) %>% 
  ggplot(aes(
    x = `Dim 1`,
    y = `Dim 2`,
    colour = cluster
  )) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values = cbbPalette)
print(figure_2d_plot)
cat("\n")

#+ scree plot, results = 'asis'

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 2.0))

#+ sankey sensitivity analysis of dimensions, results = 'asis'

ind.coord461 <-
  fread(file.path(rawdata_temp_files_path, "ind_coord_chapter3_600.txt")) %>% 
  select(1:461) #### change to chapters or medcode up the hierarchy

set.seed(123)
km.res <- kmeans(ind.coord461, 6, nstart = 25, iter.max = 100, algorithm = "MacQueen")

# Add clusters obtained using the K-means algorithm

ind.coord_sensitivity$cluster461 <- factor(km.res$cluster)

df_ind_k6 <- ind.coord_sensitivity %>%
  select(cluster, cluster461) %>%
  rename(Dim_5 = cluster,
         Dim_461 = cluster461) %>%
  mutate(
    Dim_461 = case_when(
      Dim_461 == 1 ~ "4",
      Dim_461 == 2 ~ "3",
      Dim_461 == 3 ~ "6",
      Dim_461 == 4 ~ "5",
      Dim_461 == 5 ~ "1",
      Dim_461 == 6 ~ "2"
    )
  ) %>%
  make_long(Dim_5, Dim_461)                          

sankey_sensitivity_analysis <- df_ind_k6 %>%
  mutate( node = case_when(
    node == "1" ~ "4",
    node == "2" ~ "3",
    node == "3" ~ "1",
    node == "4" ~ "5",
    node == "5" ~ "6",
    node == "6" ~ "2"
  ), 
  next_node = case_when(
    next_node == "1" ~ "4",
    next_node == "2" ~ "3",
    next_node == "3" ~ "1",
    next_node == "4" ~ "5",
    next_node == "5" ~ "6",
    next_node == "6" ~ "2"
  ), 
  ) %>% 
  ggplot( aes(x = x,
              next_x = next_x,
              node = node,
              next_node = next_node,
              fill = factor(node),
              label = node)) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  scale_fill_viridis_d(option = "D", alpha = 0.95, labels=c('1. Less multimorbid', '2. Mental health -> Musculoskeletal', '3. Established risk factors', '4. General AKI', '5. Female multimorbid', '6. Non-specific codes')) +
  theme_sankey(base_size = 18) +
  theme(
    legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=10)
  ) +
  geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE) +
  geom_sankey_label(size = 3, color = "black", fill= "white", hjust = 0.7)

sankey_sensitivity_analysis
