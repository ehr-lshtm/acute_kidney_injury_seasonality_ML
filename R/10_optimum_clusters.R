#' ---
#' title: Optimum cluster selction K-means
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
# rmarkdown::render("10_optimum_clusters.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("optimum_clusters", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)
#+ setup

### load data

ind.coord <-
  fread(file.path(rawdata_temp_files_path, "ind_coord_chapter3_600.txt"))            

#+ 30 indices method, results = 'asis'

# 30 indices ind.coord 5

for (i in 1:5) {

  set.seed(NULL)
  ind.coord.sample <- ind.coord %>%
    select(1:5) %>%
    sample_n(25000)

  indices5 <- NbClust(
      ind.coord.sample,
      distance = "euclidean",
      min.nc = 2,
      max.nc = 10,
      method = "kmeans",
      # index = "alllong"
    )

  run <- fviz_nbclust(indices5)
  assign(paste0("run", i), run)
  remove(indices5)
  gc()

}

dim5 <- ggarrange(run1, run2, run3, run4, run5, ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E"))

dim5

### Reformat figures

optimum_k <- fread("data/optimum_k.csv")

optimum_k %>% 
  pivot_longer(
    cols =  2:6 ,
    names_to = "run",
    values_to = "count",
    values_drop_na = TRUE
  ) %>%
  filter(run == 1) %>% 
  ggplot(aes(x = k, y = count)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  scale_x_continuous(expand = c(0,0), limits=c(-1,11), breaks=seq(0,10, by = 1)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,12), breaks=seq(0,12, by = 1)) +
  labs(y = "Frequency among all indices",
       x = "Number of clusters k") +
  theme_classic()

for (i in 1:5) {
  
  run <- optimum_k %>% 
    pivot_longer(
      cols =  2:6 ,
      names_to = "run",
      values_to = "count",
      values_drop_na = TRUE
    ) %>%
    filter(run == i) %>% 
    ggplot(aes(x = k, y = count)) +
    geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
    scale_x_continuous(expand = c(0,0), limits=c(-1,11), breaks=seq(0,10, by = 1)) +
    scale_y_continuous(expand = c(0,0), limits=c(0,12), breaks=seq(0,12, by = 1)) +
    labs(y = "Frequency among all indices",
         x = "Number of clusters k") +
    theme_classic()
  
  assign(paste0("run", i), run)
  
  }

ggarrange(run1, run2, run3, run4, run5, ncol = 2, nrow = 3,   hjust = -1.2
, labels = c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5"))

ggsave("outputs/indices_plot.png")
