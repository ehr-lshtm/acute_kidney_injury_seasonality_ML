# pacman package first

if (!("pacman" %in% installed.packages()))
  install.packages("pacman")
library(pacman)

# load packages

p_load(
  haven,
  dplyr,
  tidyr,
  readr,
  ggplot2,
  stringr,
  glue,
  zoo,
  TSA,
  rmarkdown,
  gitignore,
  FactoMineR,
  factoextra,
  janitor,
  reshape2,
  data.table,
  readstata13,
  kableExtra,
  ggpubr,
  incidence2,
  plotly,
  formattable,
  scales,
  inspectdf,
  lubridate,
  ggsankey,
  viridis,
  cluster,
  grid,
  gridExtra,
  RColorBrewer,
  treemapify,
  flextable,
  htmltools,
  knitr,
  NbClust,
  ggrepel,
  officer,
  arrow
)

# ggplot colour palette with grey:
cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

# ggplot colour palette with black:

cbbPalette <-
  c(
    "#009E73",
    "#000000",
    "#E69F00",
    "#56B4E9",
    "#D55E00",
    "#0072B2",
    "#CC79A7",
    "#004949",
    "#009292",
    "#ff6db6",
    "#ffb6db",
    "#490092",
    "#006ddb",
    "#b66dff",
    "#6db6ff",
    "#b6dbff",
    "#920000",
    "#924900",
    "#db6d00",
    "#24ff24",
    "#ffff6d"
  )
