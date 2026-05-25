library(tidyverse)

bonne_sante_info <- tibble(
  cities = c(
    "Nidoma",
    "Talu",
    "Pauma",
    "Riroua",
    "Gordes",
    "Maeva",
    "Valais",
    "Colmar",
    "Eden",
    "Vaiku",
    "Mahuti",
    "Kinsale"
  ),
  houses = c(548, 612, 505, 568, 435, 642, 448, 2374, 615, 460, 1255, 512)
)

housing_options <- character(8974)

c <- 1
for (i in seq_len(nrow(bonne_sante_info))) {
  for (j in 1:(bonne_sante_info[i, "houses"] |> pull())) {
    housing_options[c] <- str_glue(
      "{bonne_sante_info[i, 'cities'] |> pull()}_{j}"
    )
    c <- c + 1
  }
}

set.seed(11042004)

list_in_order <- sample(housing_options, size = 1000, replace = FALSE)

# Kevin used 1:85

calvin_list_in_order <- list_in_order[86:length(list_in_order)]
