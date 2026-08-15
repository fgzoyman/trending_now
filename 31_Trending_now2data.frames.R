library(tidyverse)
library(rvest)
library(xml2)
# 
# 
# "itt egy feladatitt egy feladat" --> "itt egy feladat"
felez <- function(tt) tt |> str_replace("(^.*)\\1$", "\\1")
# 
# a könyvtár
# konyvtar <- "gtarchive"
files <- konyvtar |> 
  list.files(pattern = "html$", full.names = TRUE)
# 
# 
trending_heads <- files |> 
  map(
    ~ .x |> 
      read_html() |> 
      html_nodes("table, tbody tr") |> 
      html_text2() |> 
      str_trim() |> 
      str_replace_all("\\n·\\n", "\n") |> 
      # str_replace_all("\\+ keresés(\\n[\\p{L}_]+){2}", "+") |> 
      str_split_fixed("\\n\\t\\n", 4) |>                          # TODO: 4
      as_tibble(.name_repair = ~ letters[1:length(.x)])|> 
      separate_wider_delim(
        a, 
        delim = "\n",                                     # ez CSAK szimpla
        names = LETTERS[1:5], names_sep = "_",
        too_few = "align_start") |>                               # TODO: 5
      separate_wider_delim(
        b:c, 
        delim = "\n",                                     # ez CSAK szimpla
        names = LETTERS[1:3], names_sep = "_",
        too_few = "align_start") |>                               # TODO: 3
      mutate(
        d = d |> 
          str_remove_all("Keresési kifejezésquery_statsFelfedezés") |> 
          str_split("\\n") |> 
          map(~ .x |> felez() |> paste(collapse = "; "))
      ) |> 
      unnest(d) |> 
      # ha hiányos az [a_B] oszlop, nem kell -- az első két sor
      filter(!is.na(a_B)) |> 
      mutate(
        timestamp = .x |> 
          str_extract_all("\\d") |> 
          unlist() |> 
          paste(collapse = "") |> 
          ymd_hm(tz = "Europe/Budapest"),
        ab = .x |> 
          str_sub(-6, -6),
        id = 1:n(),
        .before = a_A
      )
    )
# 
# 
trending_details <- trending_heads |> 
  bind_rows() |> 
  select(!ab) |> 
  mutate(
    id = 1:n(),
    .by = timestamp
  )
trending_details |> 
  summarise(
    nrow = max(id),
    .by = timestamp
  )
# 
# 
# trends_lines <- trends_html 
trending_points <- files |> 
  map(
    ~ .x |> 
      read_html() |> 
      html_nodes("polyline[fill='none']") |> 
      html_attr("points") |> 
      as.list() |> 
      map_df(
        ~ tibble(nyers = .x) |> 
          separate_longer_delim(nyers, " "),
        .id = "id"
        ) |> 
      separate_wider_delim(
        nyers, 
        delim = ",", 
        names = c("x", "y"),
        too_few = "align_start"
      ) |> 
      mutate(across(everything(), as.integer)) |> 
      filter(!is.na(x)) |> 
      mutate(
        timestamp = .x |> 
          str_extract_all("\\d") |> 
          unlist() |> 
          paste(collapse = "") |> 
          ymd_hm(tz = "Europe/Budapest"),
        ab = .x |> 
          str_sub(-6, -6),
        id = consecutive_id(id),
        .before = id
      )
    )
trending_lines <- trending_points |> 
  map2(
    trending_heads,
    ~ .x |> 
      left_join(
        .y |> 
          select(timestamp:a_A)
      )
  ) |> 
  bind_rows() |> 
  select(timestamp, id, a_A, x, y) |> 
  mutate(
    id = consecutive_id(id),
    .by = timestamp
  )
#
# a vonalak hossza:
trending_lines |> 
  count(timestamp, id, name = "hossz") |> 
  count(hossz, name = "db")
# 
# mi hiányzik a [trending_lines]ból
trending_details |> 
  full_join(
    trending_lines |> 
      filter(x == 2)
    ) |> 
  filter(is.na(x))
# 
# mi lett vele: ** 2026-08-13 09:00:00 -- 91 -- jd vance ** ?
files[[32]] |> 
  read_html() |> 
  html_nodes("polyline[fill='none']") |> 
  html_attr("points") |> 
  as.list()
# 
# 
trending_details |> 
  write_tsv("trending_details.tsv")
trending_lines |> 
  write_tsv("trending_lines.tsv")