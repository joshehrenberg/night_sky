library(jsonlite)
library(purrr)
constellations <- fromJSON("Data Code\\Constellations\\index.json", simplifyMatrix = FALSE)
constell_names <- constellations[["constellations"]][["common_name"]][["native"]]
constell_lines <- constellations[["constellations"]][["lines"]]
for(i in seq_len(88)) {
  for(j in seq_len(length(constell_lines[[i]]))) {
    constell_lines[[i]][[j]] <- constell_lines[[i]][[j]][which(constell_lines[[i]][[j]] != "thin")]
  }
}
constell_lines <- map(constell_lines, map, as.numeric) %>%
  map(map, combn, 2, simplify = FALSE)
times <- map(constell_lines, map, length) %>%
  map(as.numeric) %>%
  map(sum)
constell_names <- unlist(map2(constell_names, times, rep))
constell_lines <- purrr::flatten(purrr::flatten(constell_lines))
constell_line_starts <- map_dbl(constell_lines, 1)
constell_line_ends <- map_dbl(constell_lines, 2)
constell_lines <- cbind.data.frame(constell_names, constell_line_starts, constell_line_ends)