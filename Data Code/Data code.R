library(dplyr)
library(magrittr)
library(astroFns)
library(jsonlite)
library(purrr)

source("functions.R")
catalog <- read.csv("Data Code\\Objects\\NGC 2022.csv") %>%
  select(N, NI, A, S, RH, RM, RS, V, DG, DM, DS, VMAG, TYPE, D.z.) %>%
  filter(S %in% 1:5) %>%
  mutate(name = paste0(N, ifelse(N == N, "G", ""), "C", NI, A), .before = 1, .keep = "unused") %>%
  mutate(.keep = "unused", .after = 1, type = case_when(S == 1 ~ "Galaxy", 
                                                        S == 2 ~ "Galactic Nebula",
                                                        S == 3 ~ "Planetary Nebula",
                                                        S == 4 ~ "Open Cluster",
                                                        S == 5 ~ "Globular Cluster")) %>%
  mutate(rascension = rad_to_degree(dms2rad(paste0(RH, "d ", RM, "m ", RS, "s"))), .keep = "unused", .after = 2) %>%
  mutate(.keep = "unused", .after = 3, declination = ifelse(V == "+", 
                                                            rad_to_degree(dms2rad(paste0(DG, "d ", DM, "m ", DS, "s"))), 
                                                            -1 * rad_to_degree(dms2rad(paste0(DG, "d ", DM, "m ", DS, "s")))))
saveRDS(catalog, "data\\Deep_objects.RDS")

stars <- read.csv("Data Code\\Stars\\hygdata_v3.csv") %>%    
  filter(id != 0) %>%
  select(hip, ra, dec)
saveRDS(stars, "Data\\stars.RDS")

constellations <- fromJSON("Data Code\\Constellations\\index.json", simplifyMatrix = FALSE)
constell_names <- constellations[["constellations"]][["common_name"]][["native"]]
constell_lines <- constellations[["constellations"]][["lines"]]
#constellations 39 and 40 have a "thin designations for some of their lines
#this removes those designations
for(i in seq_len(88)) {
  for(j in seq_len(length(constell_lines[[i]]))) {
    constell_lines[[i]][[j]] <- constell_lines[[i]][[j]][which(constell_lines[[i]][[j]] != "thin")]
  }
}
#some lines were character because of the "thin" entries, those can be changed now
constell_lines <- map(constell_lines, map, as.numeric)
#count total number of line segments this works because each constellation has a number of segments
#equal to the number of stars in each line of the constellation length(unlist()) 
#minus the number of individual lines because each line has a "starting star"
#that doesn't add to the number of segments.
times <- map_dbl(constell_lines, ~ length(unlist(.)) - length(.))
#make each line segment just 2 stars for eventual use in geom_segment()
temp <- vector("list", length(88))
for(i in seq_len(88)) {
  temp[[i]] <- vector("list", length(constell_lines[[i]]))
  for(j in seq_along(constell_lines[[i]])) {
    temp[[i]][[j]] <- vector("list", length(times[i]))
    for(k in seq_len(length(constell_lines[[i]][[j]]) - 1)) {
      temp[[i]][[j]][[k]] <- c(constell_lines[[i]][[j]][[k]], constell_lines[[i]][[j]][[k + 1]])
    }
  }
}
constell_lines <- purrr::flatten(purrr::flatten(temp))
constell_names <- unlist(map2(constell_names, times, rep))
constell_line_starts <- map_dbl(constell_lines, 1)
constell_line_ends <- map_dbl(constell_lines, 2)
constell_lines <- cbind.data.frame(constell_names, constell_line_starts, constell_line_ends) %>%
  left_join(stars, by = c("constell_line_starts" = "hip")) %>%
  mutate(rascension_start = ra, declination_start = dec, .keep = "unused") %>%
  left_join(stars, by = c("constell_line_ends" = "hip")) %>%
  mutate(rascension_end = ra, declination_end = dec, .keep = "unused")
saveRDS(constell_lines, "Data\\Constell_lines.RDS")
  


 