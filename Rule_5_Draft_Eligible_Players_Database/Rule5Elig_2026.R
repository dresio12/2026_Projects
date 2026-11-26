library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(chromote)

# Team ID lookup
team_lookup <- c(
  "LAA","BAL","BOS","CHW","CLE","DET","KCR","MIN","NYY","ATH",
  "SEA","TBR","TEX","TOR","ARI","ATL","CHC","CIN","COL","MIA",
  "HOU","LAD","MIL","WSN","NYM","PHI","PIT","STL","SDP","SFG"
)

# Extracted from your browser cURL command
# TIP: if you still get 403s, replace this with the ENTIRE Cookie header
# from your cURL command (all cookies, not just cf_clearance), e.g.
# cookie_header <- "cf_clearance=...; _ga=...; other=..."
cookies <- c(
  "cf_clearance" = "ClqLYSLXOWW7A-4IFCoEItVRluvY8skJFl2M0FbGbcIF2YgTgJySK3_ozXRkBDBMQqB72Ea94TuBrXpJmw_O-lWR6vEfidLtifR8Szjzf-cBdz7w_2GgiqOsyKHbTKqc0gCe4hyhPGj2PAyCbl8f6fDTq_3L5TBxkhhsZ16CVhazcNyHDL84aMDGOmTrrDErFxVBJJUroh0ihGRDAgQgI8CVuhazWhJB7l5GplRM2jU0eDDPvh_eZwze1r_C9AAWfpwxkuc4amwbE14mw9FrkQBE6iloYA1EeAjFwY8Q-uqOAqxYdkoaDTCh_D0qTwxXyWw7NxuGoB1DF8Ilz6Z6EwT9ae0PeJinKNzP9Ml474j6f5_j2QOzPRXpR8h5AnPeXE1gMZkWr2rNiLmPgRpqRkQuxLutVRtm394zpfE-8HMpz9hRDxktRxyiZ1I8hfxIbQ")

cat("Using Cloudflare clearance token from your browser session...\n")

# Create session handle
session <- handle("https://www.fangraphs.com")

cookie_header <- paste(
  names(cookies),
  cookies,
  sep = "=",
  collapse = "; "
)

# Headers (this is an API/XHR call, so JSON accept + cors/empty fetch metadata)
headers <- add_headers(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/157.0.0.0 Safari/537.36 OPR/140.0.0.0",
  "Accept" = "application/json, text/plain, */*",
  "Accept-Language" = "en-US,en;q=0.9",
  "Referer" = "https://www.fangraphs.com/",
  "Cookie" = cookie_header,
  "sec-ch-ua" = '"Chromium";v="134", "Not:A-Brand";v="24", "Opera GX";v="119"',
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = '"Windows"',
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-origin"
)

# Scrape all 30 teams (prints status instead of dying on the first failure)
all_teams <- lapply(1:30, function(i) {
  
  cat("Scraping team", i, "of 30...\n")
  
  resp <- GET(
    paste0("https://www.fangraphs.com/api/depth-charts/roster?teamid=", i),
    handle = session,
    headers
  )
  
  if (status_code(resp) != 200) {
    cat("  Failed with status", status_code(resp), "\n")
    return(NULL)
  }
  
  txt <- content(resp, as = "text", encoding = "UTF-8")
  df <- fromJSON(txt, flatten = TRUE)
  
  if (is.data.frame(df)) df$teamid <- i
  
  Sys.sleep(1)
  
  df
})



# Combine into one data frame (bind_rows skips NULLs)
df_all <- bind_rows(all_teams)
df_all$teamid <- team_lookup[df_all$teamid]

# filter down to R5 players
r5 <- df_all |>
  filter(options1 == "R5")

# Filter columns
r5_filt <- r5 |>
  select(teamid, type, role, position, player, age1, bats, throws, 
         Org_Rank_Next, Ovr_Rank, Overall_Rank, minormasterid)

#### cleaning columns and data ####

# create primary position
r5_filt$primary_pos <- sub("/.*", "", r5_filt$position)

#create current level
r5_filt <- r5_filt |>
  mutate(
    level = case_when(
      str_starts(type, "off") ~ role,  # if type starts with "off", take role
      TRUE ~ str_to_upper(str_extract(type, "^[^-]+"))  # otherwise take before "-"
    ),
    # Fix "HA" and "LA" special cases
    level = case_when(
      level == "HA" ~ "A+",
      level == "LA" ~ "A",
      TRUE ~ level
    )
  )

# Vector of row IDs to manually change for level "ss" or "Inj"
rok_rows <- c(
  45, 46, 87, 256, 328, 498, 663, 712, 928, 931, 959, 1013
)

# Update level for these rows
r5_filt$level[rok_rows] <- c(
  "AA", "AA", "A+", "A", "AA", "AA", "AAA", "AA", "AA", "A+", "AA",
  "AAA" 
)

# renaming columns to be more descriptive
r5_new <- r5_filt |>
  rename(
    Organization = teamid,
    Player = player,
    Age = age1,
    Bats = bats,
    Throws = throws,
    `Org Rank` = Org_Rank_Next,
    `Top 100 Rank` = Ovr_Rank,
    `Ovr Power Rank` = Overall_Rank,
    Position = primary_pos,
    Level = level
  )


#### load in minor league stats ####

# Helper: GET a FanGraphs API url with the cookie/headers, return parsed JSON
fg_get <- function(url) {
  resp <- GET(url, handle = session, headers)
  if (status_code(resp) != 200) {
    stop("Request failed with status ", status_code(resp), " for:\n", url)
  }
  txt <- content(resp, as = "text", encoding = "UTF-8")
  fromJSON(txt, flatten = TRUE)
}

base <- "https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32"
tail_split   <- "&qual=0&type=2&team=0,to&season=2026&seasonEnd=2026&org=&ind=0&splitTeam=true"
tail_nosplit <- "&qual=0&type=2&team=&season=2026&seasonEnd=2026&org=&ind=0&splitTeam=false"

# batting - split / no split
df_flat_bat     <- fg_get(paste0(base, "&stats=bat", tail_split));   Sys.sleep(1)
df_flat_bat_tot <- fg_get(paste0(base, "&stats=bat", tail_nosplit)); Sys.sleep(1)

# pitching - split / no split
df_flat_pit     <- fg_get(paste0(base, "&stats=pit", tail_split));   Sys.sleep(1)
df_flat_pit_tot <- fg_get(paste0(base, "&stats=pit", tail_nosplit))

# join data
pitchers <- left_join(df_flat_pit, r5_new, by = "minormasterid")

batters <- left_join(df_flat_bat, r5_new, by = "minormasterid")

pitchers_tot <- left_join(df_flat_pit_tot, r5_new, by = "minormasterid")

batters_tot <- left_join(df_flat_bat_tot, r5_new, by = "minormasterid")

# remove non-R5
pitchers <- pitchers |> filter(!is.na(Level))
batters <- batters |> filter(!is.na(Level))
pitchers_tot <- pitchers_tot |> filter(!is.na(Level))
batters_tot <- batters_tot |> filter(!is.na(Level))

# remove parentheses with team initials from player names
pitchers$Player <- gsub("\\s*\\(.*?\\)", "", pitchers$Player)
pitchers_tot$Player <- gsub("\\s*\\(.*?\\)", "", pitchers_tot$Player)
batters$Player <- gsub("\\s*\\(.*?\\)", "", batters$Player)
batters_tot$Player <- gsub("\\s*\\(.*?\\)", "", batters_tot$Player)
r5_new$Player <- gsub("\\s*\\(.*?\\)", "", r5_new$Player)

pitchers <- pitchers |>
  mutate(`Strike%` = Strikes / Pitches) |>   
  select(
    Player, Position, Throws, Age = Age.y, Level = aLevel, Organization, `Org Rank`, `Ovr Power Rank`,
    W, L, ERA, G, GS, SV, HLD, IP, TBF, Pitches, `K/9`, `BB/9`, `K/BB`, `K-BB%`, 
    `Strike%`, `SwStr%`, `HR/9`, AVG, WHIP, BABIP, `LOB%`, FIP, xFIP, `GB%`, `FB%`, 
    `LD%`, `HR/FB`, `GB/FB`, `Oppo%`, `Pull%`, `Cent%`
  )

pitchers_tot <- pitchers_tot |>
  mutate(`Strike%` = Strikes / Pitches) |>   
  select(
    Player, Position, Throws, Age = Age.y, Level = aLevel, Organization, `Org Rank`, `Ovr Power Rank`,
    W, L, ERA, G, GS, SV, HLD, IP, TBF, Pitches, `K/9`, `BB/9`, `K/BB`, `K-BB%`, 
    `Strike%`, `SwStr%`, `HR/9`, AVG, WHIP, BABIP, `LOB%`, FIP, xFIP, `GB%`, `FB%`, 
    `LD%`, `HR/FB`, `GB/FB`, `Oppo%`, `Pull%`, `Cent%`
  )

batters <- batters |>
  select(
    Player, Position, Bats, Age = Age.y, Level = aLevel, Organization, `Org Rank`, `Ovr Power Rank`,
    G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, HBP, SO, SB, CS, 22:31,
    34, 35, 37:39, 41:45, 49, Pitches
  ) |>
  filter(Position != "SP" & Position != "RP")

batters_tot <- batters_tot |>
  select(
    Player, Position, Bats, Age = Age.y, Level, Organization, `Org Rank`, `Ovr Power Rank`,
    G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, HBP, SO, SB, CS, 22:31,
    34, 35, 37:39, 41:45, 49, Pitches
  ) |>
  filter(Position != "SP" & Position != "RP")

r5_new <- r5_new |>
  select(Organization, Position, Level, Player, Age, Bats, Throws, `Org Rank`, 
         `Top 100 Rank`, `Ovr Power Rank`) |>
  unique() 
batters <- unique(batters)
batters_tot <- unique(batters_tot)
pitchers <- unique(pitchers)
pitchers_tot <- unique(pitchers_tot)

saveRDS(r5_new, "r5_all2026.rds")
saveRDS(batters, "r5_bsplit2026.rds")
saveRDS(pitchers, "r5_psplit2026.rds")
saveRDS(batters_tot, "r5_btot2026.rds")
saveRDS(pitchers_tot, "r5_ptot2026.rds")

# Prospect Savant Data
aaa_hitters <- read_csv("aaa_hitters2026.csv")
aaa_pitchers <- read_csv("aaa_pitchers2026.csv")

aaa_hitters <- aaa_hitters |>
  mutate(Level = "AAA") |>
  select(
    Player, Organization, Pos, `PS Score`, xwOBA, wOBA, xBA, BA, OBP, xSLG, SLG,
    `BB%`, `K%`, `Chase%`, `Whiff%`, `Max EV`, `90th% EV`, `Hard Hit%`, Spd, `Swing%`, 
    `Z-Swing%`, `Z-Contact%`, `SwStr%`, `PullAir%`, PA, AB, Level
  )


aaa_r5_advstats <- left_join(batters |>
                               select(Player, Organization, Level, Age) |>
                               filter(Level == "AAA"), 
                             aaa_hitters, 
                             by = join_by(Player, Organization, Level)) |>
  filter(!is.na(Pos)) |>
  unique()


aaa_pitchers <- aaa_pitchers |>
  mutate(Level = "AAA") |>
  select(
    Player, Organization, Pos, `PS Score`, `Contact Quality`, `Max Velo`, 
    Extension, LA, xwOBA, wOBA, xBA, BA, xSLG, OBP, SLG, `BB%`, `K%`, `Chase%`, 
    `Whiff%`, `Hard Hit%`, `Swing%`, `Z-Swing%`, `Z-Contact%`, `SwStr%`,
    PA, AB, Pitches, Level
  )

aaa_r5_advstatsp <- left_join(pitchers |>
                                select(Player, Organization, Level, Age) |>
                                filter(Level == "AAA"), 
                              aaa_pitchers, 
                              by = join_by(Player, Organization, Level)) |>
  filter(!is.na(Pos)) |>
  unique()


saveRDS(aaa_r5_advstats, "r5_advh2026.rds")
saveRDS(aaa_r5_advstatsp, "r5_advp2026.rds")
