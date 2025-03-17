# Project Tidying

library("tidymodels")
library("janitor")
library("tidyverse")
library("rvest")
library("readxl")

# Outline -----------------------------------------------------------------

# predictors to estimate committee's picks --> for fun, guess on committee's picks

# predictors to estimate tournament results --> ranking

# then compare committee's pick's vs model's ranking (better or worse? interesting)

# sub thought: what teams does committee over-seed (compared to their own criteria)?
# how do these teams fare?

# data need: predictors: net (2019) X/quads X, rpi (2003) X/quads X, SOS, nonconf SOS, conference X
# SOR X, Conf Tournament X, Record X, Nonconf record X, BPI (2009) X, KPI (2016) X, KenPom (2003) X
# committees picks X, tournament results X


# Functions ---------------------------------------------------------------

clean_team_names <- function(df){
  df %>%
    clean_names() %>%
    mutate(
      team = str_remove_all(team, "[0123456789\\*]"),
      team = str_trim(team),
      team = str_remove_all(team, "\\("),
      team = str_remove_all(team, "\\)"),
      team = str_replace_all(team, "St. ", "St "),
      team = str_replace_all(team, "C-", "C "),
      team = str_replace_all(team, "LaSalle", "La Salle"),
      team = str_replace_all(team, "Cal State-", "CS "),
      team = str_replace_all(team, "UMKC", "Missouri KC"),
      team = str_replace_all(team, "Purdue-Fort Wayne", "PFW"),
      team = str_replace_all(team, "UMass Lowell", "MA Lowell"),
      team = str_replace_all(team, "McNeese State", "McNeese St"),
      team = str_replace_all(team, "TAMU-Corpus Christi", "TAM C. Christi"),
      team = str_replace_all(team, "USC Upstate", "SC Upstate"),
      team = str_replace_all(team, "Mount St Mary's", "Mt St Mary's"),
      team = str_replace_all(team, "SIU-Edwardsville", "SIUE"),
      team = str_replace_all(team, "Houston Christian", "Houston Chr"),
      team = str_replace_all(team, "SE Missouri State", "SE Missouri St"),
      team = str_replace_all(team, "MD-Eastern Shore", "MD E Shore"),
      team = str_replace_all(team, "Southern Ind.", "Southern Indiana"),
      team = str_replace_all(team, "Tex. A&M-Commerce", "TX A&M Commerce"),
      team = str_replace_all(team, "Lindenwood MO", "Lindenwood"),
      team = str_replace_all(team, "Cal St. Bakersfield", "CS Bakersfield"),
      team = str_replace_all(team, "UTSA", "UT San Antonio"),
      team = str_replace_all(team, "Louisiana Lafayette", "Lafayette"),
      team = str_replace_all(team, "UMass Lowell", "MA Lowell"),
      team = str_replace_all(team, "College of Charleston", "Col Charleston"),
      team = str_replace_all(team, "St. Francis NY", "St Francis NY"),
      team = str_replace_all(team, "St. Francis PA", "St Francis PA"),
      team = str_replace_all(team, "UMKC", "Missouri KC"),
      team = str_replace_all(team, "Tarleton St.", "Tarleton St"),
      team = str_replace_all(team, "Southeast Missouri St.", "SE Missouri St"),
      team = str_replace_all(team, "FIU", "Florida Intl"),
      team = str_replace_all(team, "Illinois Chicago", "IL Chicago"),
      team = str_replace_all(team, "Dixie St.", "Utah Tech"),
      team = str_replace_all(team, "SIU-Edwardsville", "SIUE"),
      team = str_replace_all(team, "McNeese St.", "McNeese St"),
      team = str_replace_all(team, "Mississippi Valley St.", "MS Valley St"),
      team = str_replace_all(team, "Texas A&M Corpus Chris", "TAM C. Christi"),
      team = str_replace_all(team, "Louisiana Monroe", "ULM"),
      team = str_replace_all(team, "UT Rio Grande Valley", "UTRGV"),
      team = str_replace_all(team, "Arkansas Pine Bluff", "Ark Pine Bluff"),
      team = str_replace_all(team, "SIU Edwardsville", "SIUE"),
      team = str_replace_all(team, "Tennessee Martin", "TN Martin"),
      team = str_replace_all(team, "St. Thomas", "St Thomas MN"),
      team = str_replace_all(team, "Queens", "Queens NC"),
      team = str_replace_all(team, "Bethune Cookman", "Bethune-Cookman"),
      team = str_replace_all(team, "Maryland Eastern Shore", "MD E Shore"),
      team = str_replace_all(team, "Texas A&M Commerce", "TX A&M Commerce"),
      team = str_replace_all(team, "N.C.", "NC"),
      team = str_replace_all(team, "St. Thomas", "St Thomas MN"),
      team = str_replace_all(team, "UMass Lowell", "MA Lowell"),
      team = str_replace_all(team, "Texas A&M Corpus Chris", "TAM C. Christi"),
      team = str_replace_all(team, "Illinois Chicago", "IL Chicago"),
      team = str_replace_all(team, "UMKC", "Missouri KC"),
      team = str_replace_all(team, "Tennessee Martin", "TN Martin"),
      team = str_replace_all(team, "Cal St. ", "CS "),
      team = str_replace_all(team, "UTSA", "UT San Antonio"),
      team = str_replace_all(team, "Queens", "Queens NC"),
      team = str_replace_all(team, "FIU", "Florida Intl"),
      team = str_replace_all(team, "SIU Edwardsville", "SIUE"),
      team = str_replace_all(team, "USC Upstate", "SC Upstate"),
      team = str_replace_all(team, "Louisiana Monroe", "ULM"),
      team = str_replace_all(team, "Bethune Cookman", "Bethune-Cookman"),
      team = str_replace_all(team, "UT Rio Grande Valley", "UTRGV"),
      team = str_replace_all(team, "Arkansas Pine Bluff", "Ark Pine Bluff"),
      team = str_replace_all(team, "Texas A&M Commerce", "TX A&M Commerce"),
      team = str_replace_all(team, "LIU", "LIU Brooklyn"),
      team = str_replace_all(team, "LIU Brooklyn Brooklyn", "LIU Brooklyn"),
      team = str_replace_all(team, "Maryland Eastern Shore", "MD E Shore"),
      team = str_replace_all(team, "Mississippi Valley St.", "MS Valley St"),
      team = str_replace_all(team, "Houston Christian", "Houston Chr"),
      team = str_replace_all(team, "Southeast Missouri St.", "SE Missouri St"),
      team = str_replace_all(team, "Saint Francis", "St Francis PA"),
      team = str_replace_all(team, "St. Francis", "St Francis"),
      team = str_replace_all(team, "Dixie St.", "Utah Tech"),
      team = str_replace_all(team, "Louisiana Lafayette", "Louisiana"),
      team = str_replace_all(team, "College of Charleston", "Col Charleston"),
      team = str_replace_all(team, "IPFW", "PFW"),
      team = str_replace_all(team, "Arkansas Little Rock", "Ark Little Rock"),
      team = str_replace_all(team, "Texas Pan American", "UTRGV"),
      team = str_replace_all(team, "Winston Salem St.", "W Salem St"),
      team = str_replace_all(team, "Southwest Missouri St.", "Missouri St"),
      team = str_replace_all(team, "Southwest Texas St.", "Texas St"))
}

# Tidying -----------------------------------------------------------------

teams <- read_csv("Project/Kaggle Data/MTeams.csv", show_col_types = FALSE) %>% clean_names() %>%
  select(team_id, team_name)
conferences <- read_csv("Project/Kaggle Data/MTeamConferences.csv", show_col_types = FALSE) %>% clean_names()

historical_season <- read_csv("Project/Kaggle Data/MRegularSeasonCompactResults.csv", show_col_types = FALSE) %>% clean_names() %>%
  left_join(conferences, by = join_by(season, "w_team_id" == "team_id")) %>%
  rename(w_conf = "conf_abbrev") %>%
  left_join(conferences, by = join_by(season, "l_team_id" == "team_id")) %>%
  rename(l_conf = "conf_abbrev")

record <- historical_season %>% group_by(w_team_id, season) %>% summarise(wins = n(), .groups = "keep") %>%
  left_join(historical_season %>% group_by(l_team_id, season) %>% summarise(losses = n(), .groups = "keep"),
            by = join_by("w_team_id" == "l_team_id", season)) %>%
  ungroup() %>%
  rename("team_id" = "w_team_id") %>%
  mutate(
    wins = replace_na(wins, 0),
    losses = replace_na(losses, 0),
    win_perc = wins/(wins + losses)) %>%
  arrange(team_id, season)

non_conf_record <- historical_season %>% filter(w_conf != l_conf) %>% group_by(w_team_id, season) %>% summarise(nc_wins = n(), .groups = "keep") %>%
  ungroup() %>%
  full_join(historical_season %>% filter(w_conf != l_conf) %>% group_by(l_team_id, season) %>% summarise(nc_losses = n(), .groups = "keep") %>% ungroup(),
                             by = join_by("w_team_id" == "l_team_id", season)) %>%
  rename("team_id" = "w_team_id") %>%
  mutate(
    nc_wins = replace_na(nc_wins, 0),
    nc_losses = replace_na(nc_losses, 0),
    nc_win_perc = nc_wins/(nc_wins + nc_losses)) %>%
  arrange(team_id, season)

rankings <- read_csv("Project/Kaggle Data/MMasseyOrdinals.csv", show_col_types = FALSE) %>%
  clean_names()

ranking_filter <- function(sys){
  rankings %>% filter(system_name == sys) %>%
    left_join(group_by(., season) %>% summarise(max = max(ranking_day_num)),
              by = join_by(season)) %>%
    filter(ranking_day_num == max) %>%
    select(season, team_id, ordinal_rank)
}

rankings %>% filter(system_name == "KPI") %>%
  filter(season == "2020") %>%
  arrange(ranking_day_num)

sor <- ranking_filter("ESR") %>%
  rename(sor = ordinal_rank)

bpi <- ranking_filter("EBP") %>%
  rename(bpi = ordinal_rank)

net <- ranking_filter("NET") %>%
  rename(net = ordinal_rank)

rpi <- ranking_filter("RPI") %>%
  rename(rpi = ordinal_rank)

kpi <- ranking_filter("KPI") %>%
  rename(kpi = ordinal_rank)

pom <- ranking_filter("POM") %>%
  rename(pom = ordinal_rank)

ap <- ranking_filter("AP") %>%
  rename(ap = ordinal_rank)

sag <- ranking_filter("SAG") %>%
  rename(sag = ordinal_rank)

alternate_spellings <- read_csv("Project/Kaggle Data/MTeamSpellings.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(team_name = str_to_title(team_name_spelling)) %>%
  select(team_name, team_id) %>%
  bind_rows(teams) %>%
  distinct(team_name, .keep_all = TRUE)

sor2 <- read_excel("Project/MarchMadness_Bracket/Data/SOR data.xlsx") %>%
  clean_team_names() %>%
  left_join(alternate_spellings, by = join_by("team" == "team_name")) %>%
  select(-team)

sor <- sor %>% filter(season != 2021, season != 2023) %>%
  bind_rows(sor2) %>%
  arrange(season, sor)

kpi2 <- read_excel("Project/MarchMadness_Bracket/Data/KPI data.xlsx") %>%
  clean_team_names() %>%
  filter(team != "Non D-I") %>%
  left_join(alternate_spellings, by = join_by("team" == "team_name")) %>%
  select(-team)

kpi <- kpi %>% filter(season < 2020) %>%
  bind_rows(kpi2)

sos2 <- read_html("https://www.warrennolan.com/basketball/2025/sos-net") %>%
  html_element("body") %>%
  html_element("#container-x") %>%
  html_element("#main-body-content") %>%
  html_element(".main-body-row-flex-scroll") %>%
  html_element(".full-width-box-x") %>%
  html_table() %>%
  rename("SOS" = "SOS Rank", "NonConf SOS" = "Non-Conf SOS Rank") %>%
  mutate(Year = 2025)

sos <- read_excel("Project/MarchMadness_Bracket/Data/SOS data.xlsx") %>%
  bind_rows(sos2) %>%
  clean_team_names() %>%
  left_join(alternate_spellings, by = join_by("team" == "team_name")) %>%
  select(-team) %>%
  rename("season" = "year") %>% distinct(season, team_id)

net_quads_big <- historical_season %>%
  left_join(net, by = join_by(season, "w_team_id" == "team_id")) %>%
  filter(!is.na(net)) %>%
  rename(w_net = net) %>%
  left_join(net, by = join_by(season, "l_team_id" == "team_id")) %>%
  rename(l_net = net) %>%
  mutate(
    w_quad = case_when(
      w_loc == "H" & l_net < 31 ~ 1,
      w_loc == "H" & l_net < 76 ~ 2,
      w_loc == "H" & l_net < 161 ~ 3,
      w_loc == "H" ~ 4,
      w_loc == "N" & l_net < 51 ~ 1,
      w_loc == "N" & l_net < 101 ~ 2,
      w_loc == "N" & l_net < 201 ~ 3,
      w_loc == "N" ~ 4,
      w_loc == "A" & l_net < 76 ~ 1,
      w_loc == "A" & l_net < 136 ~ 2,
      w_loc == "A" & l_net < 241 ~ 3,
      .default = 4),
    l_quad = case_when(
      w_loc == "A" & w_net < 31 ~ 1,
      w_loc == "A" & w_net < 76 ~ 2,
      w_loc == "A" & w_net < 161 ~ 3,
      w_loc == "A" ~ 4,
      w_loc == "N" & w_net < 51 ~ 1,
      w_loc == "N" & w_net < 101 ~ 2,
      w_loc == "N" & w_net < 201 ~ 3,
      w_loc == "N" ~ 4,
      w_loc == "H" & w_net < 76 ~ 1,
      w_loc == "H" & w_net < 136 ~ 2,
      w_loc == "H" & w_net < 241 ~ 3,
      .default = 4))

net_quads <- net_quads_big %>%
  group_by(season, w_team_id, w_quad) %>%
  summarize(wins = n(),
            .groups = "keep") %>%
  full_join(net_quads_big %>% group_by(season, l_team_id, l_quad) %>% summarise(losses = n(), .groups = "keep"),
            by = join_by(season, w_team_id == l_team_id, w_quad == l_quad)) %>%
  rename(team_id = w_team_id,
         quad = w_quad) %>%
  mutate(
    wins = replace_na(wins, 0),
    losses = replace_na(losses, 0),
    win_perc = wins/(wins + losses)) %>%
  arrange(team_id, season, quad) %>%
  pivot_wider(names_from = quad, values_from = c(wins, losses), names_prefix = "q", names_sort = TRUE) %>%
  group_by(season, team_id) %>%
  summarise(
    across(wins_q1:losses_q4, ~sum(.x, na.rm = TRUE)),
    .groups = "keep") %>%
  ungroup() %>%
  rename_with(~str_replace(.x, "wins_", "net_w")) %>% 
  rename_with(~str_replace(.x, "losses_", "net_l"))
  
rpi_quads_big <- historical_season %>%
  left_join(rpi, by = join_by(season, "w_team_id" == "team_id")) %>%
  filter(!is.na(rpi)) %>%
  rename(w_rpi = rpi) %>%
  left_join(rpi, by = join_by(season, "l_team_id" == "team_id")) %>%
  rename(l_rpi = rpi) %>%
  mutate(
    w_quad = case_when(
      w_loc == "H" & l_rpi < 31 ~ 1,
      w_loc == "H" & l_rpi < 76 ~ 2,
      w_loc == "H" & l_rpi < 161 ~ 3,
      w_loc == "H" ~ 4,
      w_loc == "N" & l_rpi < 51 ~ 1,
      w_loc == "N" & l_rpi < 101 ~ 2,
      w_loc == "N" & l_rpi < 201 ~ 3,
      w_loc == "N" ~ 4,
      w_loc == "A" & l_rpi < 76 ~ 1,
      w_loc == "A" & l_rpi < 136 ~ 2,
      w_loc == "A" & l_rpi < 241 ~ 3,
      .default = 4),
    l_quad = case_when(
      w_loc == "A" & w_rpi < 31 ~ 1,
      w_loc == "A" & w_rpi < 76 ~ 2,
      w_loc == "A" & w_rpi < 161 ~ 3,
      w_loc == "A" ~ 4,
      w_loc == "N" & w_rpi < 51 ~ 1,
      w_loc == "N" & w_rpi < 101 ~ 2,
      w_loc == "N" & w_rpi < 201 ~ 3,
      w_loc == "N" ~ 4,
      w_loc == "H" & w_rpi < 76 ~ 1,
      w_loc == "H" & w_rpi < 136 ~ 2,
      w_loc == "H" & w_rpi < 241 ~ 3,
      .default = 4))

rpi_quads <- rpi_quads_big %>%
  group_by(season, w_team_id, w_quad) %>%
  summarise(wins = n(), .groups = "keep") %>%
  full_join(rpi_quads_big %>% group_by(season, l_team_id, l_quad) %>% summarise(losses = n(), .groups = "keep"),
            by = join_by(season, w_team_id == l_team_id, w_quad == l_quad)) %>%
  rename(team_id = w_team_id,
         quad = w_quad) %>%
  mutate(
    wins = replace_na(wins, 0),
    losses = replace_na(losses, 0),
    win_perc = wins/(wins + losses)) %>%
  arrange(team_id, season, quad) %>%
  pivot_wider(names_from = quad, values_from = c(wins, losses), names_prefix = "q", names_sort = TRUE) %>%
  group_by(season, team_id) %>%
  summarise(
    across(wins_q1:losses_q4, ~sum(.x, na.rm = TRUE)),
    .groups = "keep") %>%
  ungroup() %>%
  rename_with(~str_replace(.x, "wins_", "rpi_w")) %>% 
  rename_with(~str_replace(.x, "losses_", "rpi_l"))

conf_tournament <- read_csv("Project/Kaggle Data/MConferenceTourneyGames.csv", show_col_types = FALSE) %>%
  clean_names() %>% mutate(index = row_number()) %>% rename(conf = conf_abbrev)

conf_champions <- conf_tournament %>%
  group_by(season, conf) %>%
  summarise(
    last_game_row = max(index),
    .groups = "keep") %>%
  ungroup() %>%
  left_join(conf_tournament, by = join_by("season", "conf", last_game_row == index)) %>%
  select(-last_game_row, -day_num) %>%
  rename(champion = w_team_id,
         finalist = l_team_id) %>%
  pivot_longer(cols = c("champion", "finalist"), names_to = "conf_result", values_to = "team_id") %>%
  select(-conf)

# conf_champions <- tibble(
#   season = 2025,
#   conf_result = "champion",
#   team = c("Duke", "Bryant", "Lipscomb", "Houston", "St John's", "Montana",
#            "High Point", "UC San Diego", "UNC Wilmington", "Liberty", "Robert Morris",
#            "Yale", "VCU", "Florida", "Memphis", "Michigan", "VCU",
#            "Mount St Mary's", "Akron", "Norfolk St", "Drake", "Colorado St",
#            "St Francis", "SIU Edwardsville", "American", "Wofford", "McNeese St",
#            "Omaha", "Troy", "Alabama St", "Grand Canyon", "Gonzaga")) %>%
#   clean_team_names() %>%
#   left_join(alternate_spellings, by = join_by("team" == "team_name")) %>%
#   select(season, conf_result, team_id) %>%
#   bind_rows(conf_champions0) %>%
#   distinct()

seeds <- read_csv("Project/Kaggle Data/MNCAATourneySeeds.csv", show_col_types = FALSE) %>% clean_names() %>%
  mutate(seed = str_remove_all(seed, "[WXYZab]"),
         seed = as.integer(seed))

historical_tourney <- read_csv("Project/Kaggle Data/MNCAATourneyCompactResults.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  add_row(season = 2021, day_num = 137, w_team_id = 1332, w_score = 1, l_team_id = 1433, l_score = 0, w_loc = "N", num_ot = 0)

previous_performance <- historical_tourney %>%
  group_by(season) %>%
  arrange(season, day_num) %>%
  mutate(
    order = rank(-day_num, ties.method = "last"),
    round = case_when(
      order < 2 ~ 7,
      order < 4 ~ 6,
      order < 8 ~ 5,
      order < 16 ~ 4,
      order < 32 ~ 3,
      order < 64 ~ 2,
      order < 68 ~ 1,
      .default = NA)) %>%
  ungroup() %>%
  select(-order, -day_num, -w_score, -l_score, -w_loc, -num_ot) %>%
  pivot_longer(cols = c(w_team_id, l_team_id), names_to = "outcome", values_to = "team_id") %>%
  mutate(
    round = case_when(
      round == 7 & outcome == "w_team_id" ~ 8,
      .default = round),
    outcome = case_when(
      round == 8 ~ "l_team_id",
      .default = outcome)) %>%
  filter(outcome == "l_team_id") %>%
  select(-outcome) %>%
  mutate(
    season = season + 1) %>%
  rename(prev_year_result = round)

full_team <- record %>%
  left_join(non_conf_record, by = join_by(team_id, season)) %>%
  left_join(conferences, by = join_by(team_id, season)) %>%
  left_join(sos, by = join_by(team_id, season)) %>%
  left_join(sor, by = join_by(team_id, season)) %>%
  left_join(bpi, by = join_by(team_id, season)) %>%
  left_join(net, by = join_by(team_id, season)) %>%
  left_join(rpi, by = join_by(team_id, season)) %>%
  left_join(kpi, by = join_by(team_id, season)) %>%
  left_join(pom, by = join_by(team_id, season)) %>%
  left_join(ap, by = join_by(team_id, season)) %>%
  left_join(sag, by = join_by(team_id, season)) %>%
  left_join(net_quads, by = join_by(team_id, season)) %>%
  left_join(rpi_quads, by = join_by(team_id, season)) %>%
  left_join(conf_champions, by = join_by(team_id, season)) %>%
  left_join(previous_performance, by = join_by(team_id, season)) %>%
  left_join(teams, by = join_by(team_id)) %>%
  left_join(seeds, by = join_by(team_id, season)) %>% 
  relocate(team_name, .before = season)

full_team2 <- full_team %>% mutate(
  rpi_net = case_when(
    is.na(net) ~ rpi,
    .default = net),
  rpi_net_wq1 = case_when(
    is.na(net) ~ rpi_wq1,
    .default = net_wq1),
  rpi_net_wq2 = case_when(
    is.na(net) ~ rpi_wq2,
    .default = net_wq2),
  rpi_net_wq3 = case_when(
    is.na(net) ~ rpi_wq3,
    .default = net_wq3),
  rpi_net_wq4 = case_when(
    is.na(net) ~ rpi_wq4,
    .default = net_wq4),
  rpi_net_lq1 = case_when(
    is.na(net) ~ rpi_lq1,
    .default = net_lq1),
  rpi_net_lq2 = case_when(
    is.na(net) ~ rpi_lq2,
    .default = net_lq2),
  rpi_net_lq3 = case_when(
    is.na(net) ~ rpi_lq3,
    .default = net_lq3),
  rpi_net_lq4 = case_when(
    is.na(net) ~ rpi_lq4,
    .default = net_lq4),
  is_net = if_else(is.na(net), 0, 1)) %>%
  select(-rpi, -net, -net_wq1, -net_wq2, -net_wq3, -net_wq4,
         -net_lq1, -net_lq2, -net_lq3, -net_lq4,
         -contains("rpi_lq"), -contains("rpi_wq")) %>%
  filter(season > 2002, season != 2020) %>%
  mutate(
    conf_result = if_else(is.na(conf_result), "lost", conf_result),
    make_tournament = if_else(is.na(seed), 0, 1),
    prev_year_result = replace_na(prev_year_result, 0),
    ineligible = case_when(
      # team_id == 1152 & season == 2013 ~ 1,
      # team_id == 1315 & season == 2011 ~ 1,
      # team_id == 1315 & season == 2012 ~ 1,
      # team_id == 1377 & season == 2010 ~ 1,
      team_id == 1467 & season == 2023 ~ 1,
      team_id == 1468 & season == 2022 ~ 1,
      team_id == 1112 & season == 2021 ~ 1,
      team_id == 1329 & season == 2022 ~ 1,
      team_id == 1120 & season == 2021 ~ 1,
      team_id == 1468 & season == 2024 ~ 1,
      team_id == 1471 & season == 2024 ~ 1,
      team_id == 1478 & season == 2024 ~ 1,
      team_id == 1477 & season == 2024 ~ 1,
      team_id == 1476 & season == 2024 ~ 1,
      team_id == 1475 & season == 2024 ~ 1,
      team_id == 1474 & season == 2024 ~ 1,
      team_id == 1473 & season == 2024 ~ 1,
      team_id == 1472 & season == 2024 ~ 1,
      team_id == 1470 & season == 2024 ~ 1,
      team_id == 1469 & season == 2024 ~ 1,
      team_id == 1477 & season == 2023 ~ 1,
      team_id == 1476 & season == 2023 ~ 1,
      team_id == 1475 & season == 2023 ~ 1,
      team_id == 1474 & season == 2023 ~ 1,
      team_id == 1473 & season == 2023 ~ 1,
      team_id == 1472 & season == 2023 ~ 1,
      team_id == 1470 & season == 2023 ~ 1,
      team_id == 1469 & season == 2023 ~ 1,
      team_id == 1468 & season == 2023 ~ 1,
      team_id == 1471 & season == 2023 ~ 1,
      .default = 0),
    conf_result = case_when(
      team_id == 1240 & season == 2022 ~ "champion", # not accurate, but necessary
      team_id == 1192 & season == 2023 ~ "champion", # not accurate, but necessary
      # team_id == 1467 & season == 2023 ~ "finalist",
      # team_id == 1468 & season == 2022 ~ "finalist",
      .default = conf_result),
    conf_classification = case_when(
      conf_abbrev == "big_twelve" ~ "power six",
      conf_abbrev == "big_ten" ~ "power six",
      conf_abbrev == "big_east" ~ "power six",
      str_detect(conf_abbrev, "pac") ~ "power six",
      conf_abbrev == "acc" ~ "power six",
      conf_abbrev == "sec" ~ "power six",
      conf_abbrev == "a_ten" ~ "high major",
      conf_abbrev == "aac" ~ "high major",
      conf_abbrev == "mvc" ~ "high major",
      conf_abbrev == "mwc" ~ "high major",
      team_name == "Gonzaga" ~ "power six",
      .default = "mid major"))

write.csv(full_team2, "Project/MarchMadness_Bracket/Data/full_team.csv")

set.seed(1128)

full_tourney <- historical_tourney %>%
  filter(season > 2002, season != 2020) %>%
  select(-w_loc) %>%
  mutate(
    rand = runif(length(w_team_id)),
    winner = if_else(rand > .5, "t1", "t2"),
    t1_team_id = if_else(rand > .5, w_team_id, l_team_id),
    t2_team_id = if_else(rand < .5, w_team_id, l_team_id),
    t1_score = if_else(rand > .5, w_score, l_score),
    t2_score = if_else(rand < .5, w_score, l_score)) %>%
  select(-contains("w_"), -contains("l_"), -rand) %>%
  left_join(full_team2 %>% rename_with( ~ paste0("t1_", .x)), by = join_by(season == t1_season, t1_team_id)) %>%
  left_join(full_team2 %>% rename_with( ~ paste0("t2_", .x)), by = join_by(season == t2_season, t2_team_id))

write.csv(full_tourney, "Project/MarchMadness_Bracket/Data/full_tourney.csv")
