library(tidyverse)
library(pins)

snapdragon::sd_board_register("prod")

previous_data <- pin_get("results_pin_2023","rsconnect")

# 2023 Season -------------------------------------------------------------

league_setup <- tribble(
  ~person,  ~dues_paid, ~monthly_motivosity_particpant,
  "Brody",  TRUE      , TRUE,
  "Nick",   TRUE      , TRUE,
  "Magon",  TRUE      , TRUE,
  "Hoonie", FALSE     , FALSE,
  "Gus",    TRUE     , TRUE,
  "Jason",  TRUE      , FALSE,
  "Kevin",  TRUE      , TRUE,
  "Conner", TRUE     , TRUE,
  "Kaivan", TRUE      , TRUE,
  "Nate",   FALSE     , TRUE
)

motivosity_participants <- league_setup %>% filter(monthly_motivosity_particpant  == TRUE) %>% pull(person)



fantasy_scores <- tribble(
  ~person,   ~month, ~tot_wins,  ~tot_pts, ~motivosity_paid,     # ~wins_september, ~wins_october, ~pts_september, ~pts_october,
  # September
  "Brody",        9,         4,       602,            TRUE,     #               3,               4,             530,            1000, 
  "Nick",         9,         3,       534,            TRUE,     #                3,               6,             527,            1008, 
  "Magon",        9,         3,       507,            TRUE,      #                3,               6,             518,            1086, 
  "Hoonie",       9,         2,       518,            FALSE,      #               3,               6,             498,             991, 
  "Gus",          9,         2,       474,            TRUE,      #                2,               3,             508,             930, 
  "Jason",        9,         2,       473,            TRUE,      #                2,               5,             485,             983, 
  "Kevin",        9,         1,       520,            TRUE,      #                1,               2,             408,             854, 
  "Conner",       9,         1,       506,            TRUE,      #               0,               3,             523,            1157, 
  "Kaivan",       9,         1,       457,            TRUE,
  "Nate",         9,         1,       453,            TRUE,
  # Oct Example               
  "Brody",        10,        5,       1135,            TRUE,       #               3,               4,             530,            1000, 
  "Nick",         10,        6,       1101,            TRUE,      #                3,               6,             527,            1008, 
  "Magon",        10,        4,        986,            TRUE,      #                3,               6,             518,            1086, 
  "Hoonie",       10,        4,       1064,            FALSE,      #               3,               6,             498,             991, 
  "Gus",          10,        6,       1018,            TRUE,      #                2,               3,             508,             930, 
  "Jason",        10,        3,        982,            TRUE,      #                2,               5,             485,             983, 
  "Kevin",        10,        5,       1079,            TRUE,      #                1,               2,             408,             854, 
  "Conner",       10,        3,        966,            TRUE,      #               0,               3,             523,            1157, 
  "Kaivan",       10,        2,        953,            TRUE,
  "Nate",         10,        2,        861,            TRUE,
  # # Nov Example    
  "Brody",        11,        10,       1883,            TRUE,       #               3,               4,             530,            1000, 
  "Nick",         11,        7,       1753,            FALSE,      #                3,               6,             527,            1008, 
  "Magon",        11,        6,       1545,            FALSE,      #                3,               6,             518,            1086, 
  "Hoonie",       11,        7,       1674,            FALSE,      #               3,               6,             498,             991, 
  "Gus",          11,        9,       1676,            TRUE,      #                2,               3,             508,             930, 
  "Jason",        11,        3,       1390,            FALSE,      #                2,               5,             485,             983, 
  "Kevin",        11,        7,       1654,            FALSE,      #                1,               2,             408,             854, 
  "Conner",       11,        6,       1488,            FALSE,      #               0,               3,             523,            1157, 
  "Kaivan",       11,        7,       1754,            TRUE,
  "Nate",         11,        3,       1400,            FALSE,
)

#month_result_list <- list()
month_result_list <- previous_data$ranked_months

# September is One off
# sept_results <- fantasy_scores %>% 
#   filter(month == 9) %>% 
#   select(
#     person,
#     current_month_wins = tot_wins,
#     current_month_pts = tot_pts
#   ) %>% 
#   arrange(desc(current_month_wins), desc(current_month_pts)) %>% 
#   mutate(rank = row_number())
#   
# altered_sept <- sept_results
# names(altered_sept) <- gsub("current_month", "September", names(altered_sept))
# 
# month_result_list[["September"]] <- altered_sept




# Current month rankings
ranked_months <- fantasy_scores %>% 
  group_by(person) %>% 
  mutate(rank = dense_rank(desc(month))) %>% 
  ungroup() %>% 
  filter(rank <= 2) 

current_standings <- ranked_months %>% 
  select(person, starts_with('tot'), rank) %>% 
  pivot_wider(
    names_from = rank,
    values_from = c(tot_wins, tot_pts)
  ) %>% 
  transmute(
    person,
    current_month_wins = tot_wins_1 - tot_wins_2,
    current_month_pts = tot_pts_1 - tot_pts_2
  ) %>% 
  arrange(desc(current_month_wins), desc(current_month_pts)) %>% 
  mutate(rank = row_number())

current_month <- month.name[max(ranked_months$month)]

alteted_standings <- current_standings

names(alteted_standings) <- gsub("current_month", current_month, names(alteted_standings))

month_result_list[[current_month]] <- alteted_standings


# 4 things I want

# 1) Previous month standings
prvious_month <- month.name[max(fantasy_scores$month) - 1]
previous_month_standings <- month_result_list[[prvious_month]] %>% 
  filter(person %in% motivosity_participants) %>% 
  arrange(desc(across(c(2,3)))) %>% 
  mutate(rank = row_number())

# 2) Current Month standings
current_month_standings <- current_standings %>% 
  filter(person %in% motivosity_participants) %>% 
  arrange(desc(across(c(2,3)))) %>% 
  mutate(rank = row_number())

for(i in 2:nrow(current_month_standings)) {
  updated_standings <- current_month_standings %>% 
    filter(rank <= i ) %>% 
    mutate(
      lag_win = lag(current_month_wins), 
      lag_pts = lag(current_month_pts),
      lag_rank = lag(rank),
      updated_rank = case_when(
        current_month_wins == lag_win & current_month_pts == lag_pts ~ lag_rank,
        TRUE ~ rank
      )
    ) %>% 
    select(person, updated_rank)
  
  current_month_standings <- current_month_standings %>% 
    left_join(updated_standings) %>% 
    mutate(rank = coalesce(updated_rank, rank)) %>% 
    select(-updated_rank)
}

# 3) An across month view of monthly points scored
# Define a function to join dataframes using inner_join
join_dataframes <- function(df1, df2) {
  inner_join(df1, df2, by = "person")
}

altered_list <- lapply(month_result_list, function(df) {
  df[, !(names(df) %in% "rank")]
})

# Use Reduce to apply the join_dataframes function to the list
full_fantasy_results <- Reduce(join_dataframes, altered_list)

# 4) Update on paid monthly motivosity
monthly_motivosity_paid <- fantasy_scores %>% 
  filter(month <= month(today())) %>% 
  filter(person %in% motivosity_participants) %>% 
  mutate(month = month.name[month]) %>% 
  select(person, month, motivosity_paid) %>% 
  pivot_wider(names_from = month, values_from = motivosity_paid)



fantasy_list <- list(
  "current_points" = fantasy_scores,
  "ranked_months" = month_result_list,
  "previous_month_standings" = previous_month_standings,
  "current_month_standings" = current_month_standings,
  "full_fantasy_results" = full_fantasy_results,
  "monthly_motivosity" = monthly_motivosity_paid,
  "league_setup" = league_setup
)


pins::pin(fantasy_list, "results_pin_2023", board = "rsconnect")





