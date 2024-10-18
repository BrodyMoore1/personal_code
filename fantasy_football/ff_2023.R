library(tidyverse)
library(pins)

snapdragon::sd_board_register("prod")

#previous_data <- pin_get("results_pin_2023","rsconnect")

previous_data <- pin_get("results_pin_2024","rsconnect")


# 2024 Season -------------------------------------------------------------

league_setup <- tribble(
  ~person,    ~dues_paid, ~monthly_motivosity_particpant,
  "Brody",    TRUE       , TRUE,
  "Phil",     TRUE       , TRUE,
  "Kevin",    TRUE       , TRUE,
  "Cuyler",   TRUE       , TRUE,
  "Magon",    TRUE       , TRUE,
  "Kaivan",   TRUE       , TRUE,
  "Gus",      TRUE       , TRUE,
  "Conner",   FALSE      , TRUE,
  "Nick",     TRUE       , TRUE,
  "Nate",     TRUE       , TRUE,
  "Farshad",  TRUE       , TRUE,
  "Lin",      TRUE       , TRUE,
  "Dallen",   FALSE      , FALSE,
  "Hoonie",   FALSE      , TRUE
)

motivosity_participants <- league_setup %>% filter(monthly_motivosity_particpant  == TRUE) %>% pull(person)



fantasy_scores <- tribble(
  ~person,   ~month, ~tot_wins,  ~tot_pts, ~motivosity_paid,  
  # September
  "Brody",        9,         2,         474,            TRUE,
  "Phil",         9,         2,         307,            FALSE,
  "Kevin",        9,         1,         369,            TRUE,
  "Cuyler",       9,         3,         494,            TRUE,
  "Magon",        9,         2,         440,            FALSE,
  "Kaivan",       9,         2,         489,            TRUE,
  "Gus",          9,         0,         351,            FALSE,
  "Conner",       9,         3,         493,            FALSE,
  "Nick",         9,         3,         443,            TRUE,
  "Nate",         9,         3,         515,            FALSE,
  "Farshad",      9,         2,         501,            TRUE,
  "Lin",          9,         3,         465,            TRUE,
  "Dallen",       9,         0,         334,            FALSE,
  "Hoonie",       9,         2,         399,            FALSE,
  
  # Oct Example               
  "Brody",       10,         3,         607,            FALSE,
  "Phil",        10,         3,         439,            FALSE,
  "Kevin",       10,         2,         509,            FALSE,
  "Cuyler",      10,         3,         601,            FALSE,
  "Magon",       10,         2,         539,            FALSE,
  "Kaivan",      10,         3,         600,            FALSE,
  "Gus",         10,         0,         421,            FALSE,
  "Conner",      10,         4,         644,            FALSE,
  "Nick",        10,         3,         554,            FALSE,
  "Nate",        10,         4,         661,            FALSE,
  "Farshad",     10,         2,         628,            FALSE,
  "Lin",         10,         3,         567,            FALSE,
  "Dallen",      10,         1,         444,            FALSE,
  "Hoonie",      10,         2,         479,            FALSE
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
  # Uncomment the transmute after September
  #rename(
  #  current_month_wins = tot_wins_1,
  #  current_month_pts = tot_pts_1
  #) %>% 
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
  filter(month < month(today())) %>% 
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

# Need to fix this pin
pins::pin(fantasy_list, "results_pin_2024", board = "rsconnect")





