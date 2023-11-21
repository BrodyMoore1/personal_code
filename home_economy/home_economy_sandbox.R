
# Bonus Spending ----------------------------------------------------------

# Anticipated Amount
bonus_total <- 130000 * (.15 *.85) - 5304 #taxes

bonus_total -
  3000 - # 2022 Taxes {Pay as soon as money touches account}
  1000 - # Car Insurance (April) {Pay as soon as money touches account}
  1500 - # California Vacation {Transfer 600 Back to Tithing} {Pay as soon as money touches account}
  2000 - # Branson Vacation {Pay Later}
  2000 - # Utah Season Tickets {Transfer Back to Tithing} {Pre-Spent}
  1700   # Maddie's Lens {Pre-Spent}

# Put the extra / future funds in money market account for later

# Tithing Breakdown -------------------------------------------------------

#Have 
current_amount <- 2000

#Owe $21300
total_owed <- 2000 + #march 27
  1600 + #march 28
  900 + #jan 30
  2800 + #catch up through april
  1500 + # dec 25
  11000 + # jun 22
  1500 # mar 2021

# Payments Made {as of April 9th 2023}

transfer_back_bonus <- 600 + # disney tickets
  2000  # utah tickets

regular_payments <- 0

# Recalculate amount owed

final_total_owed <- total_owed -
  transfer_back_bonus -
  regular_payments
# $18,700 Worth of back pay owed {april 9th}
# $2671 per month if I want to catch up by end of December

# SPECULATION SECTION

# Rest of year current
rest_of_year_total <- 700 * 7
# $5000 for the rest of the year

# What Ifs
# If I do double time (I.E. $1400 per year out of snap paychecks)
# I'll be solid for the $5000 for the rest of the year & contribute $5000 toward the $18,700 bringing it to $13700
# If I then get 4 months of UEPC (~$8000) then I could bring it down to $5700 by the end of the year
# If Maddie and I can stick to a budget of $700
# this would mean on average an extra $814 per month after the double pay

# WITH THESE ASSUMPTIIONS MET IT SHOULD BE POSSIBLE TO GET CURRENT BY END OF THE YEAR
# Worth noting unexpected expenses would have to come out of the $1800 per month of flexable spending, so that could vary the amount contributed month to month. Just need to average out


# Current Update Sandbox --------------------------------------------------

current_month_breakdown <- august_categories %>% 
  full_join(flexible_categories, by = c("category" = "category")) %>% 
  full_join(inflexible_categories, by = c("category" = "category")) %>% 
  mutate(
    expected_amt = coalesce(expected_amt.x, expected_amt.y),
    flexible = coalesce(flexible.x, flexible.y)
  ) %>% 
  select(
    -ends_with(".x"),
    -ends_with(".y")
  ) %>% 
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>% 
  mutate(amount_left = expected_amt - amt_spent_april)

current_month_breakdown %>% 
  group_by(flexible) %>% 
  summarise(final_amount = sum(amount_left))


wrap_up_reactable(august_purchases %>% select(-month))


# Attempt to visualize




distinct_cats <- wrapped_up_current_week %>% distinct(category) %>% pull()

sd_highchart() %>%
  hc_add_series_list(
    wrapped_up_current_week %>%
      group_by(
        name =  place,
        type = "column",
        stacking = "normal",
        borderWidth = 1
      ) %>%
      do(data = list_parse(data.frame(x = .$category, name = .$place, y = .$amount)))
  ) %>%
  hc_xAxis(
    categories = c(distinct_cats)
  ) 


# commitment --------------------------------------------------------------

```{r August Commitment Data Prep}
before_commitment <- tribble(
  ~place,          ~amount, ~category,
  "super_chix",      25.35, "restaurant",
  "noom",           245.67, "personal_development",
  "maceys",          26.43, "grocery",
  "kneaders",        11.22, "restaurant",
  "better_help",    260.00, "personal_development",
  "maverik",         46.09, "gas",
  "costa_vida",      19.42, "restaurant",
  "hobby_lobby",     22.18, "church",
  "davis_park",      17.00, "golf",
  "maverik",         38.25, "gas",
  "arbys",            7.56, "restaurant",
  "zao",             29.83, "restaurant",
  "village_baker",    7.12, "restaurant",
  "quilted_bear",   118.63, "baby",
  "kyte_baby",       67.42, "baby",
  "amazon",          16.08, "misc",
  "sticky_bird",     27.06, "restaurant",
  "red_zone",        53.56, "clothes",
  "houston_chicken", 12.86, "restaurant",
  "target",          15.00, "baby",
  "taylor_swift",    51.93, "personal",
  "oakley",         153.89, "clothes",
  "jersey_mikes",     9.62, "restaurant",
  "GAP",             41.78, "baby",
  "carters",         43.72, "baby",
  "auntie_annes",     1.75, "baby",
  "maverik",         39.18, "gas",
  "usa_tech",         1.75, "parking",
  "cafe_rio",        10.71, "restaurant",
  "target",          25.95, "baby",
  "winter_garage",    2.00, "parking",
  "walmart",         18.25, "misc",
  "cv_draper",       26.48, "misc",
  "trader_joes",    122.14, "grocery",
  "scheels",         84.69, "clothes",
  "in_n_out",        20.35, "restaurant",
  "ikea",            24.77, "becks_kids",
  "ikea",           186.56, "becks_kids",
  "murray",           9.75, "misc",
  "birch_creek",     26.00, "golf",
  "tj_maxx",        393.00, "halloween",
  "GAP",             74.12, "baby",
  "sams_club",       51.48, "baby",
  "sams_club",        5.24, "restaurant",
  "maverik",         35.74, "gas",
  "target",          40.22, "baby",
  "target",          17.11, "baby",
  #"sams_club",       30.33, "misc",
  "sams_club",       21.74, "baby",
  "sams_club",        8.59, "grocery",
  "jamba",           15.97, "restaurant",
  "chick_fila",       5.17, "anniversary",
  "herms_inn",       38.00, "anniversary",
  "target",          15.27, "baby",
  "boujee_grill",     6.32, "restaurant",
  "nielsens",         2.59, "restaurant",
  "maverik",          2.67, "restaurant",
  "marquesas_corn",   8.72, "restaurant",
  "cinemark",        17.70, "personal",
  "maverik",         26.03, "gas",
  "chip_cookies",     7.94, "restaurant",
  "bruges",          12.51, "restaurant",
  "amex",            99.00, "renewal_fee",
  "ikea",            85.79, "becks_kids",
  "adobe",           10.69, "subscription",
  "chuck_a_rama",    39.49, "restaurant",
  "birkenstock",    139.11, "clothes",
  "logan_river",      9.00, "golf",
  "hoa",             60.00, "house_essential",
  "winco",            5.03, "grocery",
  "dominion",        27.43, "house_essential",
  "costco",          64.20, "subscription",
  "jacks_se",        25.31, "house_var",
  "home_depot",      43.21, "house_var",
  "taco_bell",       10.43, "restaurant",
  "mortgage",      3487.68, "house_payment",
)

after_commitment <- tribble(
  ~place,          ~amount, ~category,
  "center_st_pza",   22.68, "restaurant",
  "maverik",         42.45, "gas",
  "taco_bell",        6.00, "restaurant",
  "jamba_juice",     16.00, "restaurant",
  "zupas",           13.52, "restaurant",
  "maverik",         53.93, "gas",
  "planet_fitness",  10.70, "personal_development",
  "nibley_city",    120.85, "house_essential",
  "rocky_mtn",      128.41, "house_essential",
  
  # Week of 21 - 28
  "maverik",         49.40, "gas",
  "maverik",          2.67, "restaurant",
  "berry_station",    3.70, "restaurant",
  "nielsens",        10.28, "restaurant",
  "quantum_fiber",   75.00, "house_essential",
  "pettingill",      30.90, "grocery",
  "venmo",          -50.00, "grocery",
  "tacotime",         3.78, "restaurant",
  "roku",            11.78, "subscription",
  "old_navy",        19.19, "baby",
  "zupas",            6.38, "restaurant",
  "pretty_bird",     17.67, "restaurant",
  "maverik",         14.60, "restaurant",
  "apple",            2.99, "subscription",
  "quick_quack",     41.99, "subscription",
  "sams_club",       28.80, "gas",
  "sams_club",      165.05, "baby",
  "dog_groomer",     73.50, "dog",
  #"target",          80.83, "misc",
  "target",          12.00, "clothes",
  "target",           5.79, "baby",
  "target",          11.59, "personal",
  "target",          46.00, "house_var",
  "amazon",          35.34, "baby",
  "panda_express",   10.15, "restaurant",
  "tractor_supply",  21.39, "house_var",
  "sonic",            3.23, "restaurant",
  "tacotime",         7.77, "restaurant",
  "annies_diner",    11.86, "restaurant",
  "noodles",         20.03, "restaurant",
  "maverik",         43.92, "gas",
  "maverik",          9.42, "restaurant",
  "yonutz",          10.69, "restaurant",
  "homegoods",       35.00, "house_var",
  "nordstrom_rack",  20.00, "clothes",
  "krystal",        200.00, "phone",
  "trader_joes",     33.59, "grocery",
  "scheels",         34.32, "clothes",
  "sams_club",       99.87, "vacation_park_city",
  "nielsens",         5.17, "restaurant",
  "apple",           11.79, "subscription",
  "parking",          2.00, "misc",
  "jersey_mikes",    19.25, "restaurant",
  "zao",             22.35, "restaurant",
  "dominos",         17.59, "vacation_park_city",
  "parking",         12.00, "vacation_park_city",
  "walmart",         92.41, "vacation_park_city",
  "smiths",          91.59, "vacation_park_city",
  "smiths",          51.36, "gas",
  "loco_lizard",     40.00, "vacation_park_city",
  "davanzas",        28.03, "vacation_park_city",
  "wasatch",          4.00, "vacation_park_city",
  "walmart",          8.80, "vacation_park_city",
  "tj_maxx",        160.01, "clothes",
  "carters",         25.79, "baby",
  "davanzas",        23.65, "vacation_park_city",
  "olympic_park",    75.00, "vacation_park_city"
)

```

# Commitment Comparison {.tabset}

## Before 

```{r Before Commitment Display, eval = FALSE}
wrap_up_reactable(before_commitment) 
```

## After

```{r After Commitment Display, eval = FALSE}
wrap_up_reactable(after_commitment)
```


# vacation ----------------------------------------------------------------


all_vacations <- all_months_stacked %>% 
  filter(grepl("vaca",category)) %>% 
  distinct(category) %>% 
  pull()

vacation_list <- list()

for (i in 1:length(all_vacations)) {
  vacation_list[[all_vacations[i]]] <- all_months_stacked %>% 
    filter(category == all_vacations[i])
}
