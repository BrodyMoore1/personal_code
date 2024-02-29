

existing_thread <- tribble(
  ~phone_number,
  8018642081, # Amanda
  4352322225, # Brent Thomas
  2086545105, # Gabe
  3852445930, # Brody
  3857221773,
  4352135326,
  4352320504,
  4352325769,
  4352328168,
  4352890241,
  4354069205,
  4357603564,
  4358967920,
  8013684935,
  8014506217,
  8016153338,
  8016153393,
  8016687227,
  8017929680,
  8323388208
) %>% 
  mutate(is_existing = TRUE)

new_thread <- tribble(
  ~child,            ~parent_name,            ~phone_number, ~added_to_groupme,
  "Loclan Campbell", "Todd",                     4359321038, FALSE,
  "Loclan Campbell", "Cecilia",                  4359382353, FALSE,
  
  "Max Gotberg",     "Trevor",                   8013901162, FALSE,
  "Max Gotberg",     "Brynn",                    8013901162, FALSE,
  
  "Jarom Jackson",   "Kevin",                    8016687227, TRUE,
  "Jarom Jackson",   "Chelsie",                  8017929680, TRUE,
  
  "Jace Larsen",     "Ryan",                     7018669739, TRUE,
  "Jace Larsen",     "McKenzie",                 7018660812, TRUE,
  
  "Hayden Sharp",    "Nathan",                   8014506217, TRUE,
  "Hayden Sharp",    "Courtney",                 8013684935, FALSE,
  
  "Fox Walker",      "Greg",                     8016153338, FALSE,
  "Fox Walker",      "Jessica",                  8016153393, FALSE,
  
  "Orson Wells",     "Alex",                     4352328168, TRUE,
  "Orson Wells",     "Catherine",                4352135326, TRUE,
  
  "Noah Willis",     "Jimmie",                   NA,         NA,
  "Noah Willis",     "Arianna",                  4352325769, TRUE,
  
  "Sawyer Yates",    "Justin",                   3857221773, FALSE,
  "Sawyer Yates",    "Emilee",                   4357603564, FALSE,
  
  "Kolton York",     "Nathan",                   4354069205, TRUE,
  "Kolton York",     "Jillian",                  8323388208, FALSE,
  
  "leader",          "Brody",                    3852445930, TRUE,
  "leader",          "Kyle",                     4358900741, TRUE,
  "leader",          "Gabe",                     2086545105, TRUE,
  "leader",          "Amanda",                   8018642081, TRUE
)

new_thread %>% 
  full_join(existing_thread, by = "phone_number") %>% 
  view()
