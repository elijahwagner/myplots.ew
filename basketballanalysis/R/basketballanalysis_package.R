cbbga24 <- read.fwf("http://kenpom.com/cbbga24.txt", widths = widths, strip.white = TRUE) |> 
  rename(
    Date = V1,
    Team1 = V2,
    Score1 = V3,
    Team2 = V4,
    Score2 = V5,
    MP = V6,
    OT = V7
  ) |> 
  mutate(Score_Difference = Score1 - Score2) |> 
  arrange(Team1) |> 
  select(-MP, -OT) |> 
  filter(substr(Date, 7, 10) == "2023")

results_suu <- team_function('Southern Utah') |> 
  mutate(Result = case_when(Team1=='Southern Utah' & Score_Difference > 0 ~ 'win',
                            Team2=='Southern Utah' & Score_Difference < 0 ~ 'win',
                            Score_Difference==0 ~'tie',
                            TRUE ~ 'loss')) |> 
  select(Result)
print(pull(results_suu |> 
             summarize(mean(Result=='win'))
))

team_results <- function(team) {
  team_result <- team_function(team) |> 
    mutate(Result = case_when(Team1==team & Score_Difference > 0 ~ 'win',
                              Team2==team & Score_Difference < 0 ~ 'win',
                              Score_Difference==0 ~'tie',
                              TRUE ~ 'loss')) |> 
    select(Result)
  pull(team_result |>
         summarize(mean(Result == 'win')))
}

all_teams <- unique(union(cbbga24$Team1, cbbga24$Team2))

win_ratio <- numeric(length(all_teams))

for (i in 1:length(all_teams)){
  win_ratio[i] <- team_results(all_teams[i])
}

whateveryouwant <- tibble(teams = all_teams, win_ratio = win_ratio) |>
  arrange()
head(whateveryouwant)

