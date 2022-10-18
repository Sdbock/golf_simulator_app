## updated on 10.18 to remove avg. score

scores <- feather::read_feather("scores.feather")


sim_match <- function(data = scores, p1_hdc = "5", p2_hdc = "15", n_sims = 20000, net_scores = FALSE, return_table = TRUE, return_rounds = FALSE) {

score <- ifelse(net_scores == TRUE, "net_score", "score_norm")

  
sims <- 
    tibble(
      player1_score = scores %>% filter(handicap_f == p1_hdc) %>% sample_n(n_sims, replace = TRUE) %>% pull(score),
      player2_score = scores %>% filter(handicap_f == p2_hdc) %>% sample_n(n_sims, replace = TRUE) %>% pull(score)
    )
  

results_sum <- 
  sims %>% 
  mutate(winner = case_when(
    player1_score < player2_score ~ "player1",
    player1_score > player2_score ~ "player2",
    player1_score == player2_score ~ "Tie"
  )) %>% 
  group_by(winner) %>%
  mutate(
    wins = n()
    #avg_win_score = round(if_else(winner == "player1", mean(player1_score), mean(player2_score)), 0)
  ) %>%
  distinct(winner, wins) %>%
  ungroup() %>%
  mutate(win_pct = round(wins/sum(wins),3),
         Player = case_when(
            winner == "player1" ~ paste0(p1_hdc,"-Hdcp"),
            winner == "player2" ~ paste0(p2_hdc,"-Hdcp"),
            winner == "Tie" ~ "Tie"
         )) %>% 
  select(
    Player,
    "Win percentage" = win_pct
  ) 

# avg_scores <- 
#   sims %>% 
#   summarise(across(everything(), ~ round(mean(.x), 1))) %>%
#   pivot_longer(everything(),
#                values_to = "Average score across all rounds",
#                names_to = "Player") %>% 
#   mutate(
#     Player = case_when(
#       Player == "player1_score" ~ paste0(p1_hdc,"-Hdcp"),
#       Player == "player2_score" ~ paste0(p2_hdc,"-Hdcp"),
#       Player == "Tie" ~ "Tie"
#     ))



#results_sum <- left_join(results_sum, avg_scores, by = "Player")
  
results_sum <- results_sum %>% 
  mutate(order = factor(Player, 
                         levels = c(paste0(p1_hdc, "-Hdcp"),paste0(p2_hdc, "-Hdcp"),"Tie"),
                        labels = c("1","2","3"))) %>% 
  arrange(order) %>% 
  select(-order)


output <- lst(
  "Simulations" = sims, 
  "Results" = results_sum
)

scoring <- if_else(net_scores == TRUE, "net","gross")

#table <- results_table(output)
#plot <- plot_results(output)
#


summary <-
str_c(
  glue::glue(
    "Based on {scales::comma(n_sims)} simulated rounds of {scoring} stroke play between a {results_sum$Player[1]} and a {results_sum$Player[2]}, a {results_sum$Player[1]} wins {scales::percent(results_sum$`Win percentage`[1],accuracy = .1,trim = TRUE)} of the time, and a {results_sum$Player[2]} wins {scales::percent(results_sum$`Win percentage`[2],accuracy = 0.1,trim = TRUE)} of the time. This sample of 100 matches gives a sense of the possible outcomes from this matchup."
      )
    )

results <- lst(table,
               summary,
               "Simulations" = sims, 
               "Results" = results_sum)


return(results)

}
