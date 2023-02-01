
sim_match <- 
  function(data = data, p1_hdc = "5", p2_hdc = "15", n_sims = 10000, net_scores = FALSE) {

        score <- ifelse(net_scores == TRUE, "net_score", "score_norm")
        
          
        sims <- 
            tibble(
              player1_score = data %>% filter(handicap_f == p1_hdc) %>% sample_n(n_sims, replace = TRUE) %>% pull(score),
              player2_score = data %>% filter(handicap_f == p2_hdc) %>% sample_n(n_sims, replace = TRUE) %>% pull(score)
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
        
          
        results_sum <- results_sum %>% 
          mutate(order = factor(Player, 
                                 levels = c(paste0(p1_hdc, "-Hdcp"),paste0(p2_hdc, "-Hdcp"),"Tie"),
                                labels = c("1","2","3"))) %>% 
          arrange(order) %>% 
          select(-order)
        
        sims <- 
          sims %>% 
          ungroup() %>%
          slice_sample(n = 100)
        
        output <- lst(
          "Simulations" = sims, 
          "Results" = results_sum
        )
        
        scoring <- if_else(net_scores == TRUE, "net","gross")
        
        
        summary <-
        str_c(
          glue::glue(
            "Based on {scales::comma(n_sims)} simulated rounds of {scoring} stroke play between a {results_sum$Player[1]} and a {results_sum$Player[2]}, a {results_sum$Player[1]} wins {scales::percent(results_sum$`Win percentage`[1],accuracy = .1,trim = TRUE)} of the time, and a {results_sum$Player[2]} wins {scales::percent(results_sum$`Win percentage`[2],accuracy = 0.1,trim = TRUE)} of the time. This sample of 100 matches gives a sense of the possible outcomes from this matchup."
              )
            )
        
        results <- lst(
                       summary,
                       "Simulations" = sims, 
                       "Results" = results_sum)
        
        
    return(results)

}
