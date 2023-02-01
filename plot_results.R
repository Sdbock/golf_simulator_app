
plot_results <- function(sim_output, constant = 400) {

  matches <- sim_output$Simulations
  
  results <- matches %>% 
    mutate(across(everything(), ~ round(.x)),
           diff = player1_score - player2_score,
           outcome = case_when(
             diff < 0 ~ "Player 1 wins",
             diff > 0 ~ "Player 2 wins",
             diff == 0 ~ "Tie"
           ),
           noise = runif(nrow(matches), 0, 1))
  
  p1_hdc <- sim_output$Results[[1]][[1]]
  p2_hdc <-  sim_output$Results[[1]][[2]]
  
  p1_win <- sim_output %>% pluck("Results","Win percentage",1)
  p2_win <- sim_output %>% pluck("Results","Win percentage",2)
  
  new_order <- c("#E6CED4","#76908E","#F1FAFE","#C8B7E4")
  
  tick_colors <- c(rep("#BA9197",3),"black",rep(new_order[2],3))
  
  results %>% 
    sample_n(100) %>% 
    filter(diff != 0) %>% 
    mutate(outcome = factor(outcome, levels = c("Player 1 wins", "Player 2 wins"))) %>% 
    ggplot(aes(x = diff, y = noise, fill = outcome)) +
    geom_vline(xintercept = 0, size = 1.5) +
    geom_jitter(size = 8/400 * constant, height = .05, pch = 21, color = "white") +
    scale_x_continuous(breaks = c(-20,-10,-5,0,5,10,20),
                       limits = c(-30,30),
                       labels = c(str_wrap(glue::glue("{p1_hdc} wins by 20 strokes"), width = 10),"10","5","Tie","5","10",str_wrap(glue::glue("{p2_hdc} wins by 20 strokes"),width = 10))
    ) +
    labs(
      x = NULL,
      fill = NULL) +
    scale_fill_manual(labels = c(glue::glue("{p1_hdc} wins"), glue::glue("{p2_hdc} wins")),
                      values = c(new_order[1], new_order[2]), 
                      limits = c("Player 1 wins", "Player 2 wins"),
                      drop = FALSE) +
    
    annotate("text", x = -17, y = 1.5, label = glue::glue("{p1_hdc} Wins\n{scales::percent(p1_win, accuracy = .1, trim = TRUE)} of Matches"), fontface = "bold", size = 5.5/400 * constant, color = "#BA9197") +
    annotate("text", x = 17, y = 1.5, label = glue::glue("{p2_hdc} Wins\n{scales::percent(p2_win, accuracy = .1, trim = TRUE)} of Matches"), fontface = "bold", size = 5.5/400 * constant, color = new_order[2]) +
    theme(
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.key = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(vjust = .2, color = tick_colors, face = "bold"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      plot.background = element_rect(color = "white", fill = "white"),
      panel.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_line(color = tick_colors),
      text = element_text(size = 15/400 * constant),
      plot.margin = margin(4,0,0,0, "cm")
    ) +
      coord_cartesian(ylim = c(-.05, 1.05),clip = "off") 
  
}
