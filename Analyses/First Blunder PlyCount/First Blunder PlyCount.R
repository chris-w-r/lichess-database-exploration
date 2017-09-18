

april_eval_base %>%
  filter(TimeControlVariation != "UltraBullet",
         TimeControlVariation != "Correspondence") %>%
  group_by(WhiteEloRange, BlackEloRange, TimeControlVariation) %>%
  summarise(MedFirstBlunder = median(FirstBlunder),
            MedPlyCountAfterFirstBlunder = median(PlyCountAfterFirstBlunder),
            AvgFirstBlunderWhite = mean(Player == "White"),
            n = n()) %>%
  mutate(FirstBlunderPlayer = factor(if_else(AvgFirstBlunderWhite > .65, 
                                             ">65% White", 
                                             if_else(AvgFirstBlunderWhite < .35, 
                                                     ">65% Black", 
                                                     "Equally Likely")), 
                                     levels = c("Equally Likely",
                                                ">65% White",
                                                ">65% Black"))) %>%
  filter(n >= 100) %>%
  ggplot() +
  geom_point(aes(x = WhiteEloRange, 
                 y = BlackEloRange, 
                 size = MedFirstBlunder, 
                 color = MedPlyCountAfterFirstBlunder,
                 shape = FirstBlunderPlayer)) +
  geom_hline(aes(yintercept = 6.5), size = 1.5) + 
  geom_vline(aes(xintercept = 6.5), size = 1.5) + 
  geom_abline(aes(intercept = .5, slope = 1), size = 1) +
  geom_abline(aes(intercept = -.5, slope = 1), size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Lower rated players are more likely to blunder early (larger shapes)\nHigher rated players are more likely to capitalize on blunders quickly (brighter colors)",
       subtitle = "Displaying rating pairings having played in at least 0.01% of games analyzed (~100 games)",
       caption = "Data from https://database.lichess.org",
       x = "White Elo Range",
       y = "Black Elo Range",
       size = "Median of move number (plycount) of first blunder",
       color = "Median of number of moves played after first blunder\n(Total Game PlyCount - PlyCount of First Blunder)",
       shape = "Player making first blunder") +
  viridis::scale_color_viridis(option = "plasma", direction = -1) +
  scale_size_continuous(trans = "reverse") +
  facet_wrap(~TimeControlVariation)

