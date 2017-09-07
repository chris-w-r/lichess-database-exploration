

#obtain white's first move
#categorize it as e4, d4, or other
april_eval_base[, FirstMove := sub("1[.] ([a-h1-8KQRBNxO=#+-]+).*", "\\1", Movetext)][, FirstMove := ifelse(FirstMove != "e4" & FirstMove != "d4", "other", FirstMove)]

#transform the move into a factor
april_eval_base$FirstMove <- factor(april_eval_base$FirstMove, levels = c("e4", "d4", "other"))

#count the number of each first move by the white and black players' ranges
april_eval_base <- april_eval_base[, .N, by = .(WhiteEloRange, BlackEloRange, FirstMove)]

#cast the resulting count into a wide table with columns for both elo ranges
april_eval_base <- data.table::dcast(april_eval_base, WhiteEloRange+BlackEloRange ~ FirstMove, fill = 0, value.var = "N")

#calculate the difference in elo ranges between the players
#create a key for this elo range combination
april_eval_base[, level_diff := as.numeric(BlackEloRange) - as.numeric(WhiteEloRange)][, players := paste(WhiteEloRange, BlackEloRange)]

#determine the most used first move for each elo range combination
#create the color to describe this combination
april_eval_base[, max := pmax(e4, d4, other)][, rgb_max := rgb(e4/max, other/max, d4/max)]

#create the named list of colors by player combination key
colors <- setNames(april_eval_base$rgb_max, april_eval_base$player)



#create the heatmap
april_eval_base %>%
  ggplot() + 
  geom_raster(aes(x = WhiteEloRange, y = level_diff, fill = players)) +
  geom_hline(aes(yintercept = .5), color = "White", size = 1) + 
  geom_hline(aes(yintercept = -.5), color = "White", size = 1) + 
  geom_vline(aes(xintercept = 6.5), color = "White", size = 1.5) + 
  scale_fill_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "First Move Preference: Red for e4, Blue for d4, Green for Others",
       subtitle = "Players progress from mostly e4 to a mix of e4 and d4 to mostly others as their Elo range increases",
       x = "White Elo Range",
       y = "# of Elo Ranges Black Is Above White") + 
  theme(legend.position = "none") 


