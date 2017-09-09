

##instructions (until month_eval_formatted data file is created)
# 1. run `load data` chunk in "Data Formatting and Manipulations.Rmd"
# 2. reduce columns to speed up processing:
#  april_eval_base <- april_eval_base[, .(Event, WhiteElo, BlackElo, Movetext)]
# 3. run the `event` and `elo` chucks in "Data Formatting and Manipulations.Rmd"
# 4. run the rest of this file


##set up data

#obtain white's first move
#categorize it as e4, d4, or other
april_eval_base[, FirstMove := sub("1[.] ([a-h1-8KQRBNxO=#+-]+).*", "\\1", Movetext)][, FirstMove := ifelse(FirstMove != "e4" & FirstMove != "d4", "other", FirstMove)]

#drop the Movetext column to speed up processing
april_eval_base[, Movetext := NULL]

#transform the move into a factor
april_eval_base$FirstMove <- factor(april_eval_base$FirstMove, levels = c("e4", "d4", "other"))

#create the datasets for the visualizations
heatmap_base_facet <- april_eval_base[, .(WhiteEloRange, BlackEloRange, FirstMove, TimeControlVariation, Tournament)]
heatmap_base <- heatmap_base_facet[, .(WhiteEloRange, BlackEloRange, FirstMove)]
rm(april_eval_base)


##original heatmap

#count the number of each first move by the white and black players' ranges
heatmap_base <- heatmap_base[, .N, by = .(WhiteEloRange, BlackEloRange, FirstMove)]

#cast the resulting count into a wide table with columns for both elo ranges
heatmap_base <- data.table::dcast(heatmap_base, WhiteEloRange+BlackEloRange ~ FirstMove, fill = 0, value.var = "N")

#calculate the difference in elo ranges between the players
#create a key for this elo range combination
heatmap_base[, level_diff := as.numeric(BlackEloRange) - as.numeric(WhiteEloRange)][, players := paste(WhiteEloRange, BlackEloRange)]

#determine the most used first move for each elo range combination
#create the color to describe this combination
heatmap_base[, max := pmax(e4, d4, other)][, rgb_max := rgb(e4/max, other/max, d4/max)]

#create the named list of colors by player combination key
colors <- setNames(heatmap_base$rgb_max, heatmap_base$player)

#create the heatmap
heatmap_base %>%
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


##breakout heatmap

#count the number of each first move by the white and black players' ranges
heatmap_base_facet <- heatmap_base_facet[, .N, by = .(WhiteEloRange, BlackEloRange, FirstMove, TimeControlVariation, Tournament)]

#cast the resulting count into a wide table with columns for both elo ranges
heatmap_base_facet <- data.table::dcast(heatmap_base_facet, WhiteEloRange+BlackEloRange+TimeControlVariation+Tournament ~ FirstMove, fill = 0, value.var = "N")

#calculate the difference in elo ranges between the players
#create a key for this elo range combination
heatmap_base_facet[, level_diff := as.numeric(BlackEloRange) - as.numeric(WhiteEloRange)][, players := paste(WhiteEloRange, BlackEloRange, TimeControlVariation, Tournament)]

#determine the most used first move for each elo range combination
#create the color to describe this combination
heatmap_base_facet[, max := pmax(e4, d4, other)][, rgb_max := rgb(e4/max, other/max, d4/max)]

#create the named list of colors by player combination key
colors <- setNames(heatmap_base_facet$rgb_max, heatmap_base_facet$player)

#create the heatmap
heatmap_base_facet %>%
  filter(TimeControlVariation != "Correspondence") %>%
  ggplot() + 
  geom_raster(aes(x = WhiteEloRange, y = level_diff, fill = players)) +
  geom_hline(aes(yintercept = .5), color = "White", size = 1) + 
  geom_hline(aes(yintercept = -.5), color = "White", size = 1) + 
  geom_vline(aes(xintercept = 6.5), color = "White", size = 1.5) + 
  scale_fill_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "First Move Preference: Red for e4, Blue for d4, Green for Others",
       subtitle = "Players are more likely attempt different openings in shorter time controls",
       x = "White Elo Range",
       y = "# of Elo Ranges Black Is Above White") + 
  theme(legend.position = "none") +
  facet_grid(Tournament~TimeControlVariation)

