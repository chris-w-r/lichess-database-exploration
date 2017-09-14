

##instructions (until month_eval_formatted data file is created)
# 1. run `load data` chunk in "Data Formatting and Manipulations.Rmd"
# 2. reduce columns to speed up processing:
#  april_eval_base <- april_eval_base[, .(Event, Result, WhiteElo, BlackElo, ECO, Opening, Termination)]
# 3. run the `event`, `result`, `elo`, `eco`, and `termination` chucks in "Data Formatting and Manipulations.Rmd"
# 4. run the rest of this file


##set up data

#only keep games ending with either White or Black Winning or a Draw
april_eval_base <- april_eval_base[Winner != "Abandoned"]

#only keep games that end normally or via time forfeit
april_eval_base <- april_eval_base[Termination == "Normal" | Termination == "Time forfeit"]

#create the datasets for the visualizations
scatterplot_base_tableau <- april_eval_base[, .(ECOVolume, ECOGrouping, ECO, Opening, WhiteEloRange, BlackEloRange, TimeControlVariation, Tournament, Termination, Winner)]
scatterplot_base <- scatterplot_base_tableau[, .(ECOVolume, ECOGrouping, ECO, Opening, Winner)]
rm(april_eval_base)


##original scatterplot

#count the number of wins by ECO and Opening variation
scatterplot_base <- scatterplot_base[, .N, by = .(ECOVolume, ECOGrouping, ECO, Opening, Winner)]

#cast the resulting count into a wide table with columns for all winner types
scatterplot_base <- data.table::dcast(scatterplot_base, ECOVolume+ECOGrouping+ECO+Opening ~ Winner, fill = 0, value.var = "N")

#sum the number of games played
#calculate the win rates for each ECO
scatterplot_base[, Games := White + Black + Draw][, WhiteWinPercent := White / Games][, BlackWinPercent := Black / Games][, DrawPercent := Draw / Games][, WhiteMinusBlackWinPercent := WhiteWinPercent - BlackWinPercent]

#set colors for the ECOGroupings
colors <- setNames(c(RColorBrewer::brewer.pal(9, "Reds")[5:9],
                     RColorBrewer::brewer.pal(9, "Greys")[6:8],
                     RColorBrewer::brewer.pal(9, "Blues")[c(6,8)],
                     RColorBrewer::brewer.pal(9, "Oranges")[c(6,8)],
                     RColorBrewer::brewer.pal(9, "Purples")[c(6,8)]),
                   levels(scatterplot_base$ECOGrouping))

#filter data to openings used in at elast 0.1% of games
scatterplot_base <- scatterplot_base[Games >= 1112]

#create annotation boxes stating which side performs better
white_side_annotation <- scatterplot_base %>%
  summarise(
    WhiteMinusBlackWinPercent = .01,
    DrawPercent = -Inf,
    label = "White Wins\nMore Often"
  )
black_side_annotation <- scatterplot_base %>%
  summarise(
    WhiteMinusBlackWinPercent = -.01,
    DrawPercent = -Inf,
    label = "Black Wins\nMore Often"
  )

#create labels for top 4 most successful openings for White and Black and top 2 Draw openings
game_labels <- bind_rows(
  scatterplot_base %>%
    filter(row_number(desc(WhiteWinPercent)) <= 4) %>%
    mutate(label = stringr::str_wrap(paste0(Opening, " (", ECO, ")"), width = 17)),
  scatterplot_base %>%
    filter(row_number(desc(BlackWinPercent)) <= 4) %>%
    mutate(label = stringr::str_wrap(paste0(Opening, " (", ECO, ")"), width = 17)),
  scatterplot_base %>%
    filter(row_number(desc(DrawPercent)) <= 2) %>%
    mutate(label = stringr::str_wrap(paste0(Opening, " (", ECO, ")"), width = 17))
)

#create the scatterplot
scatterplot_base %>%
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "Black", size = 1.25) +
  ggrepel::geom_label_repel(aes(x = WhiteMinusBlackWinPercent, y = DrawPercent, label = label, color = ECOGrouping), data = game_labels, show.legend = FALSE) +
  geom_label(aes(x = WhiteMinusBlackWinPercent, y = DrawPercent, label = label), data = white_side_annotation, vjust = "bottom", hjust = "left") +
  geom_label(aes(x = WhiteMinusBlackWinPercent, y = DrawPercent, label = label), data = black_side_annotation, vjust = "bottom", hjust = "right", color = "White", fill = "Black") +
  geom_point(aes(x = WhiteMinusBlackWinPercent, y = DrawPercent, size = Games, shape = ECOVolume, color = ECOGrouping), alpha = .75) +
  labs(
    title = "4 highest winning percentage differences for White and Black & 2 highest Draw percentage Openings labeled",
    subtitle = "Displaying Openings used in at least 0.1% of games analyzed (~1100 games)",
    caption = "Data from https://database.lichess.org",
    x = "Winning Percentage Difference (White Winning Percentage - Black Winning Percentage)",
    y = "Draw Percentage",
    shape = "ECO Volume",
    color = "ECO Grouping"
  ) + 
  scale_x_continuous(breaks = seq(-.35, .35, by = .10), labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = colors, breaks = levels(scatterplot_base$ECOGrouping),
                     labels = stringr::str_wrap(levels(scatterplot_base$ECOGrouping), width = 64))
  

##data for tableau

#count the number of wins by ECO and Opening variation
scatterplot_base_tableau <- scatterplot_base_tableau[, .N, by = .(ECOVolume, ECOGrouping, ECO, Opening, WhiteEloRange, BlackEloRange, TimeControlVariation, Tournament, Termination, Winner)]

#cast the resulting count into a wide table with columns for all winner types
scatterplot_base_tableau <- data.table::dcast(scatterplot_base_tableau, ECOVolume+ECOGrouping+ECO+Opening+WhiteEloRange+BlackEloRange+TimeControlVariation+Tournament+Termination ~ Winner, fill = 0, value.var = "N")

#sum the number of games played
scatterplot_base_tableau[, Games := White + Black + Draw]

#retitle columns
setnames(scatterplot_base_tableau, "White", "WhiteWins")
setnames(scatterplot_base_tableau, "Black", "BlackWins")
setnames(scatterplot_base_tableau, "Draw", "Draws")

