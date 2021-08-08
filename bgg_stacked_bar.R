library(tidyverse)

games = read.csv('games_cleaned.csv')

numbers_of_bins = 5
games = games%>%
  mutate(owned_bins = cut(owned, 
                          breaks = unique(quantile(owned,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                          include.lowest=TRUE))
games_owned_bins = games %>% 
  group_by(owned_bins) %>%
  summarise_if(is.numeric, list(mean), na.rm = TRUE)

# firstcol = which(colnames(games_owned_bins)=="mech_Acting") # just cause it is.
# lastcol = which(colnames(games_owned_bins)=="mech_Zone.of.Control") # just cause it is.
# owned_mech_top_20 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:20] ]
# games_owned_mech_20 = games_owned_bins %>% 
#   select(owned_bins,all_of(owned_mech_top_20),-mechanics_sum) %>%
#   gather(top_mechs, per_, -c(owned_bins)) %>%
#   mutate(top_mechs = gsub('\\.',' ',gsub(".*_", "", top_mechs)))

graph_stacked_bar <- function(start_col,last_col){
# create df of the top 20 groups in question
firstcol = which(colnames(games_owned_bins)==start_col) # just cause it is.
lastcol = which(colnames(games_owned_bins)==last_col) # just cause it is.
top_20 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:20] ]
top_20_grouped = games_owned_bins %>% 
  select(owned_bins,all_of(top_20)) %>%
  gather(top_groups, per_, -c(owned_bins)) %>%
  mutate(top_groups = gsub("\\s+", " ",
                           gsub('\\.',' ',
                                gsub(".*_", "", top_groups))))
# to create ordering of graph.
top_20_total = top_20_grouped %>% 
  group_by(top_groups) %>% 
  summarise(total = sum(per_)) 
top_20_grouped = merge(top_20_grouped,top_20_total, by = 'top_groups',
                       all.x = TRUE)
plot_order = unlist(top_20_grouped %>% 
                      mutate(owned_bins= as.numeric(owned_bins)) %>% filter(owned_bins==5) %>%
                      mutate(per_ = per_/total) %>% arrange(per_) %>% select(top_groups))
top_20_grouped$top_groups <- factor(top_20_grouped$top_groups, 
                                    levels = plot_order)
# create graph
stacked_per = top_20_grouped %>%
  mutate(owned_bins = case_when(owned_bins=='[2,126]' ~ '126 - 2',
                                owned_bins=='(126,234]' ~ '234 - 127',
                                owned_bins=='(234,472]' ~ '472 - 235',
                                owned_bins=='(472,1.26e+03]' ~ '1,262 - 472',
                                owned_bins=='(1.26e+03,1.45e+05]' ~ '144,727 - 1,263'
  )) %>%
  mutate(owned_bins=factor(owned_bins, levels=c('126 - 2', '234 - 127', 
                                                '472 - 235','1,262 - 472',
                                                '144,727 - 1,263'))) %>%
  rename(GamesOwned = owned_bins) %>%
  ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
  geom_bar(position="fill", stat="identity") +
  scale_shape_manual(values=c('126 - 2', '234 - 127', '472 - 235','1,262 - 472','144,727 - 1,263')) +
  scale_fill_manual(values = c('126 - 2'="#83AF9B", # FE4365   FC9D9A   F9CDAD   C8C8A9   83AF9B 
                               '234 - 127'="#C8C8A9",
                               '472 - 235'="#F9CDAD",
                               '1,262 - 472'="#FC9D9A",
                               '144,727 - 1,263'="#FE4365"),
                    guide = guide_legend(reverse = TRUE))+ 
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = '#F0F0F0'),
        panel.background = element_rect(fill = '#F0F0F0'),
        panel.grid.major = element_line(colour = "#CDCDCD"),
        panel.grid.minor = element_line(colour = "#CDCDCD"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="top",
        legend.background = element_rect(fill="#F0F0F0", 
                                         size=0.5, linetype="solid"),
        axis.text.y = element_text(margin = margin(r = -20))
  )
return(stacked_per)
}

mech_stacked = graph_stacked_bar("mech_Acting",
                           "mech_Global.Mechanics.Average")
cat_stacked = graph_stacked_bar("cat_Abstract.Strategy",
                                 "cat_Global.Categorys.Average")
mech_stacked
cat_stacked












start_col = 'mech_Acting'
last_col = 'mech_Zone.of.Control'



firstcol = which(colnames(games_owned_bins)==start_col) # just cause it is.
lastcol = which(colnames(games_owned_bins)==last_col) # just cause it is.
top_20 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:20] ]
top_20_grouped = games_owned_bins %>% 
  select(owned_bins,all_of(top_20)) %>%
  gather(top_groups, per_, -c(owned_bins)) %>%
  mutate(top_groups = gsub("\\s+", " ",
                           gsub('\\.',' ',
                                gsub(".*_", "", top_groups))))
# to create ordering of graph.
plot_order = unlist(top_20_grouped %>% 
  group_by(top_groups) %>% 
  summarise(total = sum(per_)) %>% 
  arrange(total) %>% select(top_groups))

top_20_grouped$top_groups <- factor(top_20_grouped$top_groups, 
                                    levels = plot_order)

stacked_per = top_20_grouped %>%
  mutate(owned_bins = case_when(owned_bins=='[2,126]' ~ '126 - 2',
                                owned_bins=='(126,234]' ~ '234 - 127',
                                owned_bins=='(234,472]' ~ '472 - 235',
                                owned_bins=='(472,1.26e+03]' ~ '1,262 - 472',
                                owned_bins=='(1.26e+03,1.45e+05]' ~ '144,727 - 1,263'
  )) %>%
  mutate(owned_bins=factor(owned_bins, levels=c('126 - 2', '234 - 127', 
                                                '472 - 235','1,262 - 472',
                                                '144,727 - 1,263'))) %>%
  rename(GamesOwned = owned_bins) %>%
  ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
  geom_bar(position="stack", stat="identity") +
  scale_shape_manual(values=c('126 - 2', '234 - 127', '472 - 235','1,262 - 472','144,727 - 1,263')) +
  scale_fill_manual(values = c('126 - 2'="#83AF9B", # FE4365   FC9D9A   F9CDAD   C8C8A9   83AF9B 
                               '234 - 127'="#C8C8A9",
                               '472 - 235'="#F9CDAD",
                               '1,262 - 472'="#FC9D9A",
                               '144,727 - 1,263'="#FE4365"),
                    guide = guide_legend(reverse = TRUE))+ 
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = '#F0F0F0'),
        panel.background = element_rect(fill = '#F0F0F0'),
        panel.grid.major = element_line(colour = "#CDCDCD"),
        panel.grid.minor = element_line(colour = "#CDCDCD"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="top",
        legend.background = element_rect(fill="#F0F0F0", 
                                         size=0.5, linetype="solid"),
        axis.text.y = element_text(margin = margin(r = -20))
  )


stacked_per



