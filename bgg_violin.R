library(tidyverse)
# read data
games = read.csv('games_cleaned.csv')
# prepare data for graphing by adding the 5 bins.
numbers_of_bins = 5
games = games%>%
  mutate(owned_bins = cut(owned, 
                          breaks = unique(
                            quantile(
                              owned,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                          include.lowest=TRUE))

games_owned_bins = games %>%
  group_by(owned_bins) %>%
  summarise_if(is.numeric, list(sum), na.rm = TRUE)
# construct graph function
graph_violinplot <- function(y_axis,title_,adjust_val){
  graph_output = games %>%
    filter(!!as.symbol(y_axis)!=0) %>%
    mutate(owned_bins = case_when(
      owned_bins=='[2,126]' ~ '126 - 2',
      owned_bins=='(126,234]' ~ '234 - 127',
      owned_bins=='(234,472]' ~ '472 - 235',
      owned_bins=='(472,1.26e+03]' ~ '1,262 - 472',
      owned_bins=='(1.26e+03,1.45e+05]' ~ '144,727 - 1,263'
    )) %>%
    mutate(owned_bins=factor(owned_bins, levels=c('126 - 2', '234 - 127',
                                                  '472 - 235','1,262 - 472',
                                                  '144,727 - 1,263'))) %>%
    ggplot(aes_string(x='owned_bins',y=y_axis)) +
    geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
    geom_boxplot(width=0.08, fill="#F0F0F0",outlier.shape=NA)+
    coord_flip() +
    scale_x_discrete(labels=c(
      '126 - 2'="2 to 126\nGames Owned", 
      '234 - 127'="127 to 234",
      '472 - 235'="235 to 472",
      '1,262 - 472'="472 to 1,262",
      '144,727 - 1,263'="1,263 to 144,727\nGames Owned")) +
    scale_shape_manual(values=c('126 - 2', 
                                '234 - 127', 
                                '472 - 235',
                                '1,262 - 472',
                                '144,727 - 1,263')) +
    scale_fill_manual(values = c('126 - 2'="#83AF9B", 
                                 '234 - 127'="#C8C8A9",
                                 '472 - 235'="#F9CDAD",
                                 '1,262 - 472'="#FC9D9A",
                                 '144,727 - 1,263'="#FE4365"),
                      guide = guide_legend(reverse = TRUE))+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          plot.background = element_rect(fill = '#F0F0F0'),
          panel.background = element_rect(fill = '#F0F0F0'),
          panel.grid.major = element_line(colour = "#CDCDCD"),
          panel.grid.minor = element_line(colour = "#CDCDCD"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="npne",
          legend.background = element_rect(fill="#F0F0F0",
                                           size=0.5, linetype="solid"),
          axis.text.y = element_text(margin = margin(r = -20))
    )+
    ggtitle(paste('Proportion of Top 20 Game',title_))
  return(graph_output)
}
# test graph for each metric
# game rank
g_rank_violin = graph_violinplot('g_rank','rank graph',1.4)
g_rank_violin
# average rating
avg_rating_violin = graph_violinplot('avg_rating','average ranking graph',1.4)
avg_rating_violin
# complexity score
complex_violin = graph_violinplot('complex','complex  graph',1.4)
complex_violin
# playtime
play_time_violin = graph_violinplot('play_time','Play Time  graph',1.4)
play_time_violin
# minimum age
min_age_violin = graph_violinplot('min_age','Minimum Age graph',1.4)
min_age_violin
# year published
year_violin = graph_violinplot('year_published','year published graph',1.4)
year_violin

# construct graph function
graph_group_violinplot <- function(x_axis,y_axis){
  games[x_axis] = as.factor(games[,x_axis])
  type_ = gsub("\\s+", " ",gsub('\\.',' ',gsub(".*_", "", x_axis)))
  output_graph = games %>%
    filter(!!as.symbol(y_axis)!=0) %>%
    ggplot(aes_string(x=x_axis,y=y_axis)) +
    geom_violin(aes_string(fill=x_axis)) +
    geom_boxplot(width=0.08, fill="#F0F0F0",outlier.shape=NA)+
    coord_flip() +
    scale_x_discrete(labels=c('1'=paste(type_,'Games'), 
                              '0'=paste0("Non-",type_," Games"))) +
    scale_shape_manual(values=c('1', '0')) +
    scale_fill_manual(values = c('1'="#FC9D9A",
                                 '0'="#F9CDAD"
    ))+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          plot.background = element_rect(fill = '#F0F0F0'),
          panel.background = element_rect(fill = '#F0F0F0'),
          panel.grid.major = element_line(colour = "#CDCDCD"),
          panel.grid.minor = element_line(colour = "#CDCDCD"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="npne",
          legend.background = element_rect(fill="#F0F0F0",
                                           size=0.5, linetype="solid"),
          axis.text.y = element_text(margin = margin(r = -20))
    )+
    ggtitle(paste('Proportion of',type_,'by',y_axis))
  return(output_graph)
}
# test violin graphs with myriad of game types and metrics
cat_g_rank = graph_group_violinplot('cat_Fantasy', 'g_rank')
cat_g_rank

cat_rating = graph_group_violinplot('cat_Fantasy', 'avg_rating')
cat_rating

cat_complex = graph_group_violinplot('cat_Fantasy', 'complex')
cat_complex

cat_time = graph_group_violinplot('cat_Fantasy', 'play_time')
cat_time

cat_age = graph_group_violinplot('cat_Fantasy', 'min_age')
cat_age

adult_g_rank = graph_group_violinplot('cat_Mature...Adult', 'g_rank')
adult_g_rank

adult_rating = graph_group_violinplot('cat_Mature...Adult', 'avg_rating')
adult_rating

adult_complex = graph_group_violinplot('cat_Mature...Adult', 'complex')
adult_complex

adult_time = graph_group_violinplot('cat_Mature...Adult', 'play_time')
adult_time

adult_age = graph_group_violinplot('mech_Variable.Player.Powers', 'avg_rating')
adult_age
