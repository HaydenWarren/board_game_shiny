library(tidyverse)

graph_stacked_bar <- function(start_col,last_col,title_){
  games_owned_bins = games %>% 
    group_by(owned_bins) %>%
    summarise_if(is.numeric, list(mean), na.rm = TRUE)
  # create df of the top 20 groups in question
  firstcol = which(colnames(games_owned_bins)==start_col) # manually named
  lastcol = which(colnames(games_owned_bins)==last_col) # manually named
  top_20 = colnames(games_owned_bins[c(firstcol:lastcol)]
                    )[rev(sort.list(colSums(games_owned_bins[c(
                      firstcol:lastcol
                      )])))[1:20]]
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
                        mutate(owned_bins= as.numeric(owned_bins)) %>% 
                        filter(owned_bins==5) %>%
                        mutate(per_ = per_/total) %>% arrange(per_) %>% 
                        select(top_groups))
  top_20_grouped$top_groups <- factor(top_20_grouped$top_groups, 
                                      levels = plot_order)
  # create graph
  stacked_per = top_20_grouped %>%
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
    rename(GamesOwned = owned_bins) %>%
    ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
    geom_bar(position="fill", stat="identity") +
    scale_shape_manual(values=c('126 - 2', 
                                '234 - 127', 
                                '472 - 235',
                                '1,262 - 472',
                                '144,727 - 1,263')) +
    scale_fill_manual(values = c('126 - 2'="#E76F51", #e60012 #ad0c2f #73184d #3a246a #003087
                                 '234 - 127'="#F4A261",
                                 '472 - 235'="#E9C46A",
                                 '1,262 - 472'="#2A9D8F",
                                 '144,727 - 1,263'="#264653"),
                      guide = guide_legend(reverse = TRUE))+ 
    theme(axis.title.y=element_blank(),
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
    )+ 
    ggtitle(paste('Proportion of Top 20 Game',title_,
                  'by Games Owned.'))
  return(stacked_per)
}

graph_total_bar <- function(start_col,last_col,title_){
  games_owned_bins = games %>% 
    group_by(owned_bins) %>%
    summarise_if(is.numeric, list(sum), na.rm = TRUE)
  
  firstcol = which(colnames(games_owned_bins)==start_col) # manually named
  lastcol = which(colnames(games_owned_bins)==last_col) # manually named
  top_20 = colnames(games_owned_bins[c(firstcol:lastcol)]
                    )[rev(sort.list(colSums(games_owned_bins[c(
                      firstcol:
                        lastcol
                      )])))[1:20]]
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
    rename(GamesOwned = owned_bins) %>%
    ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
    geom_bar(position="stack", stat="identity") +
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
          legend.position="top",
          legend.background = element_rect(fill="#F0F0F0", 
                                           size=0.5, linetype="solid"),
          axis.text.y = element_text(margin = margin(r = -20))
    ) +
    labs(x = "Games Owned") + 
    ggtitle(paste('Top 20',title_,'Count of \nReported Games Owned'))
  return(stacked_per)
}

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
    ggtitle(paste(title_))
  return(graph_output)
}

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

graph_stacked_bar_group <- function(group,group_parent){
  title_ = gsub("\\s+", " ",gsub('\\.',' ',gsub(".*_", "", group)))
  graph_list = c(group,group_parent)
  games_owned_bins = games %>% 
    group_by(owned_bins) %>%
    summarise_if(is.numeric, list(mean), na.rm = TRUE)
  # create df of the top 20 groups in question
  top_20_grouped = games_owned_bins %>% 
    select(owned_bins,all_of(graph_list)) %>%
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
                        mutate(owned_bins= as.numeric(owned_bins)) %>% 
                        filter(owned_bins==5) %>%
                        mutate(per_ = per_/total) %>% arrange(per_) %>% 
                        select(top_groups))
  top_20_grouped$top_groups <- factor(top_20_grouped$top_groups, 
                                      levels = plot_order)
  # create graph
  stacked_per = top_20_grouped %>%
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
    rename(GamesOwned = owned_bins) %>%
    ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
    geom_bar(position="fill", stat="identity") +
    scale_shape_manual(values=c('126 - 2', '234 - 127', 
                                '472 - 235','1,262 - 472',
                                '144,727 - 1,263')) +
    scale_fill_manual(values = c('126 - 2'="#83AF9B", 
                                 '234 - 127'="#C8C8A9",
                                 '472 - 235'="#F9CDAD",
                                 '1,262 - 472'="#FC9D9A",
                                 '144,727 - 1,263'="#FE4365"),
                      guide = guide_legend(reverse = TRUE))+ 
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.background = element_rect(fill = '#F0F0F0'),
          panel.background = element_rect(fill = '#F0F0F0'),
          panel.grid.major = element_line(colour = "#CDCDCD"),
          panel.grid.minor = element_line(colour = "#CDCDCD"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="top",
          legend.background = element_rect(fill="#F0F0F0", 
                                           size=0.5, linetype="solid"),
          axis.text.y = element_text(margin = margin(r = -20))
    )+ 
    ggtitle(paste('Proportion of Top 20 Game',title_,
                  'by Games Owned.'))
  return(stacked_per)
}

graph_total_bar_group <- function(group){
  title_ = gsub("\\s+", " ",gsub('\\.',' ',gsub(".*_", "", group)))
  
  games_owned_bins = games %>% 
    group_by(owned_bins) %>%
    summarise_if(is.numeric, list(sum), na.rm = TRUE)
  graph_list = c(group)
  top_20_grouped = games_owned_bins %>% 
    select(owned_bins,all_of(graph_list)) %>%
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
    rename(GamesOwned = owned_bins) %>%
    ggplot(aes(fill=GamesOwned, x=per_, y=top_groups)) + 
    geom_bar(position="dodge", stat="identity") +
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
          legend.position="top",
          legend.background = element_rect(fill="#F0F0F0", 
                                           size=0.5, linetype="solid"),
          axis.text.y = element_text(margin = margin(r = -20))
    ) +
    labs(x = "Games Owned") + 
    ggtitle(paste(title_,'\nReported Games Owned\non BoardGameGeek.com'))
  return(stacked_per)
}