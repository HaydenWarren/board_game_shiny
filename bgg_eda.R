
library(tidyverse)
# library(data.table)
# look at the top publishers to look at different publishers.


games = read.csv('games_cleaned.csv')

# limit to after 1995 makes sense
numbers_of_bins = 5

games = games%>%
  mutate(owned_bins = cut(owned, 
                          breaks = unique(quantile(owned,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                                     include.lowest=TRUE))

games_owned_bins = games %>% 
  group_by(owned_bins) %>%
  summarise_if(is.numeric, list(mean), na.rm = TRUE)

adjust_val = 1.4
games %>% ggplot(aes(x=owned_bins,y=g_rank)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()

games %>% ggplot(aes(x=owned_bins,y=avg_rating)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()

games %>% filter(complex!=0) %>% ggplot(aes(x=owned_bins,y=complex)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()

games %>% ggplot(aes(x=owned_bins,y=play_time)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()
#not useful
games %>% ggplot(aes(x=owned_bins,y=min_players)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()
#not useful
games %>% ggplot(aes(x=owned_bins,y=max_players)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()

games %>% filter(min_age!=0) %>% ggplot(aes(x=owned_bins,y=min_age)) + 
  geom_violin(aes(fill=owned_bins),adjust = adjust_val) +
  geom_boxplot(width=0.08, fill="white",outlier.shape=NA)+ 
  coord_flip()

firstcol = which(colnames(games_owned_bins)=="mech_Acting") # just cause it is.
lastcol = which(colnames(games_owned_bins)=="mech_Zone.of.Control") # just cause it is.
owned_mech_top_8 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:8] ]
games_owned_mech_8 = games_owned_bins %>% 
  select(owned_bins,all_of(owned_mech_top_8),-mechanics_sum) %>%
  gather(top_mechs, per_, -c(owned_bins))

games_owned_mech_8 %>%
  mutate(owned_bins= as.numeric(owned_bins)) %>%
  ggplot(aes(x=owned_bins,y=per_)) + geom_line(aes(color = top_mechs))

firstcol = which(colnames(games_owned_bins)=="mech_Acting") # just cause it is.
lastcol = which(colnames(games_owned_bins)=="mech_Zone.of.Control") # just cause it is.
owned_mech_top_20 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:20] ]
games_owned_mech_20 = games_owned_bins %>% 
  select(owned_bins,all_of(owned_mech_top_20),-mechanics_sum) %>%
  gather(top_mechs, per_, -c(owned_bins)) %>%
  mutate(top_mechs = gsub('\\.',' ',gsub(".*_", "", top_mechs)))
  

games_owned_mech_20 %>%
  mutate(owned_bins= as.numeric(owned_bins)) %>%
  ggplot(aes(x=owned_bins,y=per_)) + geom_line(aes(color = top_mechs))

games_owned_bins = games %>% 
  group_by(owned_bins) %>%
  summarise_if(is.numeric, list(sum), na.rm = TRUE)
firstcol = which(colnames(games_owned_bins)=="cat_Abstract.Strategy") # just cause it is.
lastcol = which(colnames(games_owned_bins)=="cat_Zombies") # just cause it is.
owned_cat_top_8 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:8] ]
games_owned_mech_8 = games_owned_bins %>% 
  select(owned_bins,all_of(owned_cat_top_8),-mechanics_sum) %>%
  gather(top_mechs, per_, -c(owned_bins))

games_owned_mech_8 %>%
  mutate(owned_bins= as.numeric(owned_bins)) %>%
  ggplot(aes(x=owned_bins,y=per_)) + geom_line(aes(color = top_mechs))

firstcol = which(colnames(games_owned_bins)=="cat_Abstract.Strategy") # just cause it is.
lastcol = which(colnames(games_owned_bins)=="cat_Zombies") # just cause it is.
owned_mech_top_20 = colnames(games_owned_bins[c(firstcol:lastcol)])[rev(sort.list(colSums(games_owned_bins[c(firstcol:lastcol)])))[1:20] ]
games_owned_mech_20 = games_owned_bins %>% 
  select(owned_bins,all_of(owned_mech_top_20),) %>%
  gather(top_mechs, per_, -c(owned_bins))


#########
games_owned_mech_20 %>%
  mutate(top_mechs = gsub('\\.',' ',gsub(".*_", "", top_mechs))) %>%
  mutate(owned_bins= as.numeric(owned_bins)) %>%
  ggplot(aes(x=owned_bins,y=per_)) + geom_line(aes(color = top_mechs))




######3
top_20_total =games_owned_mech_20 %>% 
  group_by(top_mechs) %>% 
  summarise(total = sum(per_)) 
games_owned_mech_20 = merge(games_owned_mech_20,top_20_total, by = 'top_mechs',
                            all.x = TRUE)

plot_order = unlist(games_owned_mech_20 %>% 
  mutate(owned_bins= as.numeric(owned_bins)) %>% filter(owned_bins==5) %>%
  mutate(per_ = per_/total) %>% arrange(per_) %>% select(top_mechs))

games_owned_mech_20$top_mechs <- factor(games_owned_mech_20$top_mechs, 
                                        levels = plot_order)

nice_bar_graph = games_owned_mech_20 %>%
  mutate(owned_bins= as.factor(as.numeric(owned_bins))) %>%
  ggplot(aes(fill=owned_bins, x=per_, y=top_mechs)) + 
  geom_bar(position="fill", stat="identity")+
  scale_shape_manual(values=c(1, 2, 3,4,5)) +
  scale_fill_manual(values = c('5'="#F8B195",
                               '4'="#F67280",
                               '3'="#C06C84",
                               '2'="#6C5B7B",
                               '1'="#355C7D"))

nice_bar_graph + theme(axis.title.y=element_blank(),
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
                       axis.text.y = element_text(margin = margin(r = -30))
)


games_owned_mech_20 %>%
  mutate(owned_bins = case_when(owned_bins=='[2,126]' ~ '2-126',
                                owned_bins=='(126,234]' ~ '127-234',
                                owned_bins=='(234,472]' ~ '235-472',
                                owned_bins=='(472,1.26e+03]' ~ '472-1262',
                                owned_bins=='(1.26e+03,1.45e+05]' ~ '1263-144727'
  )) %>%
  mutate(owned_bins=factor(owned_bins, levels=c('2-126', '127-234', '235-472','472-1262','1263-144727'))) %>%
  ggplot(aes(fill=owned_bins, x=per_, y=top_mechs)) + 
  geom_bar(position="fill", stat="identity") +
  scale_shape_manual(values=c('2-126', '127-234', '235-472','472-1262','1263-144727')) +
  scale_fill_manual(values = c('2-126'="#F8B195", #F8B195   F67280   C06C84   6C5B7B   355C7D 
                               '127-234'="#F67280",
                               '235-472'="#C06C84",
                               '472-1262'="#6C5B7B",
                               '1263-144727'="#355C7D"),
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
        axis.text.y = element_text(margin = margin(r = -30))
  )

games_owned_mech_20 %>%
  mutate(owned_bins = case_when(owned_bins=='[2,126]' ~ '2-126',
                                owned_bins=='(126,234]' ~ '127-234',
                                owned_bins=='(234,472]' ~ '235-472',
                                owned_bins=='(472,1.26e+03]' ~ '472-1262',
                                owned_bins=='(1.26e+03,1.45e+05]' ~ '1263-144727'
  )) %>%
  mutate(owned_bins=factor(owned_bins, levels=c('2-126', '127-234', '235-472','472-1262','1263-144727'))) %>%
  ggplot(aes(fill=owned_bins, x=per_, y=top_mechs)) + 
  geom_bar(position="fill", stat="identity") +
  scale_shape_manual(values=c('2-126', '127-234', '235-472','472-1262','1263-144727')) +
  scale_fill_manual(values = c('2-126'="#83AF9B", # FE4365   FC9D9A   F9CDAD   C8C8A9   83AF9B 
                               '127-234'="#C8C8A9",
                               '235-472'="#F9CDAD",
                               '472-1262'="#FC9D9A",
                               '1263-144727'="#FE4365"),
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
        axis.text.y = element_text(margin = margin(r = -30))
  )



# ########## colors that julie prefers
# 
# games_owned_mech_20 %>%
#   mutate(owned_bins = case_when(owned_bins=='[2,126]' ~ '2-126',
#                                 owned_bins=='(126,234]' ~ '127-234',
#                                 owned_bins=='(234,472]' ~ '235-472',
#                                 owned_bins=='(472,1.26e+03]' ~ '472-1262',
#                                 owned_bins=='(1.26e+03,1.45e+05]' ~ '1263-144727'
#   )) %>%
#   mutate(owned_bins=factor(owned_bins, levels=c('2-126', '127-234', '235-472','472-1262','1263-144727'))) %>%
#   ggplot(aes(fill=owned_bins, x=per_, y=top_mechs)) + 
#   geom_bar(position="fill", stat="identity") +
#   scale_shape_manual(values=c('2-126', '127-234', '235-472','472-1262','1263-144727')) +
#   scale_fill_manual(values = c('2-126'="#f9caa7",
#                                '127-234'="#fad9c1",
#                                '235-472'="#fec8c1",
#                                '472-1262'="#feb2a8",
#                                '1263-144727'="#fe9c8f"))+ 
#   theme(axis.title.y=element_blank(),
#         # axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         plot.background = element_rect(fill = '#F0F0F0'),
#         panel.background = element_rect(fill = '#F0F0F0'),
#         panel.grid.major = element_line(colour = "#CDCDCD"),
#         panel.grid.minor = element_line(colour = "#CDCDCD"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.position="top",
#         legend.background = element_rect(fill="#F0F0F0", 
#                                          size=0.5, linetype="solid"),
#         axis.text.y = element_text(margin = margin(r = -30))
#   )
# #fe9c8f • #feb2a8 • #fec8c1 • #fad9c1 • #f9caa7


test1 = games_owned_mech_20 %>% 
  group_by(top_mechs) %>%
  summarise(total = sum(per_)) %>%
  select(owned_bins,total) %>%
  filter(owned_bins=='(1.26e+03,1.45e+05]') %>%
  transmute(sort_order = per_/total)

# interesting to look at given mechs/cats but it will be hard to 
# look at that. will need a better way to look at top cats.
# rankings
test = games %>% 
  group_by(year_published) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

# complex split up is good. lets you see that moer complex games are more
# popular. and they have less players.
games = games%>%
  mutate(complex_bins = cut(complex, 
                          breaks = unique(quantile(complex,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                          include.lowest=TRUE))
test = games %>% 
  group_by(complex_bins) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

# could be interesting but i'm not super sold on it
games = games%>%
  mutate(max_players_bins = cut(max_players, breaks = c(0, 3.5, 4.5, 5.5, 6.5,22),
                                labels = c("1-3", "4", "5", "6",'7+'),
                                include.lowest = TRUE))
test = games %>% 
  group_by(max_players_bins) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

# decently interesting that 60+ game time correlates with more popular/complex game.
# i'd be interested to see which mechs are also the most successful for whatever.
games = games%>%
  mutate(play_time_bins = cut(play_time, 
                            breaks = unique(quantile(play_time,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                            include.lowest=TRUE))
test = games %>% 
  group_by(play_time_bins) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)
#card games are just kinda everywhere. But fantasy games are huge!
test = games %>% 
  group_by(Card.Game) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

test = games %>% 
  group_by(Fantasy) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

test = games %>% 
  group_by(Wargame) %>%
  # summarise(cnt=n())
  summarise_if(is.numeric, list(mean,sum), na.rm = TRUE)

# test = games %>% 
#   select(starts_with("mech_")) %>%
#   gather(key, value, starts_with("mech_")) %>% 
#   group_by(key) %>%
#   summarise(sum(value))
# 
#   filter(owned_bins=='(1.26e+03,1.45e+05]') %>%
#   gather(starts_with("mech_"),sum)

# this will get the column names for the columns that have the highest sum in each column
# you first need to have the list of columns that you are considering
# i started with the first/ last columns that are mechs.
firstcol = which(colnames(games)=="mech_Acting") # just cause it is.
lastcol = which(colnames(games)=="mech_Zone.of.Control") # just cause it is.
test = colnames(games[c(firstcol:lastcol)])[rev(sort.list(colSums(games[c(firstcol:lastcol)])))[1:8] ]

games[, colSums(.SD), .SDcols=patterns("mech_")]
# considering limiting games to the total percentage of the gaming market
# # this is how many games are still prevalent.
# games = games %>% 
#   arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
#   filter(csum_per>.01) # 15865
# 
# games = games %>% 
#   # select(id,name,num_rating,owned) %>%
#   arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
#   filter(csum_per>.1) # 6806

# games = games %>% 
#   # select(id,name,num_rating,owned) %>%
#   arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
#   filter(csum_per>.25) # 2658
# 
# games = games %>% 
#   # select(id,name,num_rating,owned) %>%
#   arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
#   filter(csum_per>.5) # 700






###################
# just looking at some graphs.
# the big new take away is that there is a problem in having such a fat tail 
# of there being SOOOO many games that have basically NO reviews.

#####
# also owned is so related to num_ratings that i may use that.

# option 1 with out limiting.
games_graph = ggplot(games,aes(y=num_rating))

games_graph = ggplot(games,aes(y=owned))

# option 2 with limiting.
games_graph = games %>% filter(num_rating>1000) %>% ggplot(aes(y=num_rating))
games %>% filter(num_rating>10000) %>% ggplot() +geom_density(aes(x=num_rating))

games %>% 
  filter(owned>10000) %>% 
  ggplot() +geom_density(aes(x=owned))


games_graph + geom_point(aes(year_published))

games_graph + geom_point(aes(avg_rating))

games_graph + geom_point(aes(bayes_rating))

games_graph + geom_point(aes(owned))

games_graph + geom_point(aes(g_rank))

games_graph + geom_point(aes(trading))
games_graph + geom_point(aes(wanting))
games_graph + geom_point(aes(wishing))
games_graph + geom_point(aes(num_comment))
games_graph + geom_point(aes(num_complex))
games_graph + geom_point(aes(complex))
games_graph + geom_density(aes(complex))
ggplot(games) + geom_density(aes(x=complex))
games_graph + geom_point(aes(min_players))
ggplot(games) + geom_density(aes(x=min_players))

games %>% filter(num_rating>1000) %>% 
  ggplot() + geom_col(aes(x = min_players,
                          y = num_rating))

games_graph + geom_point(aes(max_players))
ggplot(games) + geom_density(aes(x=max_players))
games %>% filter(max_players<20)%>% ggplot() + geom_density(aes(x=max_players))


games_graph + geom_point(aes(play_time))
games %>% filter(play_time<500)%>% ggplot() + geom_density(aes(x=play_time))

games_graph + geom_point(aes(min_play_time))
games_graph + geom_point(aes(max_play_time))
games_graph + geom_point(aes(min_age))
games %>% 
  # filter(play_time<500)%>% 
  ggplot() + geom_density(aes(x=min_age))

games %>% filter(num_rating>1000) %>% 
  ggplot() + geom_col(aes(x = min_age,
                          y = num_rating))

games_graph + geom_point(aes(max_players))
games %>% 
  filter(max_players<25)%>%
  ggplot() + geom_density(aes(x=max_players))

games %>%
  filter(play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(play_time))

games %>% 
  filter(play_time<500)%>%
  ggplot() + geom_density(aes(x=play_time))
games %>% 
  filter(play_time<500)%>%
  ggplot() + stat_density_2d(aes(x=play_time,y=owned,fill=..density..))

games %>% 
  filter(play_time<500)%>%ggplot(aes(x=play_time,y=owned)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level..),
                  bins = 7) 

games %>%
  filter(min_play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(min_play_time))


games %>%
  filter(max_play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(max_play_time))

games_graph + geom_point(aes(min_age))

ggplot(games,aes(x=num_rating)) + geom_boxplot()

games_graph + geom_point(aes(category_sum))

games %>% 
  filter(play_time<500)%>%ggplot(aes(x=category_sum,y=owned)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level..),
                  bins = 7) 
games_graph + geom_point(aes(mechanics_sum))

games$mechanics_fsum <- factor(games$mechanics_sum)
games %>%
  group_by(mechanics_sum) %>%
  summarise(cnt = n(),mean(owned),median(owned))
games %>% 
  filter(csum_per>.5) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=mechanics_fsum))
games %>% 
  filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=mechanics_fsum))
games %>% 
  # filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=mechanics_fsum))
games %>% 
  filter(play_time<500)%>%ggplot(aes(x=mechanics_sum,y=owned)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level..),
                  bins = 7) 


games$category_fsum <- factor(games$category_sum)
games %>%
  group_by(category_sum) %>%
  summarise(cnt = n(),mean(owned),median(owned))
games %>% 
  filter(csum_per>.5) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=category_fsum))
games %>% 
  filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=category_fsum))
games %>% 
  # filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=category_fsum))
games %>% 
  filter(play_time<500)%>%ggplot(aes(x=category_sum,y=owned)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level..),
                  bins = 7) 

games$f_hand_management <- factor(games$'Hand Management')
games %>% 
  filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_density(aes(fill=f_hand_management))

games$fEconomic <- factor(games$'Economic')
games %>% 
  filter(csum_per>.75) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=fEconomic))

games$fcard_game <- factor(games$'Card Game')
games %>% 
  filter(csum_per>.7) %>%
  ggplot(aes(x=owned)) +
  geom_boxplot(aes(fill=fcard_game))
#########
games %>% filter(id == diplo_id) %>% ggplot() +
  geom_point(aes(x=owned,y=num_rating),color='red',
             size=10) +
  geom_point(data = games,aes(x=owned,y=num_rating))

sum(is.na(games$num_rating))

top = games %>% select(name,owned) %>% arrange(desc(owned))

top$Diff <- top$owned - dplyr::lag(top$owned, n = 1)

ggplot(top) +geom_point(aes(x=owned,y=Diff))+
  geom_point(aes(x=29591,y=-908),color='red')
# possible cut off point.

### also would remove the top two from some of the things as different.
# 102
# Roll for the Galaxy
# 30499
# -52
# 103
# Zombie Dice
# 29591
# -908

years = games %>% 
  group_by(year_published) %>%
  summarise(cnt=n(),
            mean(owned),median(owned),
            mean(complex),median(complex))

years_cat = games %>% 
  rename(card_game= 'Card Game',
         party='Party Game',
         sf='Science Fiction',
         child = 'Childrens Game',
         abstract = 'Abstract Strategy') %>%
  group_by(year_published) %>%
  summarise(cnt=n(),
            card_game_total = sum(card_game),
            wargame_total = sum(Wargame),
            fantasy_total = sum(Fantasy),
            party_total = sum(party),
            dice_total = sum(Dice),
            fight_total = sum(Fighting),
            sf_total = sum(sf),
            child_total = sum(child),
            abstract_total = sum(abstract),
            econ_total = sum(Economic)
  ) %>%
  transmute(year_published,
            card_per = card_game_total/cnt,
            war_per = wargame_total/cnt,
            fantasy_per = fantasy_total/cnt,
            party_per = party_total/cnt,
            dice_per = dice_total/cnt,
            fight_per = fight_total/cnt,
            sf_per = sf_total/cnt,
            child_per = child_total/cnt,
            abstract_per= abstract_total/cnt,
            econ_per = econ_total/cnt) %>%
  gather(top_cats, per_, -c(year_published))

years_cat %>%
  ggplot(aes(x=year_published,y=per_)) + geom_line(aes(color = top_cats))




years_mech = games %>% 
  rename(dice= 'Dice Rolling',
         hand_manage='Hand Management',
         set_collection='Set Collection',
         vari_powers = 'Variable Player Powers',
         hex_grid='Hexagon Grid',
         simulation='Simulation',
         card_draft = 'Card Drafting',
         tile_place='Tile Placement',
         modular_board = 'Modular Board',
         area_influence = 'Area Majority / Influence') %>%
  group_by(year_published) %>%
  summarise(cnt=n(),
            dice_total = sum(dice),
            hand_manage_total = sum(hand_manage),
            set_collection_total = sum(set_collection),
            vari_powers_total = sum(vari_powers),
            hex_grid_total = sum(hex_grid),
            simulation_total = sum(simulation),
            card_draft_total = sum(card_draft),
            tile_place_total = sum(tile_place),
            modular_board_total = sum(modular_board),
            area_influence_total = sum(area_influence)
  ) %>%
  transmute(year_published,
            dice_per = dice_total/cnt,
            hand_manage_per = hand_manage_total/cnt,
            set_collection_per = set_collection_total/cnt,
            vari_powers_per = vari_powers_total/cnt,
            hex_grid_per = hex_grid_total/cnt,
            simulation_per = simulation_total/cnt,
            card_draft_per = card_draft_total/cnt,
            tile_place_per = tile_place_total/cnt,
            modular_board_per= modular_board_total/cnt,
            area_influence_per = area_influence_total/cnt) %>%
  gather(top_mechs, per_, -c(year_published))

years_mech %>%
  ggplot(aes(x=year_published,y=per_)) + geom_line(aes(color = top_mechs))

games %>% ggplot() +geom_(aes(x = expansion,y=name))



games %>% group_by(expansion) %>% 
  summarise(cnt=n(),mean(owned),median(owned))

games %>% group_by(integration) %>% 
  summarise(cnt=n(),mean(owned),median(owned))

games %>% group_by(compilation) %>% 
  summarise(cnt=n(),mean(owned),median(owned))

games %>% group_by(expansion) %>% 
  summarise(cnt=n(),mean(bayes_rating),median(bayes_rating))

games %>% group_by(integration) %>% 
  summarise(cnt=n(),mean(bayes_rating),median(bayes_rating))

games %>% group_by(compilation) %>% 
  summarise(cnt=n(),mean(bayes_rating),median(bayes_rating))

compilation_mech = games %>% 
  rename(dice= 'Dice Rolling',
         hand_manage='Hand Management',
         set_collection='Set Collection',
         vari_powers = 'Variable Player Powers',
         hex_grid='Hexagon Grid',
         simulation='Simulation',
         card_draft = 'Card Drafting',
         tile_place='Tile Placement',
         modular_board = 'Modular Board',
         area_influence = 'Area Majority / Influence') %>%
  group_by(compilation) %>%
  summarise(cnt=n(),
            dice_total = sum(dice),
            hand_manage_total = sum(hand_manage),
            set_collection_total = sum(set_collection),
            vari_powers_total = sum(vari_powers),
            hex_grid_total = sum(hex_grid),
            simulation_total = sum(simulation),
            card_draft_total = sum(card_draft),
            tile_place_total = sum(tile_place),
            modular_board_total = sum(modular_board),
            area_influence_total = sum(area_influence)
  ) %>%
  transmute(compilation,
            dice_per = dice_total/cnt,
            hand_manage_per = hand_manage_total/cnt,
            set_collection_per = set_collection_total/cnt,
            vari_powers_per = vari_powers_total/cnt,
            hex_grid_per = hex_grid_total/cnt,
            simulation_per = simulation_total/cnt,
            card_draft_per = card_draft_total/cnt,
            tile_place_per = tile_place_total/cnt,
            modular_board_per= modular_board_total/cnt,
            area_influence_per = area_influence_total/cnt) 


integration_mech = games %>% 
  rename(dice= 'Dice Rolling',
         hand_manage='Hand Management',
         set_collection='Set Collection',
         vari_powers = 'Variable Player Powers',
         hex_grid='Hexagon Grid',
         simulation='Simulation',
         card_draft = 'Card Drafting',
         tile_place='Tile Placement',
         modular_board = 'Modular Board',
         area_influence = 'Area Majority / Influence') %>%
  group_by(integration) %>%
  summarise(cnt=n(),
            dice_total = sum(dice),
            hand_manage_total = sum(hand_manage),
            set_collection_total = sum(set_collection),
            vari_powers_total = sum(vari_powers),
            hex_grid_total = sum(hex_grid),
            simulation_total = sum(simulation),
            card_draft_total = sum(card_draft),
            tile_place_total = sum(tile_place),
            modular_board_total = sum(modular_board),
            area_influence_total = sum(area_influence)
  ) %>%
  transmute(integration,
            dice_per = dice_total/cnt,
            hand_manage_per = hand_manage_total/cnt,
            set_collection_per = set_collection_total/cnt,
            vari_powers_per = vari_powers_total/cnt,
            hex_grid_per = hex_grid_total/cnt,
            simulation_per = simulation_total/cnt,
            card_draft_per = card_draft_total/cnt,
            tile_place_per = tile_place_total/cnt,
            modular_board_per= modular_board_total/cnt,
            area_influence_per = area_influence_total/cnt)


expansion_mech = games %>% 
  rename(dice= 'Dice Rolling',
         hand_manage='Hand Management',
         set_collection='Set Collection',
         vari_powers = 'Variable Player Powers',
         hex_grid='Hexagon Grid',
         simulation='Simulation',
         card_draft = 'Card Drafting',
         tile_place='Tile Placement',
         modular_board = 'Modular Board',
         area_influence = 'Area Majority / Influence') %>%
  group_by(expansion) %>%
  summarise(cnt=n(),
            dice_total = sum(dice),
            hand_manage_total = sum(hand_manage),
            set_collection_total = sum(set_collection),
            vari_powers_total = sum(vari_powers),
            hex_grid_total = sum(hex_grid),
            simulation_total = sum(simulation),
            card_draft_total = sum(card_draft),
            tile_place_total = sum(tile_place),
            modular_board_total = sum(modular_board),
            area_influence_total = sum(area_influence)
  ) %>%
  transmute(expansion,
            dice_per = dice_total/cnt,
            hand_manage_per = hand_manage_total/cnt,
            set_collection_per = set_collection_total/cnt,
            vari_powers_per = vari_powers_total/cnt,
            hex_grid_per = hex_grid_total/cnt,
            simulation_per = simulation_total/cnt,
            card_draft_per = card_draft_total/cnt,
            tile_place_per = tile_place_total/cnt,
            modular_board_per= modular_board_total/cnt,
            area_influence_per = area_influence_total/cnt)


compilation_cat = games %>% 
  rename(card_game= 'Card Game',
         party='Party Game',
         sf='Science Fiction',
         child = 'Childrens Game',
         abstract = 'Abstract Strategy') %>%
  group_by(compilation) %>%
  summarise(cnt=n(),
            card_game_total = sum(card_game),
            wargame_total = sum(Wargame),
            fantasy_total = sum(Fantasy),
            party_total = sum(party),
            dice_total = sum(Dice),
            fight_total = sum(Fighting),
            sf_total = sum(sf),
            child_total = sum(child),
            abstract_total = sum(abstract),
            econ_total = sum(Economic)
  ) %>%
  transmute(compilation,
            card_per = card_game_total/cnt,
            war_per = wargame_total/cnt,
            fantasy_per = fantasy_total/cnt,
            party_per = party_total/cnt,
            dice_per = dice_total/cnt,
            fight_per = fight_total/cnt,
            sf_per = sf_total/cnt,
            child_per = child_total/cnt,
            abstract_per= abstract_total/cnt,
            econ_per = econ_total/cnt)

integration_cat = games %>% 
  rename(card_game= 'Card Game',
         party='Party Game',
         sf='Science Fiction',
         child = 'Childrens Game',
         abstract = 'Abstract Strategy') %>%
  group_by(integration) %>%
  summarise(cnt=n(),
            card_game_total = sum(card_game),
            wargame_total = sum(Wargame),
            fantasy_total = sum(Fantasy),
            party_total = sum(party),
            dice_total = sum(Dice),
            fight_total = sum(Fighting),
            sf_total = sum(sf),
            child_total = sum(child),
            abstract_total = sum(abstract),
            econ_total = sum(Economic)
  ) %>%
  transmute(integration,
            card_per = card_game_total/cnt,
            war_per = wargame_total/cnt,
            fantasy_per = fantasy_total/cnt,
            party_per = party_total/cnt,
            dice_per = dice_total/cnt,
            fight_per = fight_total/cnt,
            sf_per = sf_total/cnt,
            child_per = child_total/cnt,
            abstract_per= abstract_total/cnt,
            econ_per = econ_total/cnt)

expansion_cat = games %>% 
  rename(card_game= 'Card Game',
         party='Party Game',
         sf='Science Fiction',
         child = 'Childrens Game',
         abstract = 'Abstract Strategy') %>%
  group_by(expansion) %>%
  summarise(cnt=n(),
            card_game_total = sum(card_game),
            wargame_total = sum(Wargame),
            fantasy_total = sum(Fantasy),
            party_total = sum(party),
            dice_total = sum(Dice),
            fight_total = sum(Fighting),
            sf_total = sum(sf),
            child_total = sum(child),
            abstract_total = sum(abstract),
            econ_total = sum(Economic)
  ) %>%
  transmute(expansion,
            card_per = card_game_total/cnt,
            war_per = wargame_total/cnt,
            fantasy_per = fantasy_total/cnt,
            party_per = party_total/cnt,
            dice_per = dice_total/cnt,
            fight_per = fight_total/cnt,
            sf_per = sf_total/cnt,
            child_per = child_total/cnt,
            abstract_per= abstract_total/cnt,
            econ_per = econ_total/cnt)















library(corrplot)
df_cor <- cor(game_mech)
df_cor[lower.tri(df_cor)] <- NA 
#drop perfect correlations
df_cor[df_cor == 1] <- NA 
df_cor <- as.data.frame(as.table(df_cor))
df_cor <- na.omit(df_cor) 
df_cor <- subset(df_cor, abs(Freq) > .2) 
df_cor <- df_cor[order(-abs(df_cor$Freq)),] 

corrplot(df_cor)



library(reshape2)

corr_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data = game_mech)
