
library(tidyverse)

# look at the top publishers to look at different publishers.
# 


## columns to drop:
# type, thumbnail, alternate, median, boardgameintegration, boardgamecompilation

## columns to consider dropping:
# image, description, syggested_language_dependenc..., family
# Strategy.Game.Rank , Family.Game.Rank, XXXXXXX.Game.Rank

##columns to clearn
# suggested_num_players,
# suggested_playerage,
# category,
# mechanic, ### check!!!!
# family,
# implementation

# load in data. first column is index so it is skipped.
games = read.csv('games_detailed_info.csv',
                 colClasses=c("NULL", 
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA
                 ))
# rename and shrink columns
games = games %>% 
  select(id, 
         name = primary, 
         num_rating = usersrated,
         year_published = yearpublished, 
         category = boardgamecategory,
         mechanic = boardgamemechanic,
         avg_rating = average, bayes_rating = bayesaverage,
         g_rank = Board.Game.Rank, std_rank = stddev, owned, trading, wanting, 
         wishing, num_comment = numcomments, num_complex = numweights,
         complex = averageweight,
         suggested_num_players, min_players = minplayers, 
         max_players = maxplayers, play_time = playingtime, 
         min_play_time = minplaytime, max_play_time = maxplaytime, 
         suggested_playerage,
         min_age = minage, family = boardgamefamily,
         expansion = boardgameexpansion, 
         implementation = boardgameimplementation,
         designer = boardgamedesigner, artist = boardgameartist,
         publisher = boardgamepublisher
         ) 

# replace double quote for single quote because "Prisoner's Dilemma" ruins the split
games$mechanic <- gsub('"', "'", games$mechanic)

#
mech_dirty = games %>% 
  select(id, mechanic) %>%
  separate(mechanic, paste0('mechanic', c(1:18)), sep = "', '", remove = T)

mech_dirty[,startsWith(names(mech_dirty),"mechanic")] = as.data.frame(lapply(mech_dirty[,startsWith(names(mech_dirty),"mechanic")], function(y) gsub("\\'|\\[|\\]", '', y)))

# si this the solution. I can have a list (or whatever R calls things) of all
# the mechanics and then do a string detect for those words. The only problem
# will be if there mechanics that can fit within another mechanic.

all_mechs = unlist(mech_dirty[,c(2:19)])
all_mechs = data.frame(table(all_mechs))
all_mechs = all_mechs[-c(1), ]

#################################################################
# i'm not sure that i need to set it up this way. I think that i can now just
# change things within the mech_dirty dataframe to have them work out
###############################################################
# games$Dice.Rolling = rowSums(games[,startsWith(names(games),"mechanic")]=="Dice Rolling",na.rm = TRUE)

game_mech <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = nrow(games),
                          ncol = nrow(all_mechs)+1))

game_mech$X1 = games$id
colnames(game_mech)[1] = "id"
for (i in 2:ncol(game_mech)){
  game_mech[i] = rowSums(mech_dirty[ ,startsWith(names(mech_dirty),"mechanic")]==as.character(all_mechs[i-1,1]),
                         na.rm = TRUE)
  colnames(game_mech)[i] = as.character(all_mechs[i-1,1])
  }

row_sum = data.frame(thing = rowSums(game_mech[,-1]))
ggplot(data = row_sum) +geom_density(aes(x=thing))

sum_test = data.frame(thing = colSums(game_mech[,-1]))
sum_test <- cbind(all_mechs = rownames(sum_test), sum_test)
rownames(sum_test) <- 1:nrow(sum_test)

mechs_test = merge(x = all_mechs_count, y = sum_test, by = "all_mechs", all = TRUE)
mechs_test = mechs_test %>% mutate(sum_diff = Freq - thing, mech_per = Freq/19230)


############################################
# looking at the stuff now for category
games$category <- gsub('"', "'", games$category)

cat_dirty = games %>% 
  select(id, category) %>%
  separate(category, paste0('category', c(1:18)), sep = "', '", remove = T)

cat_dirty[,startsWith(names(cat_dirty),"category")] = as.data.frame(lapply(cat_dirty[,startsWith(names(cat_dirty),"category")], function(y) gsub("\\'|\\[|\\]", '', y)))


all_cats = unlist(cat_dirty[,c(2:19)])
all_cats = data.frame(table(all_cats))
all_cats = all_cats[-c(1), ]

games_cat <- data.frame(matrix(NA,    # Create empty data frame
                               nrow = nrow(games),
                               ncol = nrow(all_cats)+1))

games_cat$X1 = games$id
colnames(games_cat)[1] = "id"
for (i in 2:ncol(games_cat)){
  games_cat[i] = rowSums(cat_dirty[ ,startsWith(names(cat_dirty),"category")]==as.character(all_cats[i-1,1]),
                         na.rm = TRUE)
  colnames(games_cat)[i] = as.character(all_cats[i-1,1])
}


row_sum = data.frame(thing = rowSums(games_cat[,-1]))
ggplot(data = row_sum) +geom_density(aes(x=thing))

sum_test = data.frame(thing = colSums(games_cat[,-1]))
sum_test <- cbind(all_cats = rownames(sum_test), sum_test)
rownames(sum_test) <- 1:nrow(sum_test)

cats_test = merge(x = all_cats, y = sum_test, by = "all_cats", all = TRUE)
cats_test = cats_test %>% mutate(sum_diff = Freq - thing, mech_per = Freq/19230)



# considering limiting games to the total percentage of the gaming market
# this is how many games are still prevalent.
games_cum = games %>% 
  arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
  filter(csum_per>.01) # 15865

games_cum = games %>% select(id,name,num_rating,owned) %>%
  arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
  filter(csum_per>.1) # 6806

games_cum = games %>% select(id,name,num_rating,owned) %>%
  arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
  filter(csum_per>.25) # 2658

games_cum = games %>% select(id,name,num_rating,owned) %>%
  arrange(owned) %>% mutate(csum = cumsum(owned),csum_per = csum/26276972) %>% 
  filter(csum_per>.5) # 700



##### 


games = merge(x = games, y = game_mech, by ='id')
games_head = head(games)


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











# https://www.r-bloggers.com/2019/07/clean-consistent-column-names/
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}
cor




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
games_graph + geom_point(aes(min_players))

games %>% filter(num_rating>1000) %>% 
  ggplot() + geom_col(aes(x = min_players,
                              y = num_rating))

games_graph + geom_point(aes(max_players))

games_graph + geom_point(aes(play_time))
games_graph + geom_point(aes(min_play_time))
games_graph + geom_point(aes(max_play_time))
games_graph + geom_point(aes(min_age))
games %>% filter(num_rating>1000) %>% 
  ggplot() + geom_col(aes(x = min_age,
                          y = num_rating))

games_graph + geom_point(aes(max_players))


games %>%
  filter(play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(play_time))

games %>%
  filter(min_play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(min_play_time))

games %>%
  filter(max_play_time<500) %>% ggplot(aes(y=num_rating)) +
  geom_point(aes(max_play_time))

games_graph + geom_point(aes(min_age))

ggplot(games,aes(x=num_rating)) + geom_boxplot()


#########

sum(is.na(games$num_rating))






