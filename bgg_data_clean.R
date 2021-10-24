library(tidyverse)

# load in data. first column is index so it is skipped.
games_dirty = read.csv('games_detailed_info.csv',
                 colClasses=c("NULL", 
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA
                 ))
# rename and shrink columns
games_dirty = games_dirty %>% 
  select(id, 
         name = primary, 
         num_rating = usersrated,
         year_published = yearpublished, 
         category = boardgamecategory,
         mechanic = boardgamemechanic,
         avg_rating = average, owned, 
         complex = averageweight,
         min_players = minplayers, 
         max_players = maxplayers, play_time = playingtime, 
         min_age = minage, 
         expansion = boardgameexpansion, 
         image,
         publisher = boardgamepublisher
         ) 
# dummifying the mechanic column into a binary 1/0 if a game has each mechanic.
# replace double quote for single quote because "Prisoner's Dile
# makes df of all the games with their mechanics
mech_dirty = games_dirty %>% 
  select(id, mechanic) %>%
  separate(mechanic, paste0('mechanic', c(1:18)), sep = "', '", remove = T)
# cleans the strings in df
mech_dirty[,startsWith(names(mech_dirty),"mechanic")] = as.data.frame(
  lapply(mech_dirty[,startsWith(names(mech_dirty),"mechanic")], 
         function(y) gsub("\\'|\\[|\\]", '', y)))
# makes a count of each mechanic
all_mechs = unlist(mech_dirty[,c(2:18)])
all_mechs = data.frame(table(all_mechs))
all_mechs = all_mechs[-c(1), ]
# Create empty data frame
game_mech <- data.frame(matrix(NA,    
                          nrow = nrow(games_dirty),
                          ncol = nrow(all_mechs)+1))
# put game id's into matrix of mechanics
game_mech$X1 = games_dirty$id
colnames(game_mech)[1] = "id"
# puts a 1 if game has that mechanic. and 0 if it does not. for each mechanic.
for (i in 2:ncol(game_mech)){
  game_mech[i] = rowSums(mech_dirty[ ,startsWith(names(mech_dirty),
                                 "mechanic")]==as.character(all_mechs[i-1,1]),
                         na.rm = TRUE)
  colnames(game_mech)[i] = paste0('mech_',as.character(all_mechs[i-1,1]))
}
# sum of how many mechanics a given game has.
game_mech$mech_Global.Mechanics.Average = rowSums(game_mech[,-1])
# merges mechanics back into main games_dirty
games_dirty = merge(x = games_dirty, y = game_mech, by ='id')
### Same thing now for category
# replace double quote for single quote
games_dirty$category <- gsub('"', "'", games_dirty$category)
# makes df of all the games with their categorys
cat_dirty = games_dirty %>% 
  select(id, category) %>%
  separate(category, paste0('category', c(1:18)), sep = "', '", remove = T)
# cleans the strings in df
cat_dirty[,startsWith(names(cat_dirty),"category")] = as.data.frame(
  lapply(cat_dirty[,startsWith(names(cat_dirty),"category")], 
         function(y) gsub("\\'|\\[|\\]", '', y)))
# makes a count of each category
all_cats = unlist(cat_dirty[,c(2:19)])
all_cats = data.frame(table(all_cats))
all_cats = all_cats[-c(1), ]
# Create empty data frame
games_cat <- data.frame(matrix(NA,    
                               nrow = nrow(games_dirty),
                               ncol = nrow(all_cats)+1))
# put game id's into matrix of category
games_cat$X1 = games_dirty$id
colnames(games_cat)[1] = "id"
# puts a 1 if game has that category and 0 if it does not. for each category
for (i in 2:ncol(games_cat)){
  games_cat[i] = rowSums(
    cat_dirty[ ,startsWith(names(cat_dirty),
                           "category")]==as.character(all_cats[i-1,1]),
                         na.rm = TRUE)
  colnames(games_cat)[i] = paste0('cat_',as.character(all_cats[i-1,1]))
}
# sum of how many mechanics a given game has.
games_cat$cat_Global.Categorys.Average = rowSums(games_cat[,-1])
# merges mechanics back into main games_dirty
games_dirty = merge(x = games_dirty, y = games_cat, by ='id')

### cleaning the data
games_dirty = games_dirty %>%
  # remove games with no category or mechanic values.
  filter(
    cat_Global.Categorys.Average!=0 | mech_Global.Mechanics.Average!=0
    ) %>% 
  # this data was collected in august 2020. doesn't make sense
  filter(year_published<2021) %>% 
  # 1995 is the first year that there are at least 200 games in the data set 
  # and Catan was made in that year. 
  # It is the best of bad options to cut off at that time.
  filter(year_published>=1995) %>% 
  # removing public domain games
  filter(!str_detect(publisher, 'Public Domain')) 
# removing outliers
games_dirty$max_players <- ifelse(
  games_dirty$max_players>21 , 21, games_dirty$max_players
  )
games_dirty$play_time <- ifelse(
  games_dirty$play_time>360 , 361, games_dirty$play_time
  )

# save file
write.csv(games_dirty, file = 'games_cleaned.csv', row.names = FALSE)