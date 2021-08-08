
library(tidyverse)

## columns to drop:
# type, thumbnail, alternate, median, boardgamecompilation, min_playtime,
# max_playtime

## columns to consider dropping:
# image, description, syggested_language_dependenc..., family
# Strategy.Game.Rank , Family.Game.Rank, XXXXXXX.Game.Rank

##columns to clearn
# suggested_num_players,
# suggested_playerage,
# category,  ## check
# mechanic, ### check!!!!
# family,

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
         avg_rating = average, bayes_rating = bayesaverage,
         g_rank = Board.Game.Rank, std_rank = stddev, owned, trading, wanting, 
         wishing, num_comment = numcomments, num_complex = numweights,
         complex = averageweight,
         min_players = minplayers, 
         max_players = maxplayers, play_time = playingtime, 
         min_age = minage, family = boardgamefamily,
         expansion = boardgameexpansion, 
         implementation = boardgameimplementation,
         integration = boardgameintegration,
         compilation = boardgamecompilation,
         designer = boardgamedesigner, artist = boardgameartist,
         publisher = boardgamepublisher
         ) 
### dummifying the mechanic column into a binary 1/0 if a game has each mechanic.
# replace double quote for single quote because "Prisoner's Dilemma" ruins the split
games_dirty$mechanic <- gsub('"', "'", games_dirty$mechanic)
# makes df of all the games with their mechanics
mech_dirty = games_dirty %>% 
  select(id, mechanic) %>%
  separate(mechanic, paste0('mechanic', c(1:18)), sep = "', '", remove = T)
# cleans the strings in df
mech_dirty[,startsWith(names(mech_dirty),"mechanic")] = as.data.frame(lapply(mech_dirty[,startsWith(names(mech_dirty),"mechanic")], function(y) gsub("\\'|\\[|\\]", '', y)))
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
  game_mech[i] = rowSums(mech_dirty[ ,startsWith(names(mech_dirty),"mechanic")]==as.character(all_mechs[i-1,1]),
                         na.rm = TRUE)
  colnames(game_mech)[i] = paste0('mech_',as.character(all_mechs[i-1,1]))
}
# sum of how many mechanics a given game has.
game_mech$mechanics_sum = rowSums(game_mech[,-1])
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
cat_dirty[,startsWith(names(cat_dirty),"category")] = as.data.frame(lapply(cat_dirty[,startsWith(names(cat_dirty),"category")], function(y) gsub("\\'|\\[|\\]", '', y)))
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
  games_cat[i] = rowSums(cat_dirty[ ,startsWith(names(cat_dirty),"category")]==as.character(all_cats[i-1,1]),
                         na.rm = TRUE)
  colnames(games_cat)[i] = paste0('cat_',as.character(all_cats[i-1,1]))
}
# sum of how many mechanics a given game has.
games_cat$category_sum = rowSums(games_cat[,-1])
# merges mechanics back into main games_dirty
games_dirty = merge(x = games_dirty, y = games_cat, by ='id')

### cleaning the data
games_dirty = games_dirty %>%
  filter(category_sum!=0 | mechanics_sum!=0) %>% # remove games with no category or mechanic values.
  filter(year_published<2021) %>% # this data was collected in august 2020. doesn't make sense
  filter(year_published>=1995) %>% # 1995 is the first year that there are at least 200 games in the data set and Catan was made in that year. It is the best of bad options to cut off at that time.
  mutate(expansion = case_when(expansion=='' ~ 0, # making expansion a binary
                               expansion!='' ~ 1),
         integration = case_when(integration=='' ~ 0, # same for integration
                                 integration!='' ~ 1),
         compilation = case_when(compilation=='' ~ 0, # same for compilation
                                 compilation!='' ~ 1)
  )%>%
  filter(!str_detect(publisher, 'Public Domain')) # removing public domain games
# removing outliers that are insane
games_dirty$max_players <- ifelse(games_dirty$max_players>21 , 21, games_dirty$max_players)
games_dirty$play_time <- ifelse(games_dirty$play_time>360 , 361, games_dirty$play_time)

# 
# # https://www.r-bloggers.com/2019/07/clean-consistent-column-names/
# clean_names <- function(.data, unique = FALSE) {
#   n <- if (is.data.frame(.data)) colnames(.data) else .data
#   n <- gsub("%+", "_pct_", n)
#   n <- gsub("\\$+", "_dollars_", n)
#   n <- gsub("\\++", "_plus_", n)
#   n <- gsub("-+", "_minus_", n)
#   n <- gsub("\\*+", "_star_", n)
#   n <- gsub("#+", "_cnt_", n)
#   n <- gsub("&+", "_and_", n)
#   n <- gsub("@+", "_at_", n)
#   n <- gsub("[^a-zA-Z0-9_]+", "_", n)
#   n <- gsub("([A-Z][a-z])", "_\\1", n)
#   n <- tolower(trimws(n))
#   
#   n <- gsub("(^_+|_+$)", "", n)
#   
#   n <- gsub("_+", "_", n)
#   
#   if (unique) n <- make.unique(n, sep = "_")
#   
#   if (is.data.frame(.data)) {
#     colnames(.data) <- n
#     .data
#   } else {
#     n
#   }
# }



write.csv(games_dirty, file = 'games_cleaned.csv', row.names = FALSE)

