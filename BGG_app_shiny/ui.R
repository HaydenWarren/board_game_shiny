# shiny app style is all based on Victoria Lowery Bon Appetit shiny app
# https://github.com/vlowery/BonApp  
# https://vlowery.shinyapps.io/bon_appetit_history/

shinyUI(dashboardPage(
  dashboardHeader(title = "Board Game Geek"),
  dashboardSidebar(sidebarMenu(
    menuItem("Introductions", tabName = "intro", icon = icon("chess")),
    menuItem("Game Attributes", tabName = "violin", icon = icon("dollar-sign")),
    menuItem("Board Games Sorted", tabName = "stacked_bar", icon = icon("sort-amount-down")),
    menuItem("Game Mechanic Trends", tabName = "mechanic", icon = icon("dice")),
    menuItem("Game Category Trends", tabName = "category", icon = icon("hat-wizard"))
  )), 
  # icon list : chess , chess-board , cogs , database , dice , 
  # dice-d20 , dollar-sign , dungeon , file-csv , github , info , link , 
  # poll-h , sort-amount-down , hat-wizard , 
  # https://fontawesome.com/v5.15/icons?d=gallery&p=3&m=free
  dashboardBody(tabItems(
    tabItem(tabName = "intro", 
            fluidPage(
              fluidRow(column(offset = 2, width = 10, h1(tags$b("The Pieces that Make a Successful Board Game")))),
              br(),
                fluidRow(
                  column(offset = 2, width = 8, 
                         box(width = 12, h2(tags$b("Introduction")), background = "black",
                             p("I am an avid Board Game player. 
                             I love that I can be very analytical in coming up with a strategy to win a game, 
                             while also communicating and having a great time with friends. 
                             After my first experience with an indie Board Game I quickly became addicted to looking at BoardGameGeek.com, 
                             the IMDB for board games, and searching for the next great game to buy."),
                             p("I built this app looking at information provided on BoardGameGeek.com. 
                               I focused on what attributes popular games have and if they are reproducible. 
                               Popular games will be described as games that have a large number of BoardGameGeek users reporting to own the game. 
                               This is not a perfect metric as games like chess are not at the top of the list. 
                               I think that is an acceptable problem because if you are a Board Game maker you can't make the new Chess. 
                               But you can make an indie game like Catan that the users of BoardGameGeeks will love, buy, and report that they own it.")
                             ))
                ),
              fluidRow(
                box(width = 6, h2(tags$b("Expectations")), background = "olive", 
                    p("Initially, I thought that less complex and shorter games would be more successful. 
                      Their shorter play time and ease of accessibility would make them more likely to be so sold to the masses. 
                      Party Games would be a prime example of a style of game that can appeal to the masses. 
                      I also thought that German style board games, or Economic games, like Catan would be popular because Catan functions as a gateway board game for so many people.")), 
                box(width = 6, h2(tags$b("Reality")), background = "maroon", 
                    p("Surprisingly, however, only half of my expectations were true. According to the data scraped,", 
                      tags$b("more complex and longer games are more popular."), "I was correct that German style board games are very common and very popular.
                      But Economic games are not the only style of game that are very popular. 
                      I was half correct that there are a lot of Party Games designed but the market might be over saturated as not very many Party Games are successful."))
              )
            )
    ),
    ###################################### stacked bar tab
    tabItem(tabName = 'stacked_bar',
            fluidPage(
              fluidRow(
                column(offset = 2, width = 8,
                       box(width = 12, h1(tags$b("Board Games Sorted"), br()), align = "center", background = "maroon",
                           h4(tags$b("Board Game Mechanic's are the tools that you use and how you use them to play the game"),br(),
                              ("(e.g. rolling dice in Monopoly or the pattern recognition of Connect 4)"), br(), br(),
                              tags$b("Board Game Categories are the themes or the stories of the game."), br(),
                              ("(e.g. Economic concerns in Monopoly or Connect 4 being for Children)")
                              ))
              )),
              h2("Top 20 Game Mechanics"),
              fluidRow(
                tabBox(width = 8,
                       tabPanel("Mechanic Proportions", plotOutput("mech_bar_per")),
                       tabPanel("Mechanic Counts", plotOutput("mech_bar_total"))),
                box(width = 4, p("The Global Average for all mechanics shows that as 
                                 games get more popular they are more likely to have 
                                 more mechanics added to their Board Game Geeks page. Each game likely has many mechanics but not all of them will be reported unless there are fans of the game willing to put it onto the website. 
                                 So it is best to compare each mechanic to its peers."),
                    p("The mechanics that tend to have more popular games are Area Majority Influence, Card Drafting, and Variable Player Powers. 
                      The mechanics that tend to have less popular games are Simulation, Roll Spin and Move, and Hexagon Grid."),
                    p("The mechanic counts graph shows that Dice Rolling and Hand Mangagement games are the most common games. 
                      They are basic mechanics that any game could build on as a foundation for a game."))
              ),
              h2("Top 20 Game Categories"),
              fluidRow(
                box(width = 4, p("There are generally less categories that a game can fit into, so games that are more popular are not substantially more likely to have more categories added."),
                    p("Games that can have a story or narrative around the game tend to be more popular. This is shown by Economic, Medieval, Adventure, Fighting, and Fantasy being at the top of the list.
                      The less popular games of Children's Games and Action Dexterity are games that might have less replay value so people are less likely to buy them. 
                      Wargames have a unique distribution where they don't have many very popular or very unpopular games and instead have a majority of their games in the middle of our games owned range."),
                    p("The category counts graphs shows that Card Games are a very popular game. There are a surprisingly large number of Fantasy games with that being the third most popular category.
                                        ")),
                tabBox(width = 8,
                       tabPanel("Mechanic Proportions", plotOutput("cat_bar_per")),
                       tabPanel("Mechanic Counts", plotOutput("cat_bar_total")))
              )
            )
    ),
#### game mechanics
    tabItem(tabName = 'violin',
            fluidPage(
              fluidRow(
                column(offset = 1, width = 10,
                       box(width = 12, h1(tags$b("Successful Board Game Attributes"), br()),
                           h4("Investigating trends for popular Board Games."),
                           align = "center", background = "olive"
                           )
                       )
              ),
              fluidRow(
                tabBox(width = 8,
                       tabPanel("Avgerage Rating", plotOutput("violin_avg_rating")),
                       tabPanel("Game Complexity", plotOutput("violin_complex")),
                       tabPanel("Year Published", plotOutput("violin_year")),
                       tabPanel("Minimum Age", plotOutput("violin_age")),
                       tabPanel("Play Time", plotOutput("violin_time"))
                ),
                box(width = 4,
                    p("The average ratings graph shows that as more people own the games that they are more likely to have a better rating. 
                      The range of the average ratings also become more spread out as less people own the game. 
                      There can be more noise in average ratings when less people own and then rate games."),
                    p("The game complexity is surprising that more popular games tend to be more complex. 
                      The least popular games, where two to 126 people reported owning that game, have the majority of their games with a complexity rating below 2 out of 5."),
                    p("Games owned by publishing year all have a similar interquartile range of about 2006 to 2016. 
                      But the graph does show that game production has been steadily increasing. 
                      The downturn in 2020 is explained by this data being accessed in mid 2020."),
                    p("The age recommendation graph shows that the most popular games are not recommended for children under the age of 10. 
                      But notice that the least popular games have a much longer tail of recommended games. 
                      Those are typically 'Mature' or adult themed games."),
                    p("Median Play Time tends to increase for more popular games. 
                      This could be related to those more popular games also being more complex and therefore need more time to resolve. P
                      lay time also has a notably long tail to its distribution where most games end in under an hour and half, 
                      while some games go longer than six hours."))
              )
            )
    ),
    ############### Game mechanics tab
    tabItem(tabName = 'mechanic',
            fluidPage(
              fluidRow(
                column(offset = 2, width = 8,
                       box(width = 12, h1(tags$b("The Trends in Game Mechanics")),
                           align = "center", background = "olive"))
              ),
              fluidRow(h2("Specific Game Mechanic Trends")),
              fluidRow(
                selectInput("mech_choice", label = h4("Select Game Mechanic"),
                            choices = list("Dice Rolling" = "mech_Dice.Rolling",
                                           "Hand Management" = "mech_Hand.Management",
                                           "Set Collection" = "mech_Set.Collection",
                                           "Variable Player Powers" = "mech_Variable.Player.Powers",
                                           "Card Drafting" = "mech_Card.Drafting" ,
                                           "Tile Placement" = "mech_Tile.Placement",
                                           "Modular Board" = "mech_Modular.Board",
                                           "Area Majority Influence" = "mech_Area.Majority...Influence",
                                           "Cooperative Game" = "mech_Cooperative.Game",
                                           "Hexagon Grid" = "mech_Hexagon.Grid",
                                           "Simulation" = "mech_Simulation",
                                           "Action Points" = 'mech_Action.Points',
                                           "Simultaneous Action Selection" = 'mech_Simultaneous.Action.Selection',
                                           "Auction Bidding" = 'mech_Auction.Bidding',
                                           "Area Movement" = 'mech_Area.Movement',
                                           "Grid Movement"= 'mech_Grid.Movement',
                                           "Take That" = 'mech_Take.That',
                                           "Roll Spin and Move" = 'mech_Roll...Spin.and.Move',
                                           "Team Based Game" = 'mech_Team.Based.Game',
                                           "Push Your Luck" = 'mech_Push.Your.Luck',
                                           "Memory" = 'mech_Memory',
                                           "Worker Placement" = 'mech_Worker.Placement',
                                           "Deck Bag and Pool Building" = 'mech_Deck..Bag..and.Pool.Building',
                                           "Point to Point Movement" = 'mech_Point.to.Point.Movement',
                                           "Pattern Building" = 'mech_Pattern.Building',
                                           "Player Elimination" = 'mech_Player.Elimination',
                                           "Trading" = 'mech_Trading',
                                           "Network and Route Building" = 'mech_Network.and.Route.Building',
                                           "Campaign Battle Card Driven"  = 'mech_Campaign...Battle.Card.Driven',
                                           "Solo Solitaire Game" =  'mech_Solo...Solitaire.Game',
                                           "Pick up and Deliver" = 'mech_Pick.up.and.Deliver',
                                           "Pattern Recognition" = 'mech_Pattern.Recognition',
                                           "Secret Unit Deployment"  = 'mech_Secret.Unit.Deployment',
                                           "Role Playing" = 'mech_Role.Playing',
                                           "Storytelling"  = 'mech_Storytelling',
                                           "Voting" = 'mech_Voting',
                                           "Action Queue" = 'mech_Action.Queue',
                                           "Betting and Bluffing"  = 'mech_Betting.and.Bluffing',
                                           "Paper and Pencil" = 'mech_Paper.and.Pencil',
                                           "Variable Phase Order" =  'mech_Variable.Phase.Order',
                                           "Trick taking" = 'mech_Trick.taking',
                                           "Stock Holding"  = 'mech_Stock.Holding',
                                           "Chit Pull System" = 'mech_Chit.Pull.System',
                                           "Acting" = 'mech_Acting',
                                           "Commodity Speculation" = 'mech_Commodity.Speculation',
                                           "Enclosure" = 'mech_Enclosure',
                                           "Rock Paper Scissors" =  'mech_Rock.Paper.Scissors',
                                           "Movement Points" = 'mech_Movement.Points',
                                           "Time Track" = 'mech_Time.Track',
                                           "Line Drawing" =   'mech_Line.Drawing'),
                            selected = "mech_Variable.Player.Powers"),
                tabBox(width = 12,
                       tabPanel("Avgerage Rating", plotOutput("mech_violin_avg_rating")),
                       tabPanel("Game Complexity", plotOutput("mech_violin_complex")),
                       tabPanel("Year Published", plotOutput("mech_violin_year")),
                       tabPanel("Minimum age", plotOutput("mech_violin_age")),
                       tabPanel("Playing Time", plotOutput("mech_violin_time")),
                       tabPanel("Owned Percentile", plotOutput("mech_group_bar_stack")),
                       tabPanel("Owned Count", plotOutput("mech_group_bar_total"))
                       )
            )
    )
    ),
    ############### Game Categorys tab
    tabItem(tabName = 'category',
            
            fluidPage(
              fluidRow(
                column(offset = 2, width = 8,
                       box(width = 12, h1(tags$b("The Trends in Game Categories")),
                           align = "center", background = "maroon"))
              ),
              fluidRow(h2("Specific Game Category Trends")),
              fluidRow(
                selectInput("cat_choice", label = h4("Select Game Category"),
                            choices = list("Card Game" = "cat_Card.Game",
                                           "Wargame" = "cat_Wargame",
                                           "Fantasy" = "cat_Fantasy",
                                           "Party Game" = "cat_Party.Game",
                                           "Dice" = "cat_Dice" ,
                                           "Fighting" = "cat_Fighting",
                                           "Science Fiction" = "cat_Science.Fiction",
                                           "Childrens Game" = "cat_Childrens.Game",
                                           "Economic" = "cat_Economic",
                                           "Animals" = "cat_Animals",
                                           "Abstract Strategy" = "cat_Abstract.Strategy",
                                           "Bluffing" = 'cat_Bluffing',
                                           "Humor" = 'cat_Humor',
                                           "Deduction" = 'cat_Deduction',
                                           "Adventure" = 'cat_Adventure',
                                           "Miniatures"= 'cat_Miniatures',
                                           "Action Dexterity" = 'cat_Action...Dexterity',
                                           "Medieval" = 'cat_Medieval',
                                           "Movies TV Radio theme" = 'cat_Movies...TV...Radio.theme',
                                           "World War II" = 'cat_World.War.II',
                                           "Exploration" = 'cat_Exploration',
                                           "Real time" = 'cat_Real.time',
                                           "Ancient" = 'cat_Ancient',
                                           "Horror" = 'cat_Horror',
                                           "Negotiation" = 'cat_Negotiation',
                                           "Puzzle" = 'cat_Puzzle',
                                           "Racing" = 'cat_Racing',
                                           "City Building" = 'cat_City.Building',
                                           "Print Play"  = 'cat_Print...Play',
                                           "Nautical" =  'cat_Nautical',
                                           "Territory Building" = 'cat_Territory.Building',
                                           "Trivia" = 'cat_Trivia',
                                           "Memory"  = 'cat_Memory',
                                           "Novel based" = 'cat_Novel.based',
                                           "Educational"  = 'cat_Educational',
                                           "Political" = 'cat_Political',
                                           "Mythology" = 'cat_Mythology',
                                           "Collectible Components"  = 'cat_Collectible.Components',
                                           "Sports" = 'cat_Sports',
                                           "Word Game" =  'cat_Word.Game',
                                           "Transportation" = 'cat_Transportation',
                                           "Civilization"  = 'cat_Civilization',
                                           "Pirates" = 'cat_Pirates',
                                           "Trains" = 'cat_Trains',
                                           "Video Game Theme" = 'cat_Video.Game.Theme',
                                           "Comic Book Strip" = 'cat_Comic.Book...Strip',
                                           "Industry Manufacturing" =  'cat_Industry...Manufacturing',
                                           "Murder Mystery" = 'cat_Murder.Mystery',
                                           "Renaissance" = 'cat_Renaissance',
                                           "Space Exploration" =   'cat_Space.Exploration'),
                            selected = "cat_Fantasy"),
                tabBox(width = 12,
                       tabPanel("Avgerage Rating", plotOutput("cat_violin_avg_rating")),
                       tabPanel("Game Complexity", plotOutput("cat_violin_complex")),
                       tabPanel("Year Published", plotOutput("cat_violin_year")),
                       tabPanel("Minimum age", plotOutput("cat_violin_age")),
                       tabPanel("Playing Time", plotOutput("cat_violin_time")),
                       tabPanel("Owned Percentile", plotOutput("cat_group_bar_stack")),
                       tabPanel("Owned Count", plotOutput("cat_group_bar_total"))
                )
              )
            )
    )
  )
  )
)
)