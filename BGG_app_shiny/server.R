


  
# 

function(input, output, session){



#   
#   output$totals_table <- renderTable(
#     og_bonapp_df %>% group_by(Year = factor(year(published))) %>% 
#       summarise("Recipe Count" = n_distinct(dishtitle)) %>% na.omit()
#   )
#   
#   output$month_totals_dodge <- renderPlot(
#     og_bonapp_df %>% select(published, dishtitle) %>% 
#       group_by(Year = as.character(year(published)), Month = month(published, label = TRUE)) %>% 
#       summarise(totals = n_distinct(dishtitle)) %>% group_by(Month) %>% 
#       mutate(mean_totals = mean(totals)) %>% na.omit() %>% ggplot() + 
#       geom_col(aes(x = Month, y = totals, fill = Year), position = "dodge") +
#       geom_point(aes(x = Month, y = mean_totals), shape = 23, size = 3, fill = "red", color = "black") + 
#       ggtitle("Number of Recipes Published From 2014-2020") + #theme(legend.title=element_blank()) +
#       xlab("Months") + ylab("Recipe Count") + theme_dark() + scale_fill_brewer(palette = "YlGnBu")
#   )
#   
#   output$interactive_year_totals <- renderPlot(
#     og_bonapp_df %>% filter(year(published) %in% c(input$checkGroup1)) %>% 
#       group_by(Year = as.character(year(published)), Month = month(published, label = TRUE)) %>% 
#       summarise(recipe_count = n_distinct(dishtitle)) %>% 
#       ggplot(aes(x=Month, y=recipe_count)) + geom_col(position = "dodge", aes(fill=Year)) +
#       theme_bw() + ylab("Total Recipes") + xlab("Months") + labs(fill = "Year") + 
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#       scale_fill_manual(breaks = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"), 
#                         aesthetics = "fill", values = brewer.pal(7, "Set3"))
#   )
#   
#   
  # violin
#   
#   output$ratings_first <- renderInfoBox(
#     infoBox(title = "Most Rated Dish", value = top_rated_dish$dishtitle[1], color = "green",
#             subtitle = paste(scales::comma(top_rated_dish$ratings_count[1]), "Total Ratings"), href = top_rated_dish$url[1], fill = TRUE)
#   )
#   
#   output$ratings_second <- renderInfoBox(
#     infoBox(title = "Second Most Rated", value = top_rated_dish$dishtitle[2], color = "green", 
#             subtitle = paste(scales::comma(top_rated_dish$ratings_count[2]), "Total Ratings"), href = top_rated_dish$url[2])
#   )
#   
#   output$ratings_third <- renderInfoBox(
#     infoBox(title = "Third Most Rated", value = top_rated_dish$dishtitle[3], color = "green", 
#             subtitle = paste(scales::comma(top_rated_dish$ratings_count[3]), "Total Ratings"), href = top_rated_dish$url[3])
#   )
#   'avg_rating','average ranking graph',1.4)
  output$violin_avg_rating <- renderPlot(
    graph_violinplot('avg_rating','Games Owned by Average Rating',1.4)
    
  )

  output$violin_complex <- renderPlot(
    graph_violinplot('complex','Games Owned by Complexity Rating',1.4)
    
  )

  output$violin_year <- renderPlot(
    graph_violinplot('year_published','Games Owned by Year Published',1.4)
    
    )
  
  output$violin_age <- renderPlot(
    graph_violinplot('min_age','Games Owned by Recommended Minimum Age',1.4)
  )
  
  output$violin_time <- renderPlot(
    graph_violinplot('play_time','Games Owned by Play Time',1.4)
  )
#   violin_age
#   
  
  
  # stacked_bar
  
  
  output$mech_bar_per <- renderPlot(
    graph_stacked_bar("mech_Acting",
                      "mech_Global.Mechanics.Average",
                      'Mechanics')
  )
  
  output$mech_bar_total <- renderPlot(
    graph_total_bar("mech_Acting",
                    "mech_Zone.of.Control",
                    'Mechanics')
  )
  
  output$cat_bar_per <- renderPlot(
    graph_stacked_bar("cat_Abstract.Strategy",
                      "cat_Global.Categorys.Average",
                      'Categories')
  )
  
  output$cat_bar_total <- renderPlot(
    graph_total_bar("cat_Abstract.Strategy",
                    "cat_Zombies",
                    'Categories')
  )
#   ## Reviews
#   
#   output$reviews_first <- renderInfoBox(
#     infoBox(title = "Most Reviewed Dish", value = top_reviewed_dish$dishtitle[1], color = "green", 
#             subtitle = paste(top_reviewed_dish$review_count[1], "Total Reviews"), href = top_reviewed_dish$url[1], fill = TRUE)
#   )
#   
#   output$reviews_second <- renderInfoBox(
#     infoBox(title = "Second Most Reviewed", value = top_reviewed_dish$dishtitle[2], color = "green", 
#             subtitle = paste(top_reviewed_dish$review_count[2], "Total Reviews"), href = top_reviewed_dish$url[2])
#   )
#   
#   output$reviews_third <- renderInfoBox(
#     infoBox(title = "Third Most Reviewed", value = top_reviewed_dish$dishtitle[3], color = "green", 
#             subtitle = paste(top_reviewed_dish$review_count[3], "Total Reviews"), href = top_reviewed_dish$url[3])
#   )
#   
#   output$timelapse_reviews <- renderPlot(
#     ggplot(og_bonapp_df, aes(x=published, y=review_count)) + geom_jitter(aes(color=review_count)) + 
#       xlab("Year, Month Published") + ylab("Total Reviews Written") + ggtitle("Total Reviews Written per Recipe") +
#       labs(color = "Count") + ylim(1, 150) + scale_color_gradient(low = "yellow", high = "blue", trans = "log") + 
#       theme(legend.position="none")
#   )
#   output$reviews_table <- renderTable(
#     og_bonapp_df %>% group_by(Year = as.character(year(published))) %>% 
#       summarise("Review Count" = scales::comma(sum(review_count))) %>% 
#       filter(!is.na(Year))
#   )
#   
#   output$years_best <- renderDataTable(
#     og_bonapp_df %>% group_by(Year = year(published)) %>% arrange(desc(review_count)) %>% 
#       top_n(n = 10, wt = review_count) %>% arrange(desc(rating)) %>% top_n(n = 1, wt = rating) %>% 
#       mutate("Dish Title" = paste0("<a href='", url, "'>", dishtitle, "</a>"), rating = round(rating, 2)) %>% 
#       select(Year, "Dish Title", Published = published, Rating = rating, "Ratings Count" = ratings_count, 
#              "Reviews Count" = review_count, -url) %>% arrange(Year) %>% 
#       datatable(escape = 1, rownames = FALSE, options = list(dom = 't'))
#   )
#   
#   # Ingredients
#   
#   output$ingred_bubbles <- renderBubbles({
#     
#     bubble_df <- ingred_badata_df %>% group_by(ingred) %>% 
#       summarise(frequency_count = n_distinct(dishtitle)) %>%
#       arrange(desc(frequency_count)) %>% head(50)
#     
#     bubbles(value = bubble_df$frequency_count, label = bubble_df$ingred, 
#             key = bubble_df$ingred, tooltip = bubble_df$ingred, 
#             color =  as.vector(wes_palette(50, name = "Moonrise3", type = "continuous")))
#   })
#   
#   output$pop_ingred_table <- renderDataTable(
#     ingred_badata_df %>% group_by(Ingredient = ingred) %>% 
#       summarise(frequency_count = n_distinct(dishtitle)) %>%
#       arrange(desc(frequency_count)) %>% transmute(Ingredient, "Frequency Count" = scales::comma(frequency_count))
#   )
#   
  # output$ingred_interactive <- renderPlot(
  #   graph_group_violinplot(input$ingred_choice, 'avg_rating')                                                                                                                                         ))))))))))))))))))
  # )
  output$mech_violin_avg_rating <- renderPlot(
      graph_group_violinplot(input$mech_choice, 'avg_rating')                
  )
  
  output$mech_violin_complex <- renderPlot(
    graph_group_violinplot(input$mech_choice, 'complex')                
  )
  
  output$mech_violin_year <- renderPlot(
    graph_group_violinplot(input$mech_choice, 'year_published')                
  )
  
  output$mech_violin_age <- renderPlot(
    graph_group_violinplot(input$mech_choice, 'min_age')                
  )
  
  output$mech_violin_time <- renderPlot(
    graph_group_violinplot(input$mech_choice, 'play_time')                
  )
  
  output$mech_group_bar_stack <- renderPlot(
    graph_stacked_bar_group(input$mech_choice,'mech_Global.Mechanics.Average')
    )

  output$mech_group_bar_total <- renderPlot(
    graph_total_bar_group(input$mech_choice)
  )

  ####
  
  
  output$cat_violin_avg_rating <- renderPlot(
    graph_group_violinplot(input$cat_choice, 'avg_rating')                
  )
  
  output$cat_violin_complex <- renderPlot(
    graph_group_violinplot(input$cat_choice, 'complex')                
  )
  
  output$cat_violin_year <- renderPlot(
    graph_group_violinplot(input$cat_choice, 'year_published')                
  )
  
  output$cat_violin_age <- renderPlot(
    graph_group_violinplot(input$cat_choice, 'min_age')                
  )
  
  output$cat_violin_time <- renderPlot(
    graph_group_violinplot(input$cat_choice, 'play_time')                
  )
  
  output$cat_group_bar_stack <- renderPlot(
    graph_stacked_bar_group(input$cat_choice,'cat_Global.Categorys.Average')
  )
  
  output$cat_group_bar_total <- renderPlot(
    graph_total_bar_group(input$cat_choice)
  )
#   # COVID-19
#   
#   output$recipes_in_2020 <- renderPlot(
#     og_bonapp_df %>% filter(year(published) == 2020) %>% group_by(published) %>% 
#       summarise(recipe_count = n_distinct(dishtitle)) %>% 
#       ggplot(aes(x=published, y=recipe_count)) + geom_col(fill="#376c8b") + 
#       ggtitle("Monthly Total Recipes Published in 2020") + xlab("Months") + ylab("Recipe Count") + 
#       theme(legend.title=element_blank())
#   )
#   
#   output$review_count_2020 <- renderPlot(
#     og_bonapp_df %>% filter(year(published) == 2020) %>% group_by(published) %>% 
#       summarise(review_count = sum(review_count)) %>% 
#       ggplot(aes(x=published, y=review_count)) + 
#       geom_col(fill="#aabad6") + theme(legend.title=element_blank()) + 
#       ggtitle("Monthly Total Reviews Written in 2020") + xlab("Months") + ylab("Review Count")
#   )
#   
#   output$avg_review_count <- renderPlot(
#     og_bonapp_df %>% filter(year(published) %in% c(as.integer(unlist(strsplit(input$all_years, ", "))))) %>%
#       group_by(Year = as.character(year(published)), published) %>%
#       summarise(avg_reviews = sum(review_count) / n_distinct(dishtitle)) %>%
#       ggplot(aes(x = month(published, label = TRUE), y = avg_reviews)) +
#       geom_line(size = 1.3, aes(group = factor(Year), color = factor(Year))) +
#       theme(legend.title=element_blank()) + 
#       ggtitle("Average Reviews Recieved, by Year") + xlab("Months") + ylab("Average Review Count") + 
#       scale_color_manual(breaks = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"), 
#                          values = rev(brewer.pal(7, "Set3")))
#   )
#   
#   output$monthly_review_count <- renderPlot(
#     ggplot(og_bonapp_df, aes(x=published, y=review_count)) + geom_col(aes(fill=as.character(year(published)))) +
#       theme(legend.title=element_blank()) + xlab("Months of Publishing") + ylab("Total Reviews Written per Month") +
#       ggtitle("Total Reviews Written by Month") + 
#       scale_fill_manual(breaks = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"), 
#                         values = rev(brewer.pal(7, "Set3"))) + theme(legend.position="none")
#   )
#   
}