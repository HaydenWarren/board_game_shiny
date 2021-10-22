function(input, output, session){
  # Game Attributes
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
  # Board Games Sorted
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
  # Game Mechanic Trends
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

  # Game Category Trends
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
}