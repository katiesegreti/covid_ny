function(input, output, session) {
  #get state total data
  selected_state <- reactive({
    state_data("NY")
  })
  #state name for chart
  state_name <- reactive({
    "New York"
  })
  #save selected county as reactive
  selected_county <- reactive({
    county_data("NY", input$county_selector)
  })
  
  selected_county_long <- reactive({
    county_data_long("NY", input$county_selector)
  })
  #testing section
  output$testing <- renderUI({
    #req(input$county_selector)
    #str(michigan)
    #str(input$state_selector)
    #str(texas)
    #str(selected_county())
    #str(selected_county()$test_date)
    #str(selected_county_long())
    #str(county_weekly())
    #str(selected_state())
    #str(county_table())
  })
  
  #select input for county
  output$county_selector <- renderUI({
    selectInput(
      inputId = "county_selector",
      label = "Select county:",
      choices = newyork$county
    )
    
  })
  #create tables for countyweek ssumamry
  county_weekly <- reactive({
    
    fourteenday_summary(newyork)
  })
  
  county_table <- reactive({
    req(input$county_selector)
    newyork %>%
      filter(county == input$county_selector) %>%
       mutate(seven_day_pos = rollmean(new_positives, 7, fill = NA, align = "right"),
              seven_day_pct = rollmean(pct_pos_today, 7, fill = NA, align = "right")) %>%
      select(county, test_date, new_positives, pct_pos_today, seven_day_pos, seven_day_pct)
  })
  
  #tables for county week summary
  output$county_table <- renderReactable({
    reactable(county_weekly(),
              defaultPageSize = 10,
              columns = list(
                county = colDef(
                  name = "County"
                ),
                fourteen_day_total = colDef(
                  name = "14 day total",
                  format = colFormat(separators = TRUE)
                ),
                prev_14_day_total = colDef(
                  name = "previous 14 day total",
                  format = colFormat(separators = TRUE)
                ),
                pct_chg = colDef(
                  name = "% change",
                  format = colFormat(percent = TRUE, digits = 0)
                )
              ),
              defaultSortOrder = "desc",
              defaultSorted = "fourteen_day_total",
              highlight = TRUE,
              striped = TRUE,
              theme = reactableTheme(
                color = "hsl(233, 9%, 87%)",
                backgroundColor = "hsl(233, 9%, 19%)",
                borderColor = "hsl(233, 9%, 22%)",
                stripedColor = "hsl(233, 12%, 22%)",
                highlightColor = "hsl(233, 12%, 24%)",
                inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
              ))
  })
  output$county_table_mobile <- renderReactable({
    
    reactable(county_weekly(),
              defaultPageSize = 10,
              columns = list(
                county = colDef(
                  name = "County"
                ),
                fourteen_day_total = colDef(
                  name = "14 day total",
                  format = colFormat(separators = TRUE)
                ),
                prev_14_day_total = colDef(
                  name = "previous 14 day total",
                  format = colFormat(separators = TRUE)
                ),
                pct_chg = colDef(
                  name = "% change",
                  format = colFormat(percent = TRUE, digits = 0)
                )
              ),
              defaultSortOrder = "desc",
              defaultSorted = "fourteen_day_total",
              highlight = TRUE,
              striped = TRUE,
              theme = reactableTheme(
                color = "hsl(233, 9%, 87%)",
                backgroundColor = "hsl(233, 9%, 19%)",
                borderColor = "hsl(233, 9%, 22%)",
                stripedColor = "hsl(233, 12%, 22%)",
                highlightColor = "hsl(233, 12%, 24%)",
                inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
              ))
  })
  
  
  #table for dates for a county
  output$date_table <- renderReactable({
    reactable(county_table()[,-1],
              columns = list(
                test_date = colDef(name = "Date", format = colFormat(date = TRUE)),
                new_positives = colDef(name = paste0(input$county_selector, " new cases"), format = colFormat(separators = TRUE)),
                pct_pos_today = colDef(name = "% positive", format = colFormat(percent = TRUE, digits = 2)),
                seven_day_pos = colDef(name = "7 day avg new cases", format = colFormat(separators = TRUE, digits = 2)),
                seven_day_pct = colDef(name = "7 day avg % positive", format = colFormat(percent = TRUE, digits = 2))
              ),
              defaultSortOrder = "desc",
              defaultSorted = "test_date",
              highlight = TRUE,
              striped = TRUE,
              theme = reactableTheme(
                color = "hsl(233, 9%, 87%)",
                backgroundColor = "hsl(233, 9%, 19%)",
                borderColor = "hsl(233, 9%, 22%)",
                stripedColor = "hsl(233, 12%, 22%)",
                highlightColor = "hsl(233, 12%, 24%)",
                inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
              )
              
              )
    
    
  })
  
  output$date_table_mobile <- renderReactable({
    reactable(county_table()[,-1],
              columns = list(
                test_date = colDef(name = "Date", format = colFormat(date = TRUE)),
                new_positives = colDef(name = paste0(input$county_selector, " new cases"), format = colFormat(separators = TRUE)),
                pct_pos_today = colDef(name = "% positive", format = colFormat(percent = TRUE, digits = 2)),
                seven_day_pos = colDef(name = "7 day avg new cases", format = colFormat(separators = TRUE, digits = 2)),
                seven_day_pct = colDef(name = "7 day avg % positive", format = colFormat(percent = TRUE, digits = 2))
              ),
              defaultSortOrder = "desc",
              defaultSorted = "test_date",
              highlight = TRUE,
              striped = TRUE,
              theme = reactableTheme(
                color = "hsl(233, 9%, 87%)",
                backgroundColor = "hsl(233, 9%, 19%)",
                borderColor = "hsl(233, 9%, 22%)",
                stripedColor = "hsl(233, 12%, 22%)",
                highlightColor = "hsl(233, 12%, 24%)",
                inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
              )
              
    )
    
    
  })
  
  #radio buttons for desktop or mobile
  # output$d_or_m <- renderUI({
  #   radioGroupButtons(inputId = "d_or_m",
  #                label = "",
  #                choiceNames = c("Desktop (interactive charts", "mobile (static charts"),
  #                choiceValues = c("desktop", "mobile"),
  #                selected = "mobile"
  #                )
  # })
  
  ##plotly
  output$county_plot <- renderPlotly({
    req(input$county_selector)
      plot_ly(
        x = selected_county()$test_date,
        y = selected_county()$new_positives,
        type = 'bar',
        color = I("Turquoise"),
        name = "new cases"
        
      ) %>% layout(title = paste0("New COVID-19 Cases By Day: ", 
                                  input$county_selector, " County, NY"),
                   # paper_bgcolor = 'rgba(253, 253, 253, 1)',
                   # plot_bgcolor = 'rgba(253, 253, 253, 1)') %>%
                   paper_bgcolor = 'rgba(0, 0, 0, 1)',
                   plot_bgcolor = 'rgba(0, 0, 0, 1)',
                   font = list(color = '#FFFFFF')) %>%
        add_lines(y = selected_county()$seven_day_avg,
                  line = list(color = 'yellow'),
                  name = "7 day avg", showlegend = FALSE)
    
    
    
  })
  #desktop (plotly) charts
  output$county_plot1 <- renderPlotly({
    req(input$county_selector)
    ggplotly(ggplot(selected_county(), aes(x = test_date, y = new_positives)) +
               geom_col(fill = "Turquoise", width = 0.6) +
               geom_line(aes(y = seven_day_avg), color = "yellow", size = 0.8) +
               counties_theme +
               labs(
                 x = "",
                 y = "",
                 #type = "col",
                 title = paste0("New COVID-19 Cases By Day: ", 
                                input$county_selector, " County, NY")
               ),
             type = "col")
  })
  
  output$county_plot2 <- renderPlotly({
    req(input$county_selector)
    
      ggplotly(ggplot(selected_county_long(), aes(x = test_date, y = value, fill = result)) +
                 geom_col(width = 0.6) +
                 scale_fill_manual(values = c("darkgrey", "Turquoise"))+
                 dark_theme_stacked +
                 #counties_theme_stacked +
                 labs(
                   x = "",
                   y = "",
                   
                   title = paste0("Tests Performed By Day: ", 
                                  input$county_selector, " County, NY")
                 )
      )
    
    
  })
  #state chart
  output$state_plot <- renderPlotly({
    #req(input$state_selector)
    plot_ly(
      x = selected_state()$test_date,
      y = selected_state()$total,
      type = 'bar',
      color = I("Turquoise"),
      name = "new cases"
      
    ) %>% layout(title = paste0("New COVID-19 Cases By Day: ", 
                                 state_name(), " (state totals)"),
                 paper_bgcolor = 'rgba(0, 0, 0, 1)',
                 plot_bgcolor = 'rgba(0, 0, 0, 1)',
                 font = list(color = '#FFFFFF')) 
  })
  
  #mobile (static) charts
  output$state_plot_mobile <- renderPlot({
    ggplot(selected_state(), aes(x = test_date, y = total)) +
      geom_col(fill = "Turquoise", width = 0.5) +
      #counties_theme +
      dark_theme +
      labs(
        x = "",
        y = "",
        title = "New COVID-19 Cases By Day",
        subtitle = paste0(state_name(), " (state totals)"),
        caption = "data-chips.com"
      )
  })
  output$county_plot1_mobile <- renderPlot({
    req(input$county_selector)
    ggplot(selected_county(), aes(x = test_date, y = new_positives)) +
      geom_col(fill = "Turquoise", width = 0.5) +
      geom_line(aes(y = rollmean(new_positives, 7, fill = NA, align = "right")), color = "yellow", size = 0.8) +
      #counties_theme +
      dark_theme +
      labs(
        x = "",
        y = "",
        title = "New COVID-19 Cases By Day",
        subtitle = paste0(input$county_selector, " County, NY"),
        caption = "data-chips.com"
      )
  })
  output$county_plot2_mobile <- renderPlot({
    req(input$county_selector)
   
      ggplot(selected_county_long(), aes(x = test_date, y = value, fill = result)) +
        geom_col(width = 0.5) +
        scale_fill_manual(values = c("darkgrey", "Turquoise"))+
        #counties_theme_stacked +
        dark_theme_stacked +
        labs(
          x = "",
          y = "",
          title = "Tests Performed By Day",
          subtitle = paste0(input$county_selector, " County, NY"),
          caption = "data-chips.com"
        )
    
    
    
    
    
  })
  
  
  output$latest_date <- renderUI({
    
    HTML(paste("Last update: ", max(newyork$test_date)))
    
  })
  
  output$county_msg <- renderUI({
    if(input$county_selector == "") {
      HTML("Please select a county from the drop-down menu")
    } else {}
    
  })
  output$county_msg_mobile <- renderUI({
    if(input$county_selector == "") {
      HTML("Please select a county from the drop-down menu")
    } else {}
    
  })
  
  output$latest_date_mobile <- renderUI({
    
    HTML(paste("Last update: ", max(newyork$test_date)))
  })
  
  output$positives_title <- renderUI({
    req(input$county_selector)
    HTML(paste("Positive COVID-19 tests in ", input$county_selector))
  })
  
  output$states_msg <- renderUI({
    HTML(paste0("*New York COVID-19 data is provided by the " , 
                a(href = "https://health.data.ny.gov/Health/New-York-State-Statewide-COVID-19-Testing/xdss-u53e", 
                  "New York State Department of Health.")))
  })
  
  # a(href = "http://www.data-chips.com/", "data chips homepage")
}