workout 01
================
Everett Fisher

The question
------------

What makes a good basketball player? There's certainly no singular answer, but everyone can agree that point-scoring ability is a major factor. But sheer number of points scored is too simple a measure: some players have had many more opportunities to shoot than others. Some players are also better at 2 point shots but less adept at 3 point shots.

In this report, we examine the effectiveness of some of the Golden State Warriors' shots during the 2016 season. We focused on all of the shots taken by Andre Iguodala, Draymond Green, Kevin Durant, Klay THompson, and Stephen Curry. The players' shots were mapped onto a court diagram and analyzed for accuracy/effectiveness.

The data
--------

Data for each player's shots include the type of shot (i.e. 2 or 3 points), the coordinates for where the shot was taken (measured in inches), whether the shot was successful, as well as other information not utilized in our analysis.

A preview of the raw data for Stephen Curry is included here, as are text summaries for the combined data sets.

    ## # A tibble: 6 x 15
    ##   team_name game_date season period minutes_remaini~ seconds_remaini~
    ##   <fct>     <chr>     <fct>   <int>            <int>            <int>
    ## 1 Golden S~ 12/15/16  2016        3                3               51
    ## 2 Golden S~ 10/28/16  2016        3                9               14
    ## 3 Golden S~ 11/1/16   2016        2                5                8
    ## 4 Golden S~ 12/1/16   2016        3                5               27
    ## 5 Golden S~ 4/4/17    2016        3                2                4
    ## 6 Golden S~ 11/19/16  2016        4                5               36
    ## # ... with 9 more variables: shot_made_flag <fct>, action_type <fct>,
    ## #   shot_type <fct>, shot_distance <int>, opponent <chr>, x <int>,
    ## #   y <int>, name <chr>, minute <dbl>

    ##      period          minute     seconds_remaining  shot_made_flag
    ##  Min.   :1.000   Min.   : 1.0   Min.   : 0.00     shot_no :2078  
    ##  1st Qu.:1.000   1st Qu.:11.0   1st Qu.:13.00     shot_yes:2256  
    ##  Median :2.000   Median :23.0   Median :29.00                    
    ##  Mean   :2.351   Mean   :22.8   Mean   :28.60                    
    ##  3rd Qu.:3.000   3rd Qu.:33.0   3rd Qu.:43.75                    
    ##  Max.   :4.000   Max.   :48.0   Max.   :59.00                    
    ##                                                                  
    ##               action_type            shot_type          x           
    ##  Jump Shot          :2051   2PT Field Goal:2402   Min.   :-248.000  
    ##  Pullup Jump shot   : 521   3PT Field Goal:1932   1st Qu.: -56.000  
    ##  Layup Shot         : 215                         Median :   1.000  
    ##  Driving Layup Shot : 160                         Mean   :   7.746  
    ##  Step Back Jump shot: 133                         3rd Qu.:  95.500  
    ##  Running Layup Shot : 115                         Max.   : 246.000  
    ##  (Other)            :1139                                           
    ##        y        
    ##  Min.   :-39.0  
    ##  1st Qu.: 13.0  
    ##  Median : 95.0  
    ##  Mean   :109.4  
    ##  3rd Qu.:198.0  
    ##  Max.   :717.0  
    ## 

Jump shots are the most common type of action taken, with layups being second most common.

The analysis
------------

Here we have mapped out the locations where each player initiated a shot, colored by the success or failure of each shot.

<img src="../images/gsw-shot-charts.png" width="100%" style="display: block; margin: auto;" />

From this chart, there's an immediate imbalance in the number of shots taken from player to player. Intuition might tell us that the players with fewer overall shots may have a higher percentage of made shots, as they had fewer opportunities to fail. Thus, it's difficult to make accurate comparisons form this visual representation. For that purpose, we calculated the effective shooting percentage of each player, displayed here for 2 PT, 3PT, and overall shots. The code for each chart is similar to the code included for 2PT shots.

#### 2PT Effective Shooting % by Player

``` r
eff_shoot_2pt <- shots_data
eff_shoot_2pt <- eff_shoot_2pt %>%
  filter(shot_type == "2PT Field Goal") %>%
  select(name, shot_made_flag) %>%
  group_by(name) %>%
  summarize(total = length(shot_made_flag), made = length(shot_made_flag[shot_made_flag == "shot_yes"]), perc_made = round(made/total, digits = 3)) %>%
  arrange(desc(perc_made))
kable(eff_shoot_2pt, caption = "2PT Effective Shooting % by Player")
```

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Andre Iguodala |    210|   134|       0.638|
| Draymond Green |    346|   175|       0.506|
| Klay Thompson  |    640|   311|       0.486|
| Stephen Curry  |    563|   259|       0.460|
| Kevin Durant   |    643|   253|       0.393|

#### 3PT Effective Shooting % by Player

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Draymond Green |    232|   158|       0.681|
| Kevin Durant   |    272|   167|       0.614|
| Stephen Curry  |    687|   407|       0.592|
| Klay Thompson  |    580|   334|       0.576|
| Andre Iguodala |    161|    58|       0.360|

#### Overall Effective Shooting % by Player

| name           |  total|  made|  perc\_made|
|:---------------|------:|-----:|-----------:|
| Draymond Green |    578|   333|       0.576|
| Stephen Curry  |   1250|   666|       0.533|
| Klay Thompson  |   1220|   645|       0.529|
| Andre Iguodala |    371|   192|       0.518|
| Kevin Durant   |    915|   420|       0.459|

Now we see that Draymond Green has the highest shot effectiveness for 3 point shots (68.1%) and overall (57.6%), taking only second place among 2 point shots. Stephen Curry and Klay Thompson have very similar numbers of overall shots taken and effectiveness percentages, but Curry leads the two in 3 point shots while Thompson leads in 2 point shots. Andre Iguodala had the lowest percent made for 3 point shots, but the highest percent of 2 point shots. This fluctation makes sense, given the relatively low number of shots taken overall.

The average effective shooting eprcentage for all five players is:

``` r
kable(shots_data %>%
  summarize(total = length(shot_made_flag), made =  length(shot_made_flag[shot_made_flag == "shot_yes"]), perc_made = round(made/total, digits = 3)) %>%
  arrange(desc(perc_made)))
```

|  total|  made|  perc\_made|
|------:|-----:|-----------:|
|   4334|  2256|       0.521|

The takeaway
------------

Of the five players analysed, it seems that Draymond Green is the most effective shooter. However, additional analysis may give more insight beyond the scope of this report. Particularly of interest would be a look into effectiveness when grouping by the type of shot (layup, junp shot, etc.)
