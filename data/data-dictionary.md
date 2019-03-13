# workout 01 data dictionary

+ team_name: factor - name of the team (Golden State Warriors for all players in this report)
+ game_date: character - date in which shot took place
+ season: factor - season in which shot took place
+ period: integer - period in which shot took place (4 periods per game, 12 minutes per period)
+ minutes_remaining: integer - minutes left in the period in which shot took place
+ seconds_remaining: integer - seconds left in the minute in which shot took place
+ shot_made_flag: factor - two levels indicating if the shot was successful (shot_yes for yes, shot_no for no)
+ action_type: factor - what kind of move was made (i.e. jump shot, layup, etc.)
+ shot_type: factor - whether th shot attempt was for 2 points or 3 points
+ shot_distance: integer - distance to basket (feet)
+ opponent: character - opposing team of the game
+ x: intger - x coordinate of shot origin (inches)
+ y: integer - y coordinate of shot origin (inches)
+ name: character - name of player who attempted shot
+ minute: integer - minutes elapsed since start of game at time of shot