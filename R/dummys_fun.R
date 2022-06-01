dummys_fun <- function(pbd_datos_fixed){
  factor_names <- c('season', 'game_code', 'team_code', 'play_number', 'play_type')
  pbd_datos_fixed[,factor_names] <- lapply(pbd_datos_fixed[,factor_names] , factor)
  
  summary(pbd_datos_fixed[1:18])
  
  #CATEGORIZACION BBDD
  df <- pbd_datos_fixed
  N_df <- dim(df)[1] # Numero de jugadas
  M_df <- dim(df)[2] # Numero de variables
  
  df_fact <- df %>%
    select(season, game_code, team_code, player_name, team_name) %>%
    mutate(season = as.factor(season),
           team_code = as.factor(team_code),
           player_name = as.factor(player_name),
           team_name = as.factor(team_name))
  
  num_teams <- length(levels(df_fact$team_name)) # Numero de equipos
  
  players <- df %>% 
    select(matches("player_name")) %>%
    arrange(player_name) %>% 
    unique() %>% 
    drop_na()
  
  players <- players$player_name
  num_players <- length(players)   # Numero de jugadores
  
  df_pbp <- df %>% 
    select(season, game_code, quarter, seconds, points_home, points_away, home_team, away_team, matches("_player[1-5]"))
  
source(here("R", "CHR_to_seconds_fun.R"))
source(here("R", "Print_MS_fun.R"))
  
  
  TP_home <- df_pbp %>%
  select(c(contains("home_"))) %>%
  unique()
  
  TP_away <- df_pbp %>%
    select(c(contains("away_"))) %>%
    unique()
  
  CBIND_MultipleCol_n <- function(data,col,n){
    d <- unlist(data[col])
    x <- cbind(rep(d, n))
    y <- unlist(data[-1])
    
    res <- cbind(x, y)
    rownames(res) <- NULL
    
    res <- unique(res)
    
    return(res)
  }
  
  TeamPlayers_home <- CBIND_MultipleCol_n(TP_home, 1, 5)
  TeamPlayers_away <- CBIND_MultipleCol_n(TP_away, 1, 5)
  
  TeamPlayers <- rbind(TeamPlayers_home, TeamPlayers_away) %>%
    as.data.frame() %>%
    `colnames<-`(c("Team", "Player")) %>%
    arrange(Team) %>%
    unique()
  
  Players_Sorted_byTeam <- TeamPlayers$Player
  
  Lineups_PasteSort <- function(x) {
    paste(sort(x), collapse = " - ")
  }
  
  lineup_home <- df_pbp %>% select(contains("home_p"))
  lineup_away <- df_pbp %>% select(contains("away_p"))
  lineups     <- cbind(lineup_home, lineup_away)
  
  lineup_home_sorted <- apply(lineup_home, 1, Lineups_PasteSort)
  lineup_away_sorted <- apply(lineup_away, 1, Lineups_PasteSort)
  lineups_sorted     <- apply(lineups, 1, Lineups_PasteSort)
  
  #lineups
  df_lineups_sorted <- df_pbp %>%
    mutate(lineup = lineups_sorted,
           lineup_home = lineup_home_sorted,
           lineup_away = lineup_away_sorted
    ) %>% select(-contains("_player"))
  
  df_merged <- df_lineups_sorted %>%
    mutate(game_code = paste0("G", str_pad(game_code, 6, pad = "0")), 
           quarter   = paste0("Q", quarter)) %>%
    unite("SeasonGame", c("season", "game_code"))
  
  df_stints <- df_merged %>%
    arrange(SeasonGame) %>%
    mutate(StintChanged = (lineup != lag(lineup)),
           StintChanged = replace_na(StintChanged, TRUE),
           stint        = cumsum(StintChanged))
  
  df_reduced <- df_stints %>%
    mutate(StintRemove  = (stint == lead(stint)),
           StintRemove  = replace_na(StintRemove, FALSE)) %>%
    filter(StintRemove != TRUE)
  
  PlusMinus_function <- function(h,a){
    (h-lag(h))-(a-lag(a))
  }
  
  #Home como referencia
  df_PlusMinus <- df_reduced %>%
    group_by(SeasonGame) %>%
    mutate(
      stint_seconds = ifelse(is.na(lag(seconds)), seconds, seconds - lag(seconds)),
      PlusMinus  = ifelse(is.na(lag(seconds)), points_home - points_away,
                          PlusMinus_function(points_home, points_away))
    ) %>% ungroup()
  
  df_PlusMinus_reduced <- df_PlusMinus %>% 
    select(c(SeasonGame, quarter, lineup, lineup_home, lineup_away, stint_seconds, PlusMinus)) 

  #Eliminar stints duplicados
  df_PlusMinus_reduced_bylineups <- df_PlusMinus_reduced %>% 
    group_by(SeasonGame, quarter, lineup, lineup_home, lineup_away) %>% 
    summarise(
      stint_seconds = sum(stint_seconds),
      PlusMinus  = sum(PlusMinus) ) %>% 
    ungroup() %>%
    as.data.frame()
  
  #10min
  df_dummys_H <- fastDummies::dummy_cols(df_PlusMinus_reduced_bylineups, 
                                         select_columns = "lineup_home", 
                                         split = "-")
  df_dummys_A <- fastDummies::dummy_cols(df_PlusMinus_reduced_bylineups, 
                                         select_columns = "lineup_away", 
                                         split = "-") %>% 
    mutate(across(starts_with("lineup_away_"), function(x) -x))
  
  Remove_firsts_chars_colnames <- function(data, char){
    num_char <- nchar(char)+1
    substring(names(data), num_char)
  }
  
  COL_From <- function(data, first_col){
    last_col = ncol(data)
    colnames(data[first_col:last_col])
  }
  
  COL_to <- function(data, first_col, char){
    last_col = ncol(data)
    Remove_firsts_chars_colnames(data[first_col:last_col], char)
  }
  
  #Primera columna con el nombre de un jugador
  match <- match(paste0("lineup_away_", players), names(df_dummys_A)) %>%
    na.omit() %>%
    min()
  
  #match = 8
  
  #nombre columnas sin primeras palabras:
  col_to_A <- COL_to(df_dummys_A, match, "lineup_away_")
  col_to_H <- COL_to(df_dummys_H, match, "lineup_home_")
  
  df_dummys_A <- df_dummys_A %>% rename_at(vars(COL_From(df_dummys_A,match)), function(x) col_to_A)
  df_dummys_H <- df_dummys_H %>% rename_at(vars(COL_From(df_dummys_H,match)), function(x) col_to_H)
  
  #Juntar los dos DF:
  df_dummys_A[df_dummys_A == 0] <- NA
  df_dummys_H[df_dummys_H == 0] <- NA
  
  df_dummys <- coalesce(df_dummys_H,df_dummys_A)
  df_dummys[is.na(df_dummys)] <- 0
  
  df_dummys <- df_dummys %>% select(-c(starts_with("lineup_"))) %>% ungroup()
}
