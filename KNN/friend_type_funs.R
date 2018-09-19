to_one <- function(x) {
  stopifnot(is.vector(x))
  (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
}

compute_distance <- function(x, y, z, x_mean, y_mean, z_mean){
  sqrt((x-x_mean)^2+(y-y_mean)^2+(z-z_mean)^2)
}


classify_friend_type <-
  function(data, accuracy, f, g, i) {
    flight_max <- max(data$flight)
    game_max <- max(data$game)
    ice_cream_max <- max(data$ice_cream)
    
    flight_min <- min(data$flight)
    game_min <- min(data$game)
    ice_cream_min <- min(data$ice_cream)
    
    flight_range <- flight_max - flight_min
    game_range <- game_max - game_min
    ice_cream_range <- ice_cream_max - ice_cream_min
    
    data_to_one <- data %>%
      mutate(
        flight_to_one = to_one(flight),
        game_to_one = to_one(game),
        ice_cream_to_one = to_one(ice_cream)
      )
      result <- data_to_one %>%
      mutate(distance = sqrt(((f - flight_min)/flight_range - flight_to_one)^2 + ((g - game_min)/game_range - game_to_one)^2 + ((i - ice_cream_min)/ice_cream_range - ice_cream_to_one)^2)) %>%
      arrange(distance) %>%
    slice(1:(nrow(.) * accuracy)) %>% 
    select(distance, friend_type) %>%
      group_by(friend_type) %>%
      summarise(n_count = n()) %>%
      arrange(desc(n_count)) %>%
        slice(1) %>%
        as.data.frame()
    if (result$friend_type == 1) {
      print('not at all')
    } else{
      if (result$friend_type == 2) {
        print('in small doses')
      } else {
        print('in large doses')
      }
    }
  }
