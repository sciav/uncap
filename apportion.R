state_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                "New Mexico", "New York", "North Carolina", "North Dakota",
                "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                "Vermont", "Virginia", "Washington", "West Virginia",
                "Wisconsin", "Wyoming")

apportion <- function(states,
                      nseats = 435,
                      new_states = NULL) {
  seat_cap <- max(nseats * (ceiling(states[["Pop"]] / sum(states[["Pop"]], na.rm = TRUE) * 100) + 5), na.rm = TRUE) %/% 100
  n <- seq_len(seat_cap)
  mult <- 1 / sqrt(n * (n - 1))
  priority <- outer(states[["Pop"]], mult)
  rownames(priority) <- states[["State"]]
  st_dat <- states[["State"]]
  non_states <- setdiff(st_dat, c(state_list, new_states))
  priority[non_states, ] <- 0
  ap <- order(priority, decreasing = TRUE)[seq_len(nseats)]
  seats <- table(rep(states[["State"]], length.out = max(ap, na.rm = TRUE))[ap],
                 dnn = "State")
  df <- as.data.frame(seats, responseName = "Seats")
  df[["Population"]] <- states[["Pop"]]
  df[c("Population %", "Seat %", "Rep Factor")] <- list(
    pop_prop <- states[["Pop"]] / sum(states[["Pop"]], na.rm = TRUE),
    seat_prop <- seats / nseats,
    seat_prop / pop_prop)
  ev <- df[["Seats"]] + 2 * (df[["Seats"]] > 0)
  if ("District of Columbia" %in% non_states) {
    ev[df[["State"]] == "District of Columbia"] <- 3
  }
  df[c("Electoral Votes", "Electoral Vote %")] <- list(ev,
                                                       ev_prop <- ev / sum(ev, na.rm = TRUE))
  df[["EV Factor"]] <- ev_prop / pop_prop
  df <- df[, c("State", "Population", "Population %", "Seats", "Seat %", "Rep Factor", "Electoral Votes", "Electoral Vote %", "EV Factor")]
  rbind(df,
        data.frame("State" = "United States",
                   "Population" = sum(states[["Pop"]]),
                   "Population %" = 1,
                   "Seats" = nseats,
                   "Seat %" = 1,
                   "Rep Factor" = 1,
                   "Electoral Votes" = sum(ev, na.rm = TRUE),
                   "Electoral Vote %" = 1,
                   "EV Factor" = 1, check.names = FALSE))
}

get_ev_results <- function(election_results, apportionment, prw) {
  if (!is.null(prw)) {
    new_dat <- list(Year = 0, State = "Puerto Rico", Winner = prw)
    election_results <- rbind(election_results, as.data.frame(new_dat))
  }
  full <- merge(election_results, apportionment)
  with(full, tapply(`Electoral Votes`, Winner, sum))
}
