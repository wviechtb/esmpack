# Helper functions to recover day and beep numbers.

# Construct day numbers from notification numbers.
get.day.number <- function(notification.number, beeps.per.day = 10, total.notifications = 50) {
    # Reality check.
    if(total.notifications %% beeps.per.day > 0) stop("The beeps are expected to be uniformly distributed across all days.")

    # How many days of measurement?
    total.days <- total.notifications / beeps.per.day

    # Day.
    determined.day = NA

    # Offset days by -1.
    for(i in 0:(total.days - 1)) {
        # Possible notifications for current day.
        notifications.for.day <- (i * beeps.per.day + 1):((i + 1) * beeps.per.day)

        # Is the notification in this set?
        if(notification.number %in% notifications.for.day) {
            # Then set the day.
            determined.day <- i + 1

            # And break free.
            break
        }
    }

    return(determined.day)
}



# Construct beep numbers from notification numbers.
get.beep.number <- function(notification.number, beeps.per.day = 10, total.notifications = 50) {
    # Prevent notification overflow.
    if(notification.number > total.notifications) stop("Notification number cannot exceed maximum number of notifications.")

    # Mod.
    mod <- notification.number %% beeps.per.day

    # Determine the beep number.
    beep.number <- ifelse(mod > 0, mod, beeps.per.day)

    return(beep.number)
}
