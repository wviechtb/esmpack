# Construct beep numbers from notification numbers.
get.beep.number <- function(notification.number, beeps.per.day = 10, total.notifications = 50) {
    # Prevent notification overflow.
    if(notification.number > total.notifications) stop("`notification.number` cannot exceed maximum number of notifications.")

    # Mod.
    mod <- notification.number %% beeps.per.day

    # Determine the beep number.
    beep.number <- ifelse(mod > 0, mod, beeps.per.day)

    return(beep.number)
}
