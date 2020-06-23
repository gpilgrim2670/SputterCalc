#' HLen
#'
#' @description distance between the chamber origin and the platen (height of platen relative to gun)
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @param Disc_1 relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity
#' @param Disc_2 relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity
#' @param Rad_1 fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)
#' @param Rad_2 fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symmetric)
#' @param Fing_1 distance from the knuckle to the gun tip
#' @param Knuc_1 angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18
#' @param Rot_1 angle (in radians) at which the gun is rotated, min is 0, max is 2*pi
#'

HLen <-
  function(Disc_1 = -5,
           Disc_2 = -33,
           Rad_1 = 1.4375,
           Rad_2 = 3,
           Fing_1 = 4.8125,
           Knuc_1 = 0.610865,
           Rot_1 = 0.785398) {
    # Converting from degrees to radians
    Disc_1 <- Disc_1 * pi / 180
    Disc_2 <- Disc_2 * pi / 180

    # Target center point, y only
    Y <-
      Rad_1 * cos(Disc_1) + Rad_2 * sin(Disc_2) - Fing_1 * sin(Knuc_1) * cos(Rot_1)

    # Platen intercept point, p only
    p <- -8

    # Distance between target center point and platen center point
    H_length <- Y - p
    return(H_length)
  }
