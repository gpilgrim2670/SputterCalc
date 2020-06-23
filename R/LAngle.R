#' LAngle
#'
#' @description calculates the angle between L and the platen normal
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @param Disc_1 relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity
#' @param Disc_2 relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity
#' @param Rad_1 fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)
#' @param Rad_2 fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symmetric)
#' @param Ins_1 distance the gun has been inserted into chamber, measured from inside of disc 2 to the center of the gun knuckle
#' @param Fing_1 distance from the knuckle to the gun tip
#' @param Knuc_1 angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18
#' @param Rot_1 angle (in radians) at which the gun is rotated, min is 0, max is 2*pi
#' @param Interval the size of each step in a numeric solution.  Larger intervals will produce courser solutions, but will be faster.  Smaller intervals will produce more detailed solutions but will be more computationally expensive

LAngle <- function(Disc_1 = -5,
                   Disc_2 = -33,
                   Rad_1 = 1.4375,
                   Rad_2 = 3,
                   Ins_1 = 2.375,
                   Fing_1 = 4.8125,
                   Knuc_1 = 0.610865,
                   Rot_1 = 0.785398,
                   Interval = 0.2) {

  # Converting from degrees to radians
  Disc_1 <- Disc_1 * pi / 180
  Disc_2 <- Disc_2 * pi / 180

  # Unknown constant from Michael
  F_30 <-
    (8 + Rad_1 * cos(Disc_1) + Rad_2 * sin(Disc_2)) / (Fing_1 * sin(Knuc_1) * cos(Rot_1))

  # Platen intercept point, n and q only
  n <-
    (-Rad_1 * sin(Disc_1) + Rad_2 * cos(Disc_2)) - F_30 * (Fing_1 * sin(Knuc_1) * sin(Rot_1))
  q <- 12 - Ins_1 - Fing_1 * cos(Knuc_1) * F_30

  # Points n and q in radians

  P1Radial <- sqrt(n ^ 2 + q ^ 2)
  P1Angle <- atan2(q, n)

  # Range of angles to sweep, defined by Interval
  Angle <- seq(0, 49, by = Interval * 5) * (2 * pi / 50/(Interval*5))

  # Range of radius lengths (of platen) to sweep, defined by Interval
  Radius <- seq(0, 6, by = Interval)

  # Grid of all combinations of Angle and Radius
  gr <- expand.grid(Angle, Radius)

  # Calculates the L angle.  Var1 is Radius from above, Var1 is Angle
  LAngle <-
    sqrt((P1Radial * sin(P1Angle) - gr$Var2 * sin(gr$Var1)) ^ 2 + (P1Radial *
                                                                     cos(P1Angle) - gr$Var2 * cos(gr$Var1)) ^ 2)

  return(LAngle)
}
