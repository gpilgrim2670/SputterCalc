#' Dep_Polar
#'
#' @description Calculates amount of material deposited for a rotating system of given geometry and deposition rate

#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @param Rate sputter power - a constant
#' @param Epsilon a sputter rate scaling factor, usually 1
#' @param Disc_1 relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity
#' @param Disc_2 relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity
#' @param Rad_1 fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)
#' @param Rad_2 fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symetric)
#' @param Ins_1 distance the gun has been inserted into chamber, measured from inside of disc 2 to the center of the gun knuckle
#' @param Fing_1 distance from the knuckle to the gun tip
#' @param Knuc_1 angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18
#' @param Rot_1 angle (in radians) at which the gun is rotated, min is 0, max is 2*pi
#' @param Interval the size of each step in a numeric solution.  Larger intervals will produce courser solutions, but will be faster.  Smaller intervals will produce more detailed solutions but will be more computationally expensive


Dep_Polar <- function(Rate,
                      Epsilon = 1,
                      Disc_1 = 0,
                      Disc_2 = 0,
                      Rad_1 = 1.4375,
                      Rad_2 = 3,
                      Ins_1 = 2.375,
                      Fing_1 = 4.8125,
                      Knuc_1 = 0.87266,
                      Rot_1 = 0.17453,
                      Interval = 0.2) {
  Dep_Polar <- (
    Rate * HLen(
      Disc_1 = Disc_1,
      Disc_2 = Disc_2,
      Rad_1 = Rad_1,
      Rad_2 = Rad_2,
      Fing_1 = Fing_1,
      Knuc_1 = Knuc_1,
      Rot_1 = Rot_1
    ) * ((
      SAngle(
        Disc_1 = Disc_1,
        Disc_2 = Disc_2,
        Rad_1 = Rad_1,
        Rad_2 = Rad_2,
        Ins_1 = Ins_1,
        Fing_1 = Fing_1,
        Knuc_1 = Knuc_1,
        Rot_1 = Rot_1,
        Interval = Interval
      ) ^ 2 + RLen(
        Disc_1 = Disc_1,
        Disc_2 = Disc_2,
        Rad_1 = Rad_1,
        Rad_2 = Rad_2,
        Ins_1 = Ins_1,
        Fing_1 = Fing_1,
        Knuc_1 = Knuc_1,
        Rot_1 = Rot_1
      ) ^ 2 - LAngle(
        Disc_1 = Disc_1,
        Disc_2 = Disc_2,
        Rad_1 = Rad_1,
        Rad_2 = Rad_2,
        Ins_1 = Ins_1,
        Fing_1 = Fing_1,
        Knuc_1 = Knuc_1,
        Rot_1 = Rot_1,
        Interval = Interval
      ) ^ 2
    ) / (
      2 * SAngle(
        Disc_1 = Disc_1,
        Disc_2 = Disc_2,
        Rad_1 = Rad_1,
        Rad_2 = Rad_2,
        Ins_1 = Ins_1,
        Fing_1 = Fing_1,
        Knuc_1 = Knuc_1,
        Rot_1 = Rot_1,
        Interval = Interval
      ) * RLen(
        Disc_1 = Disc_1,
        Disc_2 = Disc_2,
        Rad_1 = Rad_1,
        Rad_2 = Rad_2,
        Ins_1 = Ins_1,
        Fing_1 = Fing_1,
        Knuc_1 = Knuc_1,
        Rot_1 = Rot_1
      )
    )) ^
      Epsilon
  ) / SAngle(
    Disc_1 = Disc_1,
    Disc_2 = Disc_2,
    Rad_1 = Rad_1,
    Rad_2 = Rad_2,
    Ins_1 = Ins_1,
    Fing_1 = Fing_1,
    Knuc_1 = Knuc_1,
    Rot_1 = Rot_1,
    Interval = Interval
  ) ^ 3

  return(Dep_Polar)
}
