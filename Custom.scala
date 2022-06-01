import spatial.dsl._

package custom {

  @struct class RGB(
                     red: FixPt[TRUE, _24, _8],
                     green: FixPt[TRUE, _24, _8],
                     blue: FixPt[TRUE, _24, _8],
                     pad: FixPt[TRUE, _24, _8]
                   )

  @struct class Point(
                       x: FixPt[TRUE, _24, _8],
                       y: FixPt[TRUE, _24, _8],
                       z: FixPt[TRUE, _24, _8],
                       pad: FixPt[TRUE, _24, _8]
                     )

  @struct class Vec3(
                      x_mag: FixPt[TRUE, _24, _8],
                      y_mag: FixPt[TRUE, _24, _8],
                      z_mag: FixPt[TRUE, _24, _8],
                      pad: FixPt[TRUE, _24, _8]
                    )

  @struct class Ray(
                   orig: Point,
                   dir: Vec3,
                   )

  @struct class Sphere(
                      center_x: FixPt[TRUE, _24, _8],
                      center_y: FixPt[TRUE, _24, _8],
                      center_z: FixPt[TRUE, _24, _8],
                      radius: FixPt[TRUE, _24, _8],
                      )

  @struct class Hit_Record(
                          p: Point,
                          normal: Vec3,
                          t: FixPt[TRUE, _24, _8],
                          pad0: FixPt[TRUE, _32, _32],
                          pad1: FixPt[TRUE, _32, _32],
                          pad2: FixPt[TRUE, _32, _32],
                          pad3: FixPt[TRUE, _24, _8]
                          )

}