import spatial.dsl._

package custom {

  @struct class RGB(
                     red: FixPt[TRUE, _16, _16],
                     green: FixPt[TRUE, _16, _16],
                     blue: FixPt[TRUE, _16, _16],
                     pad: FixPt[TRUE, _16, _16]
                   )

  @struct class Point(
                       x: FixPt[TRUE, _16, _16],
                       y: FixPt[TRUE, _16, _16],
                       z: FixPt[TRUE, _16, _16],
                       pad: FixPt[TRUE, _16, _16]
                     )

  @struct class Vec3(
                      x_mag: FixPt[TRUE, _16, _16],
                      y_mag: FixPt[TRUE, _16, _16],
                      z_mag: FixPt[TRUE, _16, _16],
                      pad: FixPt[TRUE, _16, _16]
                    )

  @struct class Ray(
                   orig: Point,
                   dir: Vec3,
                   )

  @struct class Sphere(
                      center: Point,
                      radius: FixPt[TRUE, _16, _16],
                      pad0: FixPt[TRUE, _32, _32],
                      pad1: FixPt[TRUE, _16, _16]
                      )

  @struct class Hit_Record(
                          p: Point,
                          normal: Vec3,
                          t: FixPt[TRUE, _16, _16],
                          pad0: FixPt[TRUE, _32, _32],
                          pad1: FixPt[TRUE, _32, _32],
                          pad2: FixPt[TRUE, _32, _32],
                          pad3: FixPt[TRUE, _16, _16]
                          )

}