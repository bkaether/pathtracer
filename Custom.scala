import spatial.dsl._

package custom {

  @struct class RGB(
                     red: Float,
                     green: Float,
                     blue: Float,
                     pad: Float
                   )

  @struct class Point(
                       x: Float,
                       y: Float,
                       z: Float,
                       pad: Float
                     )

  @struct class Vec3(
                      x_mag: Float,
                      y_mag: Float,
                      z_mag: Float,
                      pad: Float
                    )

  @struct class Ray(
                   orig: Point,
                   dir: Vec3,
                   )

  @struct class Sphere(
                      center_x: Float,
                      center_y: Float,
                      center_z: Float,
                      radius: Float,
                      )

  @struct class Hit_Record(
                          p: Point,
                          normal: Vec3,
                          t: Float,
                          pad0: FixPt[TRUE, _32, _32],
                          pad1: FixPt[TRUE, _32, _32],
                          pad2: FixPt[TRUE, _32, _32],
                          pad3: Float
                          )

}