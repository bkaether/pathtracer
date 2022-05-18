import spatial.dsl._
import custom._

import java.io._
import scala.util.Random

@spatial object pathtracer extends SpatialApp {

  type T = FixPt[TRUE, _16, _16]
  type U = FixPt[TRUE, _32, _32]
  // type V = FixPt[TRUE, _64, _64]

  val pad: T = 0.to[T]
  val pad64: U = 0.to[U]
  // val pad128: V = 0.to[V]

  // Create a vector from two points
  def createVec(point1: Point, point2: Point): Vec3 = {
    Vec3(point2.x - point1.x, point2.y - point1.y, point2.z - point1.z, pad)
  }

  // Find the min between two floats
  def min(a: Hit_Record, b: Hit_Record): Hit_Record = {
    if (a.t <= b.t) a else b
  }

  // Add 2 RGB structs
  def add_RGB(a: RGB, b: RGB): RGB = {
    RGB(a.red + b.red, a.green + b.green, a.blue + b.blue, pad)
  }

  // Add Point with Vec3
  def add_Point_Vec3(a: Point, b: Vec3): Point = {
    Point(a.x + b.x_mag, a.y + b.y_mag, a.z + b.z_mag, pad)
  }

  // Add RGB with a Vec3
  def add_RGB_Vec3(a: RGB, b: Vec3): RGB = {
    RGB(a.red + b.x_mag, a.green + b.y_mag, a.blue + b.z_mag, pad)
  }

  // Multiply RGB by a scalar
  def mult_RGB_scalar(a: RGB, b: T): RGB = {
    RGB(a.red * b, a.green * b, a.blue * b, pad)
  }

  // Divide Vec3 by a scalar
  def div_Vec3_scalar(a: Vec3, b: T): Vec3 = {
    Vec3(a.x_mag / b, a.y_mag / b, a.z_mag / b, pad)
  }

  // Multiply a Vec3 by a scalar
  def mult_Vec3_scalar(a: Vec3, b: T): Vec3 = {
    Vec3(a.x_mag * b, a.y_mag * b, a.z_mag * b, pad)
  }

  // Take dot product between two vectors
  def dot(a: Vec3, b: Vec3): T = {
    (a.x_mag*b.x_mag) + (a.y_mag*b.y_mag) + (a.z_mag*b.z_mag)
  }

  // Get unit vector given a Vec3
  def get_unit_vec(a: Vec3): Vec3 = {
    val length_squared = (a.x_mag*a.x_mag) + (a.y_mag*a.y_mag) + (a.z_mag*a.z_mag)
    Vec3((a.x_mag * a.x_mag) / length_squared, (a.y_mag * a.y_mag) / length_squared, (a.z_mag * a.z_mag) / length_squared, pad)
  }

  // Calculate a point along a ray
  def at(r: Ray, t: T): Point = {
    add_Point_Vec3(r.orig, mult_Vec3_scalar(r.dir, t))
  }

  def set_face_normal(r: Ray, outward_normal: Vec3): Vec3 = {
    val front_face = dot(r.dir, outward_normal) < 0
    if (front_face) outward_normal else mult_Vec3_scalar(outward_normal, -1.to[T])
  }

  def clamp(x: T, min: T, max: T): T = {
    if (x < min) min
    else if (x > max) max
    else x
  }

  def main(args: Array[String]): Unit = {

    val pw = new PrintWriter(new File("gen/pathtracer/image.ppm"))

    // Image
    val aspect_ratio = 16.to[T] / 9.to[T]
    val image_width = 400.to[Int]
    val image_height = 225.to[Int]
    val samples_per_pixel = 100.to[Int]

    val C = ArgIn[Int]
    val R = ArgIn[Int]
    val S = ArgIn[Int]
    setArg(C, image_width)
    setArg(R, image_height)
    setArg(S, samples_per_pixel)

    // World
    val object_count = 2.to[Int]

    val world = Array[Sphere](Sphere(Point(0.0, 0.0, -1.0, pad), 0.5, pad64, pad),
                              Sphere(Point(0.0, -100.5, -1, pad), 100.0, pad64, pad))
    val world_d = DRAM[Sphere](object_count)
    setMem(world_d, world)


    // Camera
    val viewport_height = 2.to[T]
    val viewport_width: T = aspect_ratio * viewport_height
    val focal_length = 1.to[T]

    val origin = ArgIn[Point]
    setArg(origin, Point(0.0, 0.0, 0.0, pad))

    val horizontal = ArgIn[Vec3]
    setArg(horizontal, Vec3(viewport_width, 0.to[T], 0.to[T], pad))

    val vertical = ArgIn[Vec3]
    setArg(vertical, Vec3(0.to[T], viewport_height, 0.to[T], pad))

    val z_dir = ArgIn[Vec3]
    setArg(z_dir, Vec3(0.to[T], 0.to[T], focal_length, pad))

    val lower_left_corner = ArgIn[Point]
    setArg(lower_left_corner, Point(0.to[T] - getArg(horizontal).x_mag / 2.to[T],
                                    0.to[T] - getArg(vertical).y_mag / 2.to[T],
                                    0.to[T] - getArg(z_dir).z_mag,
                                    pad))

    // Max T used for calculating distance
    val max_float = ArgIn[T]
    setArg(max_float, Float.PositiveInfinity.to[T])

    // Set up DRAM for RGB values
    val pixel_colors_d = DRAM[RGB](R, C)

    // Set up dram for random values used by the accelerator
    val rand_data = Array.tabulate[T](getArg(S)) {i => Random.nextFloat().to[T]}
    val rand = DRAM[T](getArg(S))
    setMem(rand, rand_data)


    Accel {

      // Prepare SRAM space for pixel colors
      val pixel_colors_s = SRAM[RGB](image_height, image_width)

      // Load World Objects
      val world_s = SRAM[Sphere](object_count)
      world_s load world_d

      // Load Random T Values
      val rand_s = SRAM[T](samples_per_pixel)
      rand_s load rand

      // Render
      Foreach(1 until (R + 1.to[Int]) par 4) { j =>
        Foreach(0 until C by 1.to[Int] par 4) { i =>
          val pixel_color = Reg[RGB]
          Reduce(pixel_color)(0 until S by 1.to[Int]) { s =>

            val u = (i.to[T] + rand_s(s)) / (C - 1.to[Int]).to[T]
            val v = ((R - j).to[T] + rand_s(s)) / (R - 1.to[Int]).to[T]

            val x_mag = lower_left_corner.x + (horizontal.x_mag * u)
            val y_mag = lower_left_corner.y + (vertical.y_mag * v)
            val z_mag = lower_left_corner.z
            val ray = Ray(origin, Vec3(x_mag, y_mag, z_mag, pad))

            val closest_hit = Reg[Hit_Record]

            Reduce(closest_hit)(0 until 2 par 2) { obj =>
              val oc = createVec(world_s(obj).center, ray.orig)

              // length squared of Ray direction
              val a = (ray.dir.x_mag * ray.dir.x_mag) + (ray.dir.y_mag * ray.dir.y_mag) + (ray.dir.z_mag * ray.dir.z_mag)

              // Vector dot product between oc and ray direction
              val half_b = dot(oc, ray.dir)

              // oc length squared / sphere radius squared
              val c = ((oc.x_mag * oc.x_mag) + (oc.y_mag * oc.y_mag) + (oc.z_mag * oc.z_mag)) - (world_s(obj).radius * world_s(obj).radius)

              val discriminant = (half_b * half_b) - (a * c)
              if (discriminant > 0) {
                val sqrtd = sqrt_approx(discriminant)
                val root0 = ((half_b * -1.to[T]) - sqrtd) / a
                val root1 = ((half_b * -1.to[T]) + sqrtd) / a
                if (root0 > 0) {
                  val contact_point = at(ray, root0)
                  val outward_normal = set_face_normal(ray, div_Vec3_scalar(createVec(world_s(obj).center, contact_point), world_s(obj).radius))
                  Hit_Record(contact_point, outward_normal, root0, pad64, pad64, pad64, pad)
                } else if (root1 > 0) {
                  val contact_point = at(ray, root1)
                  val outward_normal = set_face_normal(ray, div_Vec3_scalar(createVec(world_s(obj).center, contact_point), world_s(obj).radius))
                  Hit_Record(contact_point, outward_normal, root1, pad64, pad64, pad64, pad)
                } else {
                  Hit_Record(Point(0.to[T], 0.to[T], 0.to[T], pad),
                             Vec3(0.to[T], 0.to[T], 0.to[T], pad),
                             max_float,
                             pad64, pad64, pad64, pad)
                }
              } else {
                Hit_Record(Point(0.to[T], 0.to[T], 0.to[T], pad),
                           Vec3(0.to[T], 0.to[T], 0.to[T], pad),
                           max_float,
                           pad64, pad64, pad64, pad)
              }
            }{min(_,_)}

            if (closest_hit.t < max_float) {
              mult_RGB_scalar(add_RGB_Vec3(RGB(1.to[T], 1.to[T], 1.to[T], pad), closest_hit.normal), 0.5.to[T])
            } else {
              val unit_direction = get_unit_vec(ray.dir)
              val t = 0.5.to[T] * (unit_direction.y_mag + 1.to[T])
              add_RGB(mult_RGB_scalar(RGB(1.to[T], 1.to[T], 1.to[T], pad), 1.to[T] - t),
                      mult_RGB_scalar(RGB(0.5.to[T], 0.7.to[T], 1.to[T], pad), t))
            }
          }{add_RGB(_,_)}
          pixel_colors_s(j, i) = pixel_color
        }
      }
      pixel_colors_d store pixel_colors_s
    }
    val result = getMatrix(pixel_colors_d)

    Foreach(image_height by 1, image_width by 1) { (i, j) =>
      val scale = 1.to[T] / samples_per_pixel.to[T]
      val scaled_r = clamp(result(i, j).red * scale, 0.to[T], 0.999.to[T])
      val scaled_g = clamp(result(i, j).green * scale, 0.to[T], 0.999.to[T])
      val scaled_b = clamp(result(i, j).blue * scale, 0.to[T], 0.999.to[T])
      pw.print(256.to[T] * scaled_r)
      pw.print(" ")
      pw.print(256.to[T] * scaled_g)
      pw.print(" ")
      pw.print(256.to[T] * scaled_b)
      pw.print("\n")
    }

    pw.close()
  }
}