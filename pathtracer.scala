import spatial.dsl._
import custom._
import _root_.spatial.node.Mux

import java.io._
import scala.util.Random

@spatial abstract class pathtracer(par0: scala.Int, par1: scala.Int, par2: scala.Int) extends SpatialApp {

  // type T = Float
  type T = FixPt[TRUE, _24, _8]
  type U = FixPt[TRUE, _32, _32]

  val pad: T = 0.to[T]
  val pad64: U = 0.to[U]

  // Create a vector from two points
  def createVec(point1: Point, point2: Point): Vec3 = {
    Vec3(point2.x - point1.x, point2.y - point1.y, point2.z - point1.z, pad)
  }

  // Find the min between two floats
  def min(a: Hit_Record, b: Hit_Record): Hit_Record = {
    mux(a.t <= b.t, a, b)
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
    (a.x_mag * b.x_mag) + (a.y_mag * b.y_mag) + (a.z_mag * b.z_mag)
  }

  // Get unit vector given a Vec3
  def get_unit_vec(a: Vec3): Vec3 = {
    val length_squared = ((a.x_mag * a.x_mag) + (a.y_mag * a.y_mag) + (a.z_mag * a.z_mag))
    Vec3((a.x_mag * a.x_mag) / length_squared, (a.y_mag * a.y_mag) / length_squared, (a.z_mag * a.z_mag) / length_squared, pad)
  }

  // Calculate a point along a ray
  def at(r: Ray, t: T): Point = {
    add_Point_Vec3(r.orig, mult_Vec3_scalar(r.dir, t))
  }

  def set_face_normal(r: Ray, outward_normal: Vec3): Vec3 = {
    mux(dot(r.dir, outward_normal) < 0, outward_normal, mult_Vec3_scalar(outward_normal, -1.to[T]))
  }

  def clamp(x: T, min: T, max: T): T = {
    mux(x < min, min, mux(x > max, max, x))
  }

  def fast_sqrt(n: T): T = {
    val guess = sqrt_approx(n)
    val iter0 = 0.5.to[T] * (guess + (n / guess))
    val iter1 = 0.5.to[T] * (iter0 + (n / iter0))
    val iter2 = 0.5.to[T] * (iter1 + (n / iter1))
    iter2
  }

  def main(args: Array[String]): Unit = {

    // Image
    val aspect_ratio = 16.to[T] / 9.to[T]
    val image_width = 400.to[Int]
    val image_height = 225.to[Int]
    // val samples_per_pixel = 1.to[Int]

    // World
    val object_count = 2.to[Int]

    val world = Array[Sphere](Sphere(0.0, 0.0, -1.0, 0.5),
      Sphere(0.0, -100.5, -1, 100.0))
    val world_d = DRAM[Sphere](object_count)
    setMem(world_d, world)


    // Set up DRAM for RGB values
    val pixel_colors_d = DRAM[RGB](image_height, image_width)

    // Set up dram for random values used by the accelerator
    // val rand_data = Array.tabulate[T](samples_per_pixel) {i => Random.nextFloat().to[T]}
    // val rand = DRAM[T](samples_per_pixel)
    // setMem(rand, rand_data)

    Accel {

      val C = image_width
      val R = image_height
      // val S = samples_per_pixel

      // Camera
      val viewport_height = 2.to[T]
      val viewport_width: T = aspect_ratio * viewport_height
      val focal_length = 1.to[T]

      val origin = Point(0.0, 0.0, 0.0, pad)
      val horizontal = Vec3(viewport_width, 0.to[T], 0.to[T], pad)
      val vertical = Vec3(0.to[T], viewport_height, 0.to[T], pad)
      val z_dir = Vec3(0.to[T], 0.to[T], focal_length, pad)
      val lower_left_corner = Point(0.to[T] - horizontal.x_mag / 2.to[T],
        0.to[T] - vertical.y_mag / 2.to[T],
        0.to[T] - z_dir.z_mag,
        pad)

      // Max Float used for calculating distance
      val max_float = 1048575.to[T] // 2^20 - 1

      // Prepare SRAM space for pixel colors
      // val pixel_colors_s = SRAM[RGB](image_height, image_width)

      // Load World Objects
      val world_s = SRAM[Sphere](object_count)
      world_s load world_d

      // Load Random T Values
      // val rand_s = SRAM[T](samples_per_pixel)
      // rand_s load rand

      // Render
      Foreach(0 until R by 1.to[Int] par par0) { j =>

        // Prepare SRAM for a row of pixels
        val pixel_colors_s = SRAM[RGB](image_width)

        Foreach(0 until C by 1.to[Int] par par1) { i =>
          // val pixel_color = Reg[RGB]
          // Reduce(pixel_color)(0 until S by 1.to[Int] par 2) { s =>

          val u = (i.to[T] + /*rand_s(s)*/ 0.5.to[T]) / (C - 1.to[Int]).to[T]
          val v = ((R - 1.to[Int] - j).to[T] + /*rand_s(s)*/ 0.5.to[T]) / (R - 1.to[Int]).to[T]

          val x_mag = lower_left_corner.x + (horizontal.x_mag * u)
          val y_mag = lower_left_corner.y + (vertical.y_mag * v)
          val z_mag = lower_left_corner.z

          val ray = Ray(origin, Vec3(x_mag, y_mag, z_mag, pad))

          // length squared of Ray direction
          val a = (x_mag * x_mag) + (y_mag * y_mag) + (z_mag * z_mag)

          val closest_hit = Reg[Hit_Record]

          Reduce(closest_hit)(0 until 2 par par2) { obj =>
            val center = Point(world_s(obj).center_x, world_s(obj).center_y, world_s(obj).center_z, pad)
            val oc = createVec(center, ray.orig)

            // Vector dot product between oc and ray direction
            val half_b = dot(oc, ray.dir)

            // oc length squared - sphere radius squared
            val c = ((oc.x_mag * oc.x_mag) + (oc.y_mag * oc.y_mag) + (oc.z_mag * oc.z_mag)) - (world_s(obj).radius * world_s(obj).radius)

            val discriminant = (half_b * half_b) - (a * c)
            val sqrtd = mux(discriminant > 0, fast_sqrt(discriminant), 0)
            val root0 = mux(discriminant > 0, ((half_b * -1.to[T]) - sqrtd) / a, 0)
            val root1 = mux(discriminant > 0, ((half_b * -1.to[T]) + sqrtd) / a, 0)

            val contact_point0 = at(ray, root0)
            val outward_normal0 = set_face_normal(ray, div_Vec3_scalar(createVec(center, contact_point0), world_s(obj).radius))
            val hit_0 = Hit_Record(contact_point0, outward_normal0, root0, pad64, pad64, pad64, pad)

            val contact_point1 = at(ray, root1)
            val outward_normal1 = set_face_normal(ray, div_Vec3_scalar(createVec(center, contact_point1), world_s(obj).radius))
            val hit_1 = Hit_Record(contact_point1, outward_normal1, root1, pad64, pad64, pad64, pad)

            val empty_record = Hit_Record(Point(0.to[T], 0.to[T], 0.to[T], pad),
              Vec3(0.to[T], 0.to[T], 0.to[T], pad),
              max_float,
              pad64, pad64, pad64, pad)

            mux(discriminant > 0, mux(root0 > 0, hit_0, mux(root1 > 0, hit_1, empty_record)), empty_record)

          } {
            min(_, _)
          }

          val unit_direction = get_unit_vec(ray.dir)
          val t = 0.5.to[T] * (unit_direction.y_mag + 1.to[T])
          val miss_color = add_RGB(mult_RGB_scalar(RGB(1.to[T], 1.to[T], 1.to[T], pad), 1.to[T] - t),
            mult_RGB_scalar(RGB(0.5.to[T], 0.7.to[T], 1.to[T], pad), t))

          val hit_color = mult_RGB_scalar(add_RGB_Vec3(RGB(1.to[T], 1.to[T], 1.to[T], pad), closest_hit.value.normal), 0.5.to[T])

          val pixel_color = mux(closest_hit.value.t < max_float, hit_color, miss_color)
          // }{add_RGB(_,_)}

          // val scale = 1.to[T] / samples_per_pixel.to[T]
          val scaled_r = clamp(pixel_color.red, 0.to[T], 0.999.to[T]) * 256.to[T]
          val scaled_g = clamp(pixel_color.green, 0.to[T], 0.999.to[T]) * 256.to[T]
          val scaled_b = clamp(pixel_color.blue, 0.to[T], 0.999.to[T]) * 256.to[T]
          pixel_colors_s(i) = RGB(scaled_r, scaled_g, scaled_b, pad)
        }
        pixel_colors_d(j, 0::image_width) store pixel_colors_s
      }
    }
    val result = getMatrix(pixel_colors_d)
    writeCSV2D(result, "image.csv")
  }
}

object test0 extends pathtracer(1, 3, 1)
object test1 extends pathtracer(2, 1, 1)