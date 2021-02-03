package basics

object ClassesAndTraits {

  sealed trait Shape2D extends Located2D with Bounded2D with Measurable2D with Movable2D

  sealed trait Shape3D extends Located3D with Bounded3D with Measurable3D with Movable3D

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Measurable2D {
    def area: Double
  }

  sealed trait Measurable3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(
      x= x + dx,
      y = y + dy
    )

    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double    = centerX
    override def y: Double    = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle = Circle(
      centerX = centerX + dx,
      centerY = centerY + dy,
      radius = radius
    )

    override def area: Double = math.Pi * math.pow(radius, 2)
  }

  final case class Rectangle(leftBottomX: Double, leftBottomY: Double, height: Double, width: Double) extends Shape2D {
    override def minX: Double = leftBottomX
    override def maxX: Double = leftBottomX + width
    override def minY: Double = leftBottomY
    override def maxY: Double = leftBottomY + height
    override def x: Double = (leftBottomX + width) / 2
    override def y: Double = (leftBottomY + height) / 2

    override def area: Double = height * width

    override def move(dx: Double, dy: Double): Shape2D = Rectangle(
      leftBottomX = leftBottomX + dx,
      leftBottomY = leftBottomY + dy,
      height = height,
      width = width
    )
  }

  final case class Triangle(p1: Point2D, p2: Point2D, p3: Point2D) extends Shape2D {

    private val points = List(p1, p2 ,p3)

    override def x: Double = ???
    override def y: Double = ???
    override def minX: Double = points.minBy(_.x).x
    override def maxX: Double = points.maxBy(_.x).x
    override def minY: Double = points.minBy(_.y).y
    override def maxY: Double = points.maxBy(_.y).y

    override def move(dx: Double, dy: Double): Triangle = Triangle(
      p1 = Point2D(p1.x + dx, p1.y + dy),
      p2 = Point2D(p2.x + dx, p2.y + dy),
      p3 = Point2D(p3.x + dx, p3.y + dy)
    )

    override def area: Double = ???
  }

  final case class Square(centerX: Double, centerY: Double, size: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - size / 2
    override def maxX: Double = centerX + size / 2
    override def minY: Double = centerY - size / 2
    override def maxY: Double = centerY - size / 2

    override def area: Double = math.pow(size, 2)

    override def move(dx: Double, dy: Double): Square = Square(
      centerX = centerX + dx,
      centerY = centerY + dy,
      size = size
    )
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(
      x = x + dx,
      y = y + dy,
      z = z + dz
    )
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ


    override def surfaceArea: Double = 4 * math.Pi * math.pow(radius, 2)

    override def volume: Double = 4 * math.Pi * math.pow(radius, 3) / 3

    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(
      centerX = centerX + dx,
      centerY = centerY + dy,
      centerZ = centerZ + dz,
      radius = radius
    )
  }

  final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3D {
    override def minX: Double = centerX - width / 2
    override def maxX: Double = centerX + width / 2
    override def minY: Double = centerY - height / 2
    override def maxY: Double = centerY + height / 2
    override def minZ: Double = centerZ - depth / 2
    override def maxZ: Double = centerZ - depth / 2
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def surfaceArea: Double = 2 * (width * height + width * depth + height * depth)

    override def volume: Double = width * height * depth

    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(
      centerX = centerX + dx,
      centerY = centerY + dy,
      centerZ = centerZ + dz,
      width = width,
      height = height,
      depth = depth
    )
  }
}
