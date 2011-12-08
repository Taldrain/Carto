class point3 a b c =
object
  val mutable x: float = a
  val mutable y: float = b
  val mutable z: float = c

  method getX = x
  method getY = y
  method getZ = z

  method setX x' = x <- x'
  method setY y' = y <- y'
  method setZ z' = z <- z'
end

