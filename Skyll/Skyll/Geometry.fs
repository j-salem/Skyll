module Geometry

type Vec2D (x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mag = sqrt(x * x + y * y)
    member this.X with get() = x and set xVal = x <- xVal
    member this.Y with get() = y and set yVal = y <- yVal
    member this.Magnitude = mag
    member this.Scale(s) =
        Vec2D(x * s, y * s)
    member this.Dot(a : Vec2D) =
        this.X * a.X + this.Y * a.Y
    member this.Normalized() =
        this.Scale(1.0 / mag)
    member this.Distance(a : Vec2D) =
        let dx = this.X - a.X
        let dy = this.Y - a.Y
        sqrt(dx * dx + dy * dy)
    static member (+) (a : Vec2D, b : Vec2D) = 
        Vec2D(a.X + b.X, a.Y + b.Y)
    static member (-) (a : Vec2D, b : Vec2D) =
        Vec2D(a.X - b.X, a.Y - b.Y)
    static member (~-) (a : Vec2D) =
        Vec2D(-a.X, -a.Y)
    override this.ToString() = 
        "{" + this.X.ToString() + ", " + this.Y.ToString() + "}"

type Vec3D(x0 : float, y0 : float, z0 : float) =
    let mutable x, y, z = x0, y0, z0
    let mag = sqrt(x * x + y * y + z * z)
    member this.X = x
    member this.Y = y
    member this.Z = z
    member this.Magnitude = mag
    member this.Scale(s) =
        Vec3D(x * s, y * s, z * s)
    member this.Dot(a : Vec3D) =
        this.X * a.X + this.Y * a.Y + this.Z * a.Z
    member this.Cross(a : Vec3D) =
        let x' = this.Y * a.Z - this.Z * a.Y
        let y' = this.Z * a.X - this.X * a.Z
        let z' = this.X * a.Y - this.Y * a.X
        Vec3D(x', y', z')
    member this.Normalized() =
        this.Scale(1.0 / mag)
    member this.Distance(a : Vec3D) =
        let dx = this.X - a.X
        let dy = this.Y - a.Y
        let dz = this.Z - a.Z
        sqrt(dx * dx + dy * dy + dz * dz)
    static member (+) (a : Vec3D, b : Vec3D) = 
        Vec3D(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member (-) (a : Vec3D, b : Vec3D) =
        Vec3D(a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    static member (~-) (a : Vec3D) =
        Vec3D(-a.X, -a.Y, -a.Z)
    override this.ToString() = 
        "{" + this.X.ToString() + ", " + this.Y.ToString() + ", " + this.Z.ToString() + "}"

type Point2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    member this.X with get() = x and set xVal = x <- xVal
    member this.Y with get() = y and set yVal = y <- yVal
    member this.Move(a : Vec2D) =
        Point2D(this.X + a.X, this.Y + a.Y)
    member this.VectorFrom(a : Point2D) = 
        Vec2D(this.X - a.X, this.Y - a.Y)
    member this.VectorTo(a : Point2D) =
        Vec2D(a.X - this.X, a.Y - this.Y)
    override this.ToString() =
        "{" + this.X.ToString() + ", " + this.Y.ToString() + "}"

type Point3D(x0 : float, y0 : float, z0 : float) =
    let mutable x, y, z = x0, y0, z0
    member this.X with get() = x and set xVal = x <- xVal
    member this.Y with get() = y and set yVal = y <- yVal
    member this.Z with get() = z and set zVal = z <- zVal
    member this.Move(a : Vec3D) =
        Point3D(this.X + a.X, this.Y + a.Y, this.Z + a.Z)
    member this.VectorFrom(a : Point3D) = 
        Vec3D(this.X - a.X, this.Y - a.Y, this.Z - a.Z)
    member this.VectorTo(a : Point3D) =
        Vec3D(a.X - this.X, a.Y - this.Y, a.Z - this.Z)
    override this.ToString() =
        "{" + this.X.ToString() + ", " + this.Y.ToString() + ", " + this.Z.ToString() + "}"

// TODO: abstract a little further and add a List<Vec2D> for storing all the vertices of the shape
[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable center = Point2D(x0, y0)
    let mutable rot = 0.0 // in rads (maybe add method for deg)

    // eventually replace with a transform component
    member this.Center with get() = center and set newCenter = center <- newCenter
    member this.Rot with get() = rot and set rotVal = rot <- rotVal
    
    abstract Area : float with get
    abstract Perimeter : float with get
    abstract Name : string with get

    member this.Move(a : Vec2D) =
        center.Move(a)

    abstract member Rotate : float -> unit
    default this.Rotate angle = rot <- rot + angle

    abstract member Draw : unit 
    default this.Draw = ()