module Geometry

type Vec2D (x : float, y : float) =
    let mag = sqrt(x * x + y * y)
    member this.X = x
    member this.Y = y
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

type Vec3D(x : float, y : float, z : float) =
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

