module Geometry


open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open Math

// TODO: implement a transform class for using with rendered entities
type Transform() =
    let mutable position = Vec3D()
    let mutable rotation = Vec3D()
    let mutable scale = Vec3D(1.0, 1.0, 1.0)

    let mutable translation = Matrix4() // possibly unnecessary
    let mutable orientation = Matrix4()
    let mutable scaling = Matrix4() // possibly unnecessary

    // Possibly change so these are derived values
    // taken directly from the affine transform matrix
    member this.GetPosition 
        with get() = position 
    member this.GetRotation
        with get() = rotation 
    member this.GetScale
        with get() = scale

    member this.GetTranslation
        with get() = Matrix4.Translate(position)
    member this.GetOrientation
        with get() = 
            orientation <- Matrix4.Rotate(rotation.X, Vec3D(1.0,0.0,0.0))
            orientation <- Matrix4.Rotate(orientation, rotation.Y, Vec3D(0.0,1.0,0.0))
            orientation <- Matrix4.Rotate(orientation, rotation.Z, Vec3D(0.0,0.0,1.0))
            orientation
    member this.GetScaling
        with get() = Matrix4.Scale(scale)

    member this.Translate(x : float, y : float, z : float) =
        position <- position + Vec3D(x,y,z)
    member this.Translate(v : Vec3D) =
        position <- position + v
    member this.SetPosition(p : Vec3D) =
        position <- p
    member this.Rotate(x : float, y : float, z : float) =
        rotation <- rotation + Vec3D(x,y,z)
    member this.Rotate(r : Vec3D) =
        rotation <- rotation + r
    member this.SetRotation(r : Vec3D) =
        rotation <- r
    member this.Scale(x : float, y : float, z : float) =
        scale <- scale + Vec3D(x,y,z)
    member this.Scale(s : Vec3D) = 
        scale <- scale + s
    member this.Scale(s : float) =
        scale <- scale + Vec3D(s,s,s)

    // TODO

// TODO: abstract a little further and add a List<Vec2D> for storing all the vertices of the shape
[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable center = Vec2D(x0, y0)
    let mutable rot = 0.0 // in rads (maybe add method for deg)

    // eventually replace with a transform component
    member this.Center with get() = center and set newCenter = center <- newCenter
    member this.Rot with get() = rot and set rotVal = rot <- rotVal
    
    abstract Area : float with get
    abstract Perimeter : float with get
    abstract Name : string with get

    member this.Move(a : Vec2D) =
        center <- center + a

    abstract member Rotate : float -> unit
    default this.Rotate angle = rot <- rot + angle

    abstract member Draw : unit -> unit 
    default this.Draw() = ()

// this is just for testing
type Square(x0 : float, y0 : float) =
    inherit Shape2D(x0, y0)

    let mutable side = 1.0
    do printf("SQUAREEEEE\n")

    member this.Side with get() = side and set s = side <- s
    override this.Area with get() = side * side
    override this.Perimeter with get() = side * 4.0
    override this.Name with get() = "Square"

    override this.Draw() =
        let half = side * 0.5
        let topRight = Vec3D(this.Center.X + half, this.Center.Y + half, 4.0)
        let bottomRight = Vec3D(this.Center.X + half, this.Center.Y - half, 4.0)
        let topLeft = Vec3D(this.Center.X - half, this.Center.Y + half, 4.0)
        let bottomLeft = Vec3D(this.Center.X - half, this.Center.Y - half, 4.0)
        do printf "%A %A %A %A\n" bottomLeft bottomRight topRight topLeft
        GL.Begin(BeginMode.Polygon)
        GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(bottomLeft.X, bottomLeft.Y, bottomLeft.Z)
        GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(bottomRight.X, bottomRight.Y, bottomRight.Z)
        GL.Color3(1.f, 1.f, 0.f); GL.Vertex3(topRight.X, topRight.Y, topRight.Z)
        GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(topLeft.X, topLeft.Y, topLeft.Z)
        GL.End()
