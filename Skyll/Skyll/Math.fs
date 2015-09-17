﻿module Math

// TODO: change internal representation to an array
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
    new() = Vec2D(0.0, 0.0)

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
    new() = Vec3D(0.0, 0.0, 0.0)

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
    new() = Point2D(0.0, 0.0)

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
    new() = Point3D(0.0, 0.0, 0.0)

type Matrix(m0 : int, n0 : int) =
    let n, m = n0, m0
    let mutable values = [| for i in 1..m -> [| for j in 1..n -> 0.0 |] |]

    member this.Item
        with get(i, j) = 
            assert(i < m && j < n)
            values.[i].[j]
        and set (i, j) value = 
            assert(i < m && j < n)
            values.[i].[j] <- value
    member this.Item
        with get(i) = 
            assert(i < m)
            values.[i]
        and set (i) value = 
            assert(i < m)
            values.[i] <- value
    member this.M with get() : int = m
    member this.N with get() : int = n
    member this.ScalarMult(s) =
        let newMat = Matrix(m,n)
        for i = 0 to m - 1 do
            for j = 0 to n - 1 do
                newMat.[i,j] <- this.[i,j] * s
        newMat
    member this.Transpose() =
        let newMat = Matrix(n,m)
        for i = 0 to m - 1 do
            for j = 0 to n - 1 do
                newMat.[j,i] <- this.[i,j]
        newMat
    member this.Trace() =
        assert(m = n) // possibly break this out into a subclass for square matrices
        let mutable trace = 0.0
        for i = 0 to m - 1 do
            trace <- trace + this.[i,i]
        trace

    static member (+) (a : Matrix, b : Matrix) =
        assert (a.N = b.N && a.M = b.M)
        let c = Matrix(a.M, a.N)
        for i = 0 to a.M - 1 do
            for j = 0 to a.N - 1 do
                c.[i,j] <- a.[i,j] + b.[i,j]
        c
    static member (-) (a : Matrix, b : Matrix) =
        assert (a.N = b.N && a.M = b.M)
        let c = Matrix(a.M, a.N)
        for i = 0 to a.M - 1 do
            for j = 0 to a.N - 1 do
                c.[i,j] <- a.[i,j] - b.[i,j]
        c
    // NOT the most efficient, but we should only be dealing with 4x4 matrices at most
    static member (*) (a : Matrix, b : Matrix) = 
        assert(a.N = b.M)
        let newMat = Matrix(a.M,b.N)
        for i = 0 to a.M - 1 do
            for j = 0 to b.N - 1 do
                let mutable sum = 0.0
                for k = 0 to a.N - 1 do
                    sum <- sum + a.[i,k] * b.[k,j]
                newMat.[i,j] <- sum
        newMat
    static member Identity(n) =
        let newMat = Matrix(n, n)
        for i = 0 to n - 1 do
            newMat.[i,i] <- 1.0
        newMat
    override this.ToString() =
        let mutable ret = ""
        ret <- ret + "{\n"
        for i = 0 to m - 1 do
            ret <- ret + "\t"
            for j = 0 to n - 1 do
                ret <- ret + this.[i,j].ToString() + "\t"
            ret <- ret + "\n"
        ret <- ret + "}\n"
        ret
    // TODO: add Matrix-Vector mult

type Matrix3() =
    inherit Matrix(3,3)

    member this.Determinant() =
        this.[0,0] * (this.[1,1] * this.[2,2] - this.[1,2] * this.[2,1]) +
        this.[0,1] * - (this.[1,0] * this.[2,2] - this.[1,2] * this.[0,2]) +
        this.[0,2] * (this.[0,1] * this.[1,2] - this.[1,1] * this.[0,2])
    member this.Inverse() =
        let det = this.Determinant()
        assert(det <> 0.0)
        let detInv = 1.0 / det
        let tr = this.Trace()
        let msqr = this * this
        let tr2 = msqr.Trace()
        do printf "%f\n%f\n" det tr
        // Hard coded Cayley-Hamilton Method
        (Matrix.Identity(3).ScalarMult(0.5 * (tr * tr - tr2)) - this.ScalarMult(tr) + msqr).ScalarMult(detInv)

                           

type Matrix4() =
    inherit Matrix(4,4)

    member this.Minor3() = 
        let newMat = Matrix3()
        for i = 0 to 2 do
            for j = 0 to 2 do
                newMat.[i,j] <- this.[i,j]
        newMat