module Dynamical.Numerical exposing (IntegrationMethod(..), eulers, rk4, dormandPrince, dormandPrinceFixedStep, rk4Step, rk4Steps, derivExample, example)

import List exposing (drop, map, head)

import Linear.Vectors.VecND exposing (Vector, add, scale)

type IntegrationMethod
    = Eulers
    | RK4
    | DormandPrince

eulers : Float -> Float -> Vector -> (Float -> Vector -> Vector) -> Vector
eulers dt t y f =
    let
        k1 = f t y
    in
    add y (scale dt k1)

rk4 : Float -> Float -> Vector -> (Float -> Vector -> Vector) -> Vector
rk4 dt t y f =
    let
        k1 = f t y
        k2 = f (t + dt/2) (add y (scale (dt/2) k1))
        k3 = f (t + dt/2) (add y (scale (dt/2) k2))
        k4 = f (t + dt)   (add y (scale dt k3))
    in
    add y (scale (dt / 6) (add (add (add k1 (scale 2 k2)) (scale 2 k3)) k4))

dormandPrince : Float -> Float -> Vector -> (Float -> Vector -> Vector) -> (Vector, Vector)
dormandPrince dt t y f =
    let
        k1 = f t y
        k2 = f (t + dt/5) (add y (scale (dt/5) k1))
        k3 = f (t + (3*dt)/10) (add y (add (scale ((3*dt)/40) k1) (scale ((9*dt)/40) k2)))
        k4 = f (t + (4*dt)/5) (add y (add (scale ((44*dt)/45) k1) (add (scale ((-56*dt)/15) k2) (scale ((32*dt)/9) k3))))
        k5 = f (t + (8*dt)/9) (add y (add (scale ((19372*dt)/6561) k1) (add (scale ((-25360*dt)/2187) k2) (add (scale ((64448*dt)/6561) k3) (scale ((-212*dt)/729) k4)))))
        k6 = f (t + dt) (add y (add (scale ((9017*dt)/3168) k1) (add (scale (-(355*dt)/33) k2) (add (scale ((46732*dt)/5247) k3) (add (scale ((49*dt)/176) k4) (scale ((-5103*dt)/18656) k5))))))
        
        y_next = add y (scale dt (add (scale (35/384) k1) (add (scale (500/1113) k3) (add (scale (125/192) k4) (add (scale (-2187/6784) k5) (scale (11/84) k6))))))
        y_err = scale dt (add (scale ((35/384)-(5179/57600)) k1) (add (scale ((500/1113)-(7571/16695)) k3) (add (scale ((125/192)-(393/640)) k4) (add (scale ((-2187/6784)-(-92097/339200)) k5) (scale ((11/84)-(187/2100)) k6)))))
    in
    (y_next, y_err)

dormandPrinceFixedStep : Float -> Float -> Vector -> (Float -> Vector -> Vector) -> Vector
dormandPrinceFixedStep dt t y f = Tuple.first (dormandPrince dt t y f)

derivExample : Float -> Vector -> Vector
derivExample t y =
    let
        g = -9.8  -- Acceleration due to gravity
    in
        case y of
            [x,dx,ddx] -> [dx,ddx,1]
            _ -> []

rk4Step dt f (t,y) = (t+dt, rk4 dt t y f)

rk4Steps n dt f (t,y) =
    case n of
        0 -> []
        _ -> 
            let
                next = (rk4Step dt f (t,y))
            in
            (t,y) :: rk4Steps (n-1) dt f next

example = map (\(x,y) -> drop 1 >> head >> Maybe.withDefault 0 <| y) (rk4Steps 10 0.1 derivExample (0,[0,0,0]))