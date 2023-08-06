module Dynamical.Physics exposing (Particle, addParticle, initParticle, escapeVelocity, particleSpeed, maxParticleSpeed, forceGravity, forceDamping, semiMajorAxis, eccentricityVector, specificMechanicalEnergy, specificRelativeAngularMomentum, ellipticalOrbit)

import Linear.Vectors.Vec2D exposing (Vec2D, add, scale, norm, subtract, normalize, distance, dot)
import Curves.Ellipse exposing (Ellipse)

type alias Particle =
    { position : Vec2D
    , velocity : Vec2D
    }

addParticle : Particle -> Particle -> Particle
addParticle p1 p2 =
    { position = add p1.position p2.position
    , velocity = add p1.velocity p2.velocity
    }

initParticle : Particle
initParticle =
    { position = { x = 0, y = 0 }
    , velocity = { x = 0, y = 0 }
    }

escapeVelocity : Float -> Float -> Float -> Float
escapeVelocity gravityConstant mass distance =
    sqrt (2 * gravityConstant * mass / distance)

particleSpeed : Particle -> Float
particleSpeed particle =
    norm particle.velocity

maxParticleSpeed : List Particle -> Maybe Float
maxParticleSpeed particles =
    List.map particleSpeed particles
        |> List.maximum

forceGravity : Float -> Float -> Vec2D -> Float -> Vec2D -> Vec2D
forceGravity gravityConstant mass1 pos1 mass2 pos2 =
    let
        direction = subtract pos2 pos1
        distance = norm direction
        normalized = normalize direction
    in
    if distance == 0 then
        { x = 0, y = 0 }
    else
        scale (gravityConstant * mass1 * mass2 / (distance * distance)) normalized

forceDamping : Float -> Float -> Vec2D -> Vec2D
forceDamping dampingConstant mass velocity =
    scale (-dampingConstant / mass) velocity

semiMajorAxis : Float -> Float -> Float -> Float -> Float
semiMajorAxis gravityConstant centreMass r v =
    1 / ((2 / r) - (v^2 / (gravityConstant * centreMass)))

eccentricityVector : Float -> Vec2D -> Particle -> Vec2D
eccentricityVector gravityConstant centreOfGravity particle =
    let
        mu = gravityConstant
        r = subtract particle.position centreOfGravity
        v = particle.velocity
    in
    subtract (scale ((norm v)^2 / mu - 1 / norm r) r) (scale ((dot r v) / mu) v)

specificMechanicalEnergy : Float -> Float -> Float -> Float -> Float
specificMechanicalEnergy gravityConstant centreMass r v =
    (v^2 / 2) - (gravityConstant * centreMass / r)

angleBetween : Vec2D -> Vec2D -> Float
angleBetween a b =
    let
        dotProduct = a.x * b.x + a.y * b.y
        magA = sqrt (a.x^2 + a.y^2)
        magB = sqrt (b.x^2 + b.y^2)
    in
    acos (dotProduct / (magA * magB))

specificRelativeAngularMomentum : Vec2D -> Vec2D -> Float
specificRelativeAngularMomentum rVec vVec =
    let
        r = norm rVec
        v = norm vVec
        theta = angleBetween rVec vVec
    in
    r * v * sin theta

ellipticalOrbit : Vec2D -> Float -> Float -> Particle -> Maybe Ellipse
ellipticalOrbit centreOfGravity gravityConstant centreMass particle =
    let
        rVec = subtract particle.position centreOfGravity
        vVec = particle.velocity
        
        eVec = eccentricityVector (gravityConstant * centreMass) centreOfGravity particle
        e = norm eVec

        -- semi-major axis
        a = semiMajorAxis gravityConstant centreMass (norm rVec) (norm vVec)
        b = sqrt ((1-e^2)*a^2)

        -- rotation angle
        rotation = atan2 (eVec.y) (eVec.x)  -- Note that the arguments to atan2 are switched 

    in
    if e < 1 then
        Just
            { centre = subtract centreOfGravity (scale a eVec)
            , a = a
            , b = b
            , rotation = rotation
            }
    else
        -- The orbit is not elliptical
        Nothing
