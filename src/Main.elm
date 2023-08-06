module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (style, type_, checked)
import Html.Events exposing (onClick, on)
import Json.Decode as Decode exposing (Decoder, float, field)
import Time
import Task exposing (perform)
import Color exposing (Color, toCssString, rgb, rgba)

import Linear.Vectors.Vec2D 
    exposing (Vec2D, add, scale, norm, subtract, normalize, distance, dot)

import Dynamical.Numerical exposing (dormandPrince)

import Dynamical.Physics 
    exposing (Particle, addParticle, initParticle, escapeVelocity,
              particleSpeed, maxParticleSpeed, forceGravity, forceDamping,
              specificMechanicalEnergy, specificRelativeAngularMomentum,
              ellipticalOrbit, eccentricityVector)

import Linear.ChangeCoords2D exposing (vec2DToVector, vectorToVec2D)
import Curves.Ellipse exposing (Ellipse)

type alias Model =
    { particles : List Particle
    , errorParticles : List Particle
    , centreOfGravity : Vec2D
    , lastTimeMS : Int
    , deltaTimeMS : Int
    , cursorRadius : Float
    , particleRadius : Float
    , gravityConstant : Float
    , dampingConstant : Float
    , centreMass : Float
    , particleMass : Float
    , numParticles : Int
    , showEllipses : Bool
    }

type Msg
    = MouseMove Int Int
    | Tick Int
    | Init Int
    | IncreaseGravityConstant
    | DecreaseGravityConstant
    | IncreaseDampingConstant
    | DecreaseDampingConstant
    | IncreaseMouseMass
    | DecreaseMouseMass
    | IncreaseParticleMass
    | DecreaseParticleMass
    | ResetParticlePositions
    | ResetGravityConstant
    | ResetDampingConstant
    | ResetMouseMass
    | ResetParticleMass
    | IncreaseNumParticles
    | DecreaseNumParticles
    | ResetNumParticles
    | ToggleShowEllipses
    
type alias Flags = ()


-------------------------------------------------------------------------------
-- init
init : Flags -> (Model, Cmd Msg)
init _ =
    let
        initNumOfParticles = 8
        tempModel =
            { particles = []
            , errorParticles = List.repeat initNumOfParticles initParticle
            , centreOfGravity = { x = 0, y = 0 }
            , lastTimeMS = 0
            , deltaTimeMS = 0
            , cursorRadius = 4
            , particleRadius = 12
            , gravityConstant = 9.8
            , dampingConstant = 0
            , centreMass = 50
            , particleMass = 30
            , numParticles = initNumOfParticles
            , showEllipses = True
            }
        initialModel =
            { tempModel | particles = resettedParticles tempModel
            }
    in
    ( initialModel
    , Task.map Time.posixToMillis Time.now |> Task.perform Init
    )


-------------------------------------------------------------------------------
-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMove x y ->
            ({ model | centreOfGravity = { x = toFloat x, y = toFloat y } }, Cmd.none)
        
        Tick realCurrTimeMS ->
            let
                dt = 10
                currTimeMS = model.lastTimeMS + dt
                
                computeAcceleration position velocity mass =
                    let
                        forceGravityRK4Particle = 
                            forceGravity model.gravityConstant model.centreMass 
                            position mass model.centreOfGravity
                        forceDampingRK4Particle = 
                            forceDamping model.dampingConstant mass velocity
                    in
                    add (scale (1 / mass) forceGravityRK4Particle) 
                        (scale (1 / mass) forceDampingRK4Particle)
                
                updateParticle particle mass =
                    let
                        stateVectorInit = 
                            [particle.position.x, particle.position.y, 
                            particle.velocity.x, particle.velocity.y]
                        
                        derivative time stateVector = 
                            case stateVector of
                                [posX, posY, velX, velY] ->
                                    let 
                                        pos = vectorToVec2D [posX, posY]
                                        vel = vectorToVec2D [velX, velY]
                                        
                                        deltaPosition = vel
                                        deltaVelocity = 
                                            computeAcceleration pos vel mass
                                    in
                                    [deltaPosition.x, deltaPosition.y, 
                                    deltaVelocity.x, deltaVelocity.y]
                                _ ->
                                    [0, 0, 0, 0]
                        
                        (newValues, errors) = 
                            dormandPrince (dt |> toFloat |> (\x -> x / 10)) 
                            (currTimeMS |> toFloat) stateVectorInit derivative
                        (newPosition, newVelocity) =
                            case newValues of
                                [newPosX, newPosY, newVelX, newVelY] ->
                                    (vectorToVec2D [newPosX, newPosY], 
                                    vectorToVec2D [newVelX, newVelY])
                                _ ->
                                    (vectorToVec2D [0, 0], vectorToVec2D [0, 0])
                        (newPositionError, newVelocityError) =
                            case errors of
                                [newPosX, newPosY, newVelX, newVelY] ->
                                    (vectorToVec2D [abs newPosX, abs newPosY], 
                                    vectorToVec2D [abs newVelX, abs newVelY])
                                _ ->
                                    (vectorToVec2D [0, 0], vectorToVec2D [0, 0])
                        
                        errorParticle = 
                            { position = newPositionError, 
                            velocity = newVelocityError }
                    in
                    ( { particle | position = newPosition, 
                        velocity = newVelocity }
                    , errorParticle
                    )
                
                (updatedParticles, errorParticles) = 
                    List.unzip <| List.map (\p -> updateParticle p 
                        model.particleMass) model.particles
            in
            ({ model 
                | particles = updatedParticles
                , errorParticles = errorParticles
                , lastTimeMS = currTimeMS
                , deltaTimeMS = currTimeMS - model.lastTimeMS
            }, Cmd.none)
        
        Init currTimeMS ->
            ({ model | lastTimeMS = currTimeMS, deltaTimeMS = currTimeMS - model.lastTimeMS }, Cmd.none)
        
        IncreaseGravityConstant ->
            let
                newGravityConstant = model.gravityConstant + 0.1
            in
            ({ model | gravityConstant = newGravityConstant }, Cmd.none)
        
        DecreaseGravityConstant ->
            let
                newGravityConstant = model.gravityConstant - 0.1
            in
            ({ model | gravityConstant = newGravityConstant }, Cmd.none)
        
        IncreaseDampingConstant ->
            let
                newDampingConstant = model.dampingConstant + 0.05
            in
            ({ model | dampingConstant = newDampingConstant }, Cmd.none)
        
        DecreaseDampingConstant ->
            let
                newDampingConstant = model.dampingConstant - 0.01
            in
            ({ model | dampingConstant = newDampingConstant }, Cmd.none)
        
        IncreaseMouseMass ->
            let
                newMouseMass = model.centreMass + 0.1
            in
            ({ model | centreMass = newMouseMass }, Cmd.none)
        
        DecreaseMouseMass ->
            let
                newMouseMass = model.centreMass - 0.1
            in
            ({ model | centreMass = newMouseMass }, Cmd.none)
        
        IncreaseParticleMass ->
            let
                newParticleMass = model.particleMass + 0.1
            in
            ({ model | particleMass = newParticleMass }, Cmd.none)
        
        DecreaseParticleMass ->
            let
                newParticleMass = model.particleMass - 0.1
            in
            ({ model | particleMass = newParticleMass }, Cmd.none)
        
        ResetParticlePositions -> (resettedModel model, Cmd.none)
        
        ResetGravityConstant ->
            ({ model | gravityConstant = 9.8 }, Cmd.none)
        
        ResetDampingConstant ->
            ({ model | dampingConstant = 0 }, Cmd.none)
        
        ResetMouseMass ->
            ({ model | centreMass = 50 }, Cmd.none)
        
        ResetParticleMass ->
            ({ model | particleMass = 30 }, Cmd.none)
        
        IncreaseNumParticles ->
            let
                newNumParticles = model.numParticles * 2
                newModel = { model | numParticles = newNumParticles }
            in
            (resettedModel newModel, Cmd.none)
        
        DecreaseNumParticles ->
            let
                newNumParticles = max 1 (model.numParticles // 2)
                newModel = { model | numParticles = newNumParticles }
            in
            (resettedModel newModel, Cmd.none)
        
        ResetNumParticles ->
            let
                newNumParticles = 8
                newModel = { model | numParticles = newNumParticles }
            in
            (resettedModel newModel, Cmd.none)
        
        ToggleShowEllipses ->
            ({ model | showEllipses = not model.showEllipses }, Cmd.none)

resettedModel : Model -> Model
resettedModel model = 
    { model     
    | particles = resettedParticles model
    , errorParticles = List.repeat model.numParticles initParticle
    }

resettedParticles : Model -> List Particle
resettedParticles model =
    let
        maxRadius = 300
        resetEachParticle i = 
            let
                fraction = i / toFloat model.numParticles
                positionOnLine = maxRadius * fraction
                distance = sqrt ((model.centreOfGravity.x - 400)^2 + (model.centreOfGravity.y - (400 + positionOnLine))^2)
                velocityMagnitude = sqrt (model.gravityConstant * model.centreMass / distance)
            in
            { position = { x = 400
                         , y = 400 + positionOnLine }
            , velocity = { x = velocityMagnitude
                         , y = 0 }
            }
    in
    List.map resetEachParticle (List.map toFloat (List.range 1 model.numParticles))


-------------------------------------------------------------------------------
-- view
view : Model -> Html Msg
view model =
    let
        ellipseHtml = 
            if model.showEllipses then
                List.filterMap (Maybe.map viewEllipse << ellipticalOrbit model.centreOfGravity model.gravityConstant model.centreMass) model.particles
            else
                []

        escapeVelocityCount = 
            countEscapeVelocityParticles model model.particles

        energies = 
            List.map 
                (\p -> specificMechanicalEnergy
                            model.gravityConstant 
                            model.centreMass
                            (norm <| subtract p.position model.centreOfGravity) 
                            (norm p.velocity)) 
                model.particles

        globalMin = List.minimum (energies) |> Maybe.withDefault 0
        globalMax = List.maximum (energies) |> Maybe.withDefault 0
        
        minEnergy = List.minimum energies
        maxEnergy = List.maximum energies

        particlePairs = 
            List.map2 Tuple.pair model.particles model.errorParticles

        errorMagnitudes = 
            List.map 
                (\errorParticle ->
                    sqrt (  errorParticle.position.x^2
                          + errorParticle.position.y^2
                          + errorParticle.velocity.x^2
                          + errorParticle.velocity.y^2 ))
                model.errorParticles

        globalMinError = List.minimum errorMagnitudes |> Maybe.withDefault 0
        globalMaxError = List.maximum errorMagnitudes |> Maybe.withDefault 0
    in
        div
            [ style "height" "100vh"
            , style "width" "100vw"
            , style "display" "flex"
            ]
            [ div
                [ onMouseMove MouseMove
                , onContextMenu ResetParticlePositions
                , style "height" "100%"
                , style "width" "50%"
                , style "border" "1px solid black"
                , style "padding" "20px"
                , style "box-sizing" "border-box"
                , style "background-color" "black"
                , style "color" "orange"
                , style "font-size" "20px"
                , style "overflow" "auto"
                ]
                ( List.map 
                    (\(particle, errorParticle) -> 
                        viewParticle
                            model
                            particle
                            errorParticle
                            False 
                            globalMinError
                            globalMaxError)
                    particlePairs
                ++ ellipseHtml
                ++ [viewMouse model]
                )
            , div
                [ style "height" "100%"
                , style "width" "50%"
                , style "border" "1px solid black"
                , style "padding" "20px"
                , style "box-sizing" "border-box"
                , style "background-color" "black"
                , style "color" "orange"
                , style "font-size" "20px"
                , style "overflow" "auto"
                ]
                (  [ div [ style "margin-top" "10px" ] [ text "Move your mouse around the screen." ]
                   , div [ style "margin-top" "10px" ] [ text ("The particles are gravitationally attracted to it.") ]
                   , div [ style "margin-top" "10px" ] [ text ("These particles also feel 'viscous damping' (default off).") ]
                   , div [ style "margin-top" "10px" ] [ text ("If a particle has achieved escape velocity, a square appears around it.") ]
                   , div [ style "margin-top" "10px" ] [ text ("The local truncation error due to numerical integration of a particle relative to the others is depicted in it's colour (redder particles have more error).") ]
                   , div [ style "margin-top" "10px" ] [ text ("Number of particles achieving escape velocity: " ++ String.fromInt escapeVelocityCount) ]
                   ]
                ++ [ div [ style "margin-top" "10px" ]
                        [ Html.input [ type_ "checkbox", checked model.showEllipses, onClick ToggleShowEllipses, style "width" "30px", style "height" "30px" ] []
                        , text " Show ellipses"
                        ]
                   ]
                ++ List.concat
                    [ 
                        createButtonGroup "Gravity Constant: " model.gravityConstant IncreaseGravityConstant DecreaseGravityConstant ResetGravityConstant
                        , createButtonGroup "Damping Constant: " model.dampingConstant IncreaseDampingConstant DecreaseDampingConstant ResetDampingConstant
                        , createButtonGroup "Mouse Mass: " model.centreMass IncreaseMouseMass DecreaseMouseMass ResetMouseMass
                        , createButtonGroup "Particle Mass: " model.particleMass IncreaseParticleMass DecreaseParticleMass ResetParticleMass
                        , createButtonGroup "Number of Particles: " (toFloat model.numParticles) IncreaseNumParticles DecreaseNumParticles ResetNumParticles
                    ]
                ++ [ button [ onClick ResetParticlePositions, style "width" "200px", style "height" "50px", style "margin-top" "10px", style "font-size" "20px" ] [ text "Reset Particle Positions" ]
                   , text "(or just right-click anywhere)"
                   ]
                )
            ]

createButtonGroup : String -> Float -> Msg -> Msg -> Msg -> List (Html Msg)
createButtonGroup label value increaseMsg decreaseMsg resetMsg =
    [ div [ style "margin-top" "10px", style "font-size" "20px" ]
        [ button [ onClick increaseMsg, style "width" "100px", style "height" "40px", style "font-size" "18px" ] [ text "Increase" ]
        , button [ onClick decreaseMsg, style "width" "100px", style "height" "40px", style "font-size" "18px" ] [ text "Decrease" ]
        , button [ onClick resetMsg, style "width" "100px", style "height" "40px", style "font-size" "18px" ] [ text "Reset" ]
        , text (label ++ String.fromFloat value)
        ]
    ]

onMouseMove : (Int -> Int -> msg) -> Html.Attribute msg
onMouseMove tagger =
    on "mousemove" (Decode.map2 tagger (Decode.field "pageX" Decode.int) (Decode.field "pageY" Decode.int))

onContextMenu : msg -> Html.Attribute msg
onContextMenu message =
    Html.Events.preventDefaultOn "contextmenu" (Decode.succeed (message, True))

countEscapeVelocityParticles : Model -> List Particle -> Int
countEscapeVelocityParticles model particles =
    List.foldl 
        (\particle count -> 
            let
                dist = distance model.centreOfGravity particle.position
                escapeVel = escapeVelocity model.gravityConstant 
                            model.centreMass dist
            in
            if norm particle.velocity >= escapeVel then
                count + 1
            else
                count
        ) 0 particles

viewParticle : Model -> Particle -> Particle -> Bool -> Float -> Float -> Html Msg
viewParticle model particle errorParticle highlight 
    globalMinError globalMaxError =
    let
        colorParticle = particleColor model particle errorMagnitude
        rgba = Color.toRgba colorParticle
        r = rgba.red
        g = rgba.green
        b = rgba.blue
        a = rgba.alpha
        color = "rgba(" ++ String.fromInt (round r) ++ "," 
            ++ String.fromInt (round g) ++ "," 
            ++ String.fromInt (round b) ++ "," 
            ++ String.fromFloat a ++ ")"
        borderWidth = if highlight then (model.particleRadius + 2) else 0
        borderColor = if highlight then "rgba(" 
            ++ String.fromInt (round r) ++ "," 
            ++ String.fromInt (round g) ++ "," 
            ++ String.fromInt (round b) ++ ",0.5)" 
            else "transparent"

        errorMagnitude = sqrt (  errorParticle.position.x^2 
                               + errorParticle.position.y^2 
                               + errorParticle.velocity.x^2 
                               + errorParticle.velocity.y^2 )
        
        particleRadius = model.particleRadius
        
        dist = distance particle.position model.centreOfGravity
        escapeVel = escapeVelocity model.gravityConstant model.centreMass dist
        particleVel = particleSpeed particle
        hasEscapeVel = particleVel >= escapeVel
        
        squareSize = particleRadius * 3
        squareBorderColor = "white"
        squareBorderWidth = 2.0  -- adjust this value as needed
    in
    div []
        [ div
            [ style "position" "absolute"
            , style "height" (String.fromFloat particleRadius ++ "px")
            , style "width" (String.fromFloat particleRadius ++ "px")
            , style "background-color" color
            , style "border-radius" "50%"
            , style "border" ("solid " ++ String.fromFloat borderWidth ++ "px " ++ borderColor)
            , style "left" (String.fromFloat (particle.position.x - particleRadius/2 - borderWidth) ++ "px")
            , style "top" (String.fromFloat (particle.position.y - particleRadius/2 - borderWidth) ++ "px")
            , style "pointer-events" "none"
            ]
            []
        , if hasEscapeVel then
            div
                [ style "position" "absolute"
                , style "height" (String.fromFloat squareSize ++ "px")
                , style "width" (String.fromFloat squareSize ++ "px")
                , style "border" ("solid " ++ String.fromFloat squareBorderWidth ++ "px " ++ squareBorderColor)
                , style "left" (String.fromFloat (particle.position.x - squareSize/2 - squareBorderWidth) ++ "px")
                , style "top" (String.fromFloat (particle.position.y - squareSize/2 - squareBorderWidth) ++ "px")
                , style "pointer-events" "none"
                ]
                []
          else
            text ""
        ]

particleColor : Model -> Particle -> Float -> Color
particleColor model particle errorMagnitude =
    let
        baseColor = (255, 255, 190)
        maxErrorColor = (255, 0, 0)
        (rBase, gBase, bBase) = baseColor
        (rMax, gMax, bMax) = maxErrorColor
        logError = 1-1/(1+3*errorMagnitude^(0.8))
        r = rBase + (toFloat (rMax - rBase) * logError)
        g = gBase - (toFloat (gBase - gMax) * logError)
        b = bBase - (toFloat (bBase - bMax) * logError)
    in
    Color.rgba r g b 255

viewMouse : Model -> Html Msg
viewMouse model =
    div
        [ style "position" "absolute"
        , style "height" (String.fromFloat (2*model.cursorRadius) ++ "px")
        , style "width" (String.fromFloat (2*model.cursorRadius) ++ "px")
        , style "background-color" "yellow"
        , style "border-radius" "50%"
        , style "left" (String.fromFloat (model.centreOfGravity.x - model.cursorRadius) ++ "px")
        , style "top" (String.fromFloat (model.centreOfGravity.y - model.cursorRadius) ++ "px")
        , style "pointer-events" "none"
        ]
        []

viewEllipse : Ellipse -> Html Msg
viewEllipse ellipse =
    let
        e = sqrt (1 - ellipse.b^2/ellipse.a^2)
        semiMinor = ellipse.a * sqrt (1 - e^2)
        angle = ellipse.rotation
        rotationDegrees = angle * (180 / pi)
        centerX = String.fromFloat (ellipse.centre.x)
        centerY = String.fromFloat (ellipse.centre.y)
    in
    div
        []
        [ div
            [ style "position" "absolute"
            , style "height" (String.fromFloat (2 * semiMinor) ++ "px")
            , style "width" (String.fromFloat (2 * ellipse.a) ++ "px")
            , style "border" "1px solid white"
            , style "border-radius" "50%"
            , style "left" (centerX ++ "px")
            , style "top" (centerY ++ "px")
            , style "transform" ("translate(-50%, -50%) rotate(" ++ String.fromFloat rotationDegrees ++ "deg)")
            , style "transform-origin" "center"
            , style "pointer-events" "none"
            , style "background-color" "transparent"
            ]
            []
        ]


-------------------------------------------------------------------------------
-- subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 10 (\currTimePosix -> Tick (Time.posixToMillis currTimePosix))


-------------------------------------------------------------------------------
-- main
main =
    Browser.element 
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions 
        }
