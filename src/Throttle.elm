module Throttle exposing
    ( Throttle
    , create
    , ifNeeded
    , try
    , update
    )


type Throttle msg
    = Throttle (ThrottleInfo msg)



{--
counterMaximum : How many times `update` needs to be applied before it will allow the next command to be executed
counter: keeps track of number of calls to `update`
command: A command to be executed when `update` sets the counter to 0
--}


type alias ThrottleInfo msg =
    { counterMaximum : Int
    , counter : Int
    , command : Maybe (Cmd msg)
    }



{--
Create a throttle that will ensure that commands will be executed at most once per `counterMaximum` applications of
`update` The counter is initially set to `0` to ensure the execution of the first command is not delayed.
--}


create : Int -> Throttle msg
create counterMaximum =
    Throttle
        { counterMaximum = counterMaximum
        , counter = 0
        , command = Nothing
        }



{--
If the counter is `0` and there is a stored command:
Return a `Throttle` whose counter is set to the counter maximum, and the stored command

If the counter is `0` and there is not a stored command:
Return an identical `Throttle` and `Cmd.none`
(Note: By using the `ifNeeded` function,  this will never happen.)

If the counter is not `0`:
Returns a throttle with a counter equal to the old counter minus one, and `Cmd.none`
--}


update : Throttle msg -> ( Throttle msg, Cmd msg )
update (Throttle info) =
    if info.counter == 0 then
        case info.command of
            Just cmd_ ->
                ( Throttle { info | counter = info.counterMaximum, command = Nothing }, cmd_ )

            Nothing ->
                ( Throttle info, Cmd.none )

    else
        ( Throttle { info | counter = info.counter - 1 }, Cmd.none )



-- Execute a command if the counter is 0 .


try : Cmd msg -> Throttle msg -> ( Throttle msg, Cmd msg )
try cmd (Throttle info) =
    if info.counter == 0 then
        ( Throttle { info | counter = info.counterMaximum, command = Nothing }, cmd )

    else
        ( Throttle { info | command = Just cmd }, Cmd.none )



{--
If the throttle's counter is `0` and there is no stored command, return `Sub.none`, otherwise, return the
subscription that was passed in.
This allows you to use high frequency subscriptions for throttling and not worry about it running when it's not needed.
--}


ifNeeded : Sub msg -> Throttle msg -> Sub msg
ifNeeded sub (Throttle info) =
    if info.counter > 0 || info.command /= Nothing then
        sub

    else
        Sub.none
