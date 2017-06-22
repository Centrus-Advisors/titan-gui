module Utils.Delay exposing (..)

import Process
import Task


delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
