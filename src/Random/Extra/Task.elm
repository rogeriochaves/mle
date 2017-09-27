module Random.Extra.Task exposing (..)

import Platform exposing (Task)
import Random exposing (Generator)
import Task
import Time


toTask : Generator a -> Task Never a
toTask generator =
    Task.map
        (Tuple.first
            << Random.step generator
            << Random.initialSeed
            << round
        )
        Time.now
