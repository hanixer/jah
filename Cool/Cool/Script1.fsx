#load "Queue.fs"

Queue.enqueueList [1;2;3] Queue.empty |> Queue.enqueueListUnique (=) [0..5]