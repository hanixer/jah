module Common


type Distance = float
type [<Measure>] Degrees
type Angle = float<Degrees>
type PenState = Up | Down
type PenColor = Black | Red | Blue
type Position = {x:float; y:float}