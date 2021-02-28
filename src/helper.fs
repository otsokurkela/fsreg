type ErrType = string
type ActFn = | ID    //identity: f(x) = x
             | TANH
type Layer = {wMatx: Matrix<float>; actFn: ActFn;}
type Network = Layer list
type NodesVect = {sVect: Vector<float>; xVect: Vector<float>} //each nodesVect will be S => without actFn and X => with actFn
type NodesVectList = NodesVect list

let epochs = 1//50
let lr = 0.04//0.0001

let getActFn (fn: ActFn) : (float->float) =
    match fn with
    | ID -> id
    | TANH -> tanh
let getActFnDeriv (fn: ActFn) : (float->float) =
    match fn with
    | ID -> (fun _ -> 1.0)
    | TANH -> (fun x  -> 1.0 - (tanh(x)**2.0))

//Only works with MSE as metric
let lastLayerDeriv (x_L : Vector<float>) (s_L : Vector<float>) (y : Vector<float>) (theta_L' : ActFn) =
    (2.0*(x_L - y)).PointwiseMultiply(s_L.Map (Func<float, float> (getActFnDeriv (theta_L'))))
let mse (h : Vector<float>) (y: Vector<float>) =
