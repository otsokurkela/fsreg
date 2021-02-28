let initNetwork (nodeLst : int list) (actFnLst : ActFn list) : Result<Network, ErrType> =
    let genRandMatx (rows) (cols) : Matrix<float> = CreateMatrix.Random(rows,cols)

    match (nodeLst.Length - actFnLst.Length) with
    | 1 -> 
        match nodeLst.Length  with
        | x when x > 1 ->
            match (List.length (List.filter (fun x -> x > 0) nodeLst)) with
            | y when y = x ->
                List.init (actFnLst.Length) (fun i ->
                    {
                        wMatx = genRandMatx (nodeLst.[i] + 1) (nodeLst.[i+1]);
                        actFn = actFnLst.[i];
                    }
                )
                |> Ok
            | _ -> Error("The specification was incorrect, all item in nodeLst must be positive.")
        | _ -> Error("The specification was incorrect, need at-least two items in nodeLst.")
    | _ -> Error("The specification was incorrect, nodeLst must be exactly one item greater than actFnLst.")