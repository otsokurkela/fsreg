let fwdProp (x: Vector<float>) (net : Network) : (NodesVectList * Network) =
    let propNetwork (nvectLstAndLayers: NodesVectList * Network) (layer : Layer) : (NodesVectList * Network) =
        let nvectLst, layerLst = nvectLstAndLayers
        let newSVect =  layer.wMatx.Transpose() * CreateVector.Dense((Array.concat [ [|1.0|] ; nvectLst.Head.xVect.ToArray() ]))
        let newXVect = (newSVect.Map (Func<float, float> (getActFn layer.actFn)))
        ({xVect=newXVect ; sVect=newSVect} :: nvectLst , layer :: layerLst) //append to top

    (List.fold propNetwork ([{xVect=x ; sVect=x}], []) net)