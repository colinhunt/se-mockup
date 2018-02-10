module Model.Utils exposing (..)

import Model.Model exposing (..)


treeMap :
    (a -> List a)
    -> (List a -> a -> a)
    -> (a -> a)
    -> a
    -> a
treeMap getChildren setChildren f nodeA =
    let
        treeMapR nodeA =
            let
                nodeB =
                    f nodeA

                childrenB =
                    List.map treeMapR (getChildren nodeB)
            in
                setChildren childrenB nodeB
    in
        treeMapR nodeA
