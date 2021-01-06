namespace LinkedList

module LinkedList =
    type LinkedList =
        | Item of int * LinkedList
        | End

    let empty =
        LinkedList.End

    let rec range start stop =
        if start <= stop then
            Item (start, (range (start + 1) stop))
        else
            End

    let cons value list =
        Item (value, list)

    let rec append alist blist =
        match alist with
        | Item (head, tail) -> Item (head, append tail blist)
        | End -> blist

    let rec length list =
        match list with
        | Item (head, tail) -> 1 + length tail
        | End -> 0

    let head list =
        match list with
        | Item (head, _) -> Some head
        | End -> None

    let tail list =
        match list with
        | Item (_, tail) -> tail
        | End -> End
