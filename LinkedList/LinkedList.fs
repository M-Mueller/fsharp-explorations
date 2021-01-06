namespace LinkedList

module LinkedList =
    type LinkedList<'T> =
        | Item of 'T * LinkedList<'T>
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

    let rec map f list =
        match list with
        | Item (head, tail) -> Item(f head, map f tail)
        | End -> End
