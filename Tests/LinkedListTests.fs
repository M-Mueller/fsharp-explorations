module LinkedListTests

open Xunit
open FsUnit.Xunit
open LinkedList

[<Fact>]
let ``range from 0 to 3 has 4 elements`` () =
    let result =
        LinkedList.Item (0, LinkedList.Item (1, LinkedList.Item (2, LinkedList.Item (3, LinkedList.End))))

    LinkedList.range 0 3 |> should equal result

[<Fact>]
let ``range from 3 to 3 has 1 element`` () =
    let result =
        LinkedList.Item (3, LinkedList.End)

    LinkedList.range 3 3 |> should equal result

[<Fact>]
let ``if range start is larger than stop, the result is empty`` () =
    let result:LinkedList.LinkedList<int> =
        LinkedList.End

    LinkedList.range 3 0 |> should equal result

[<Fact>]
let ``Length of empty list is 0`` () =
    let list =
        LinkedList.empty

    LinkedList.length list |> should equal 0

[<Fact>]
let ``Length of list with 2 items is 2`` () =
    let list =
        LinkedList.Item (1, (LinkedList.Item (2, LinkedList.End)))

    LinkedList.length list |> should equal 2

[<Fact>]
let ``cons prepends an item to an empty list`` () =
    let list =
        LinkedList.cons 1 LinkedList.empty

    list |> should equal (LinkedList.Item (1, LinkedList.End))

[<Fact>]
let ``cons prepends an item to an existing list`` () =
    let list =
        LinkedList.cons 0 (LinkedList.range 1 2)

    let result =
        LinkedList.Item (0, LinkedList.Item (1, LinkedList.Item (2, LinkedList.End)))

    list |> should equal result

type ``appending an empty list does not change the original list`` () =
    let range =
        LinkedList.range 1 3
    let empty =
        LinkedList.empty

    [<Fact>]
    member x.``when the left argument is empty`` () =
        LinkedList.append empty range |> should equal range

    [<Fact>]
    member x.``when the right argument is empty`` () =
        LinkedList.append range empty |> should equal range

    [<Fact>]
    member x.``when the boths arguments are empty`` () =
        LinkedList.append empty empty |> should equal empty

[<Fact>]
let ``appending [4; 5] to [1; 2; 3] results in [1; 2; 3; 4; 5]`` () =
    let list1 =
        LinkedList.range 1 3
    let list2 =
        LinkedList.range 4 5
    let result =
        LinkedList.range 1 5

    LinkedList.append list1 list2 |> should equal result

[<Fact>]
let ``head of a list is the value of the first item`` () =
    let list =
        LinkedList.range 1 10

    LinkedList.head list |> should equal (Some 1)

[<Fact>]
let ``head of a list an empty list is None`` () =
    let list =
        LinkedList.empty

    LinkedList.head list |> should equal None

[<Fact>]
let ``tail of a list is the list without the first item`` () =
    let list =
        LinkedList.range 1 10

    let result =
        LinkedList.range 2 10

    LinkedList.tail list |> should equal result

[<Fact>]
let ``tail of an empty list is an empty list`` () =
    let list =
        LinkedList.empty

    LinkedList.tail list |> should equal LinkedList.empty
