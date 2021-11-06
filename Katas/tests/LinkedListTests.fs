module LinkedListTests

open Xunit
open Swensen.Unquote
open LinkedList

[<Fact>]
let ``range from 0 to 3 has 4 elements`` () =
    let result =
        LinkedList.Item (0, LinkedList.Item (1, LinkedList.Item (2, LinkedList.Item (3, LinkedList.End))))

    test <@ LinkedList.range 0 3 = result @>

[<Fact>]
let ``range from 3 to 3 has 1 element`` () =
    let result =
        LinkedList.Item (3, LinkedList.End)

    test <@ LinkedList.range 3 3 = result @>

[<Fact>]
let ``if range start is larger than stop, the result is empty`` () =
    let result:LinkedList.LinkedList<int> =
        LinkedList.End

    test <@ LinkedList.range 3 0 = result @>

[<Fact>]
let ``Length of empty list is 0`` () =
    test <@ LinkedList.length LinkedList.empty = 0 @>

[<Fact>]
let ``Length of list with 2 items is 2`` () =
    let list =
        LinkedList.Item (1, (LinkedList.Item (2, LinkedList.End)))

    test <@ LinkedList.length list = 2 @>

[<Fact>]
let ``cons prepends an item to an empty list`` () =
    let list =
        LinkedList.cons 1 LinkedList.empty

    test <@ list = (LinkedList.Item (1, LinkedList.End)) @>

[<Fact>]
let ``cons prepends an item to an existing list`` () =
    let list =
        LinkedList.cons 0 (LinkedList.range 1 2)

    let result =
        LinkedList.Item (0, LinkedList.Item (1, LinkedList.Item (2, LinkedList.End)))

    test <@ list = result @>

type ``appending an empty list does not change the original list`` () =
    let range =
        LinkedList.range 1 3
    let empty =
        LinkedList.empty

    [<Fact>]
    member x.``when the left argument is empty`` () =
        test <@ LinkedList.append empty range = range @>

    [<Fact>]
    member x.``when the right argument is empty`` () =
        test <@ LinkedList.append range empty = range @>

    [<Fact>]
    member x.``when the boths arguments are empty`` () =
        test <@ LinkedList.append empty empty = empty @>

[<Fact>]
let ``appending [4; 5] to [1; 2; 3] results in [1; 2; 3; 4; 5]`` () =
    let list1 =
        LinkedList.range 1 3
    let list2 =
        LinkedList.range 4 5
    let result =
        LinkedList.range 1 5

    test <@ LinkedList.append list1 list2 = result @>

[<Fact>]
let ``head of a list is the value of the first item`` () =
    let list =
        LinkedList.range 1 10

    test <@ LinkedList.head list = (Some 1) @>

[<Fact>]
let ``head of a list an empty list is None`` () =
    test <@ LinkedList.head LinkedList.empty = None @>

[<Fact>]
let ``tail of a list is the list without the first item`` () =
    let list =
        LinkedList.range 1 10

    let result =
        LinkedList.range 2 10

    test <@ LinkedList.tail list = result @>

[<Fact>]
let ``tail of an empty list is an empty list`` () =
    test <@ LinkedList.tail LinkedList.empty = LinkedList.empty @>

let ``map add5 function on a range increases all values by 5`` () =
    let add5 x =
        x + 5

    let list =
        LinkedList.range 0 4

    let result =
        LinkedList.range 5 9

    test <@ LinkedList.map add5 list = result @>

let ``map String.length on a list of strings returns a list of ints`` () =
    let list =
        LinkedList.Item ("hello", (LinkedList.Item ("World", LinkedList.End)))

    let result =
        LinkedList.Item (4, (LinkedList.Item (5, LinkedList.End)))

    test <@ LinkedList.map String.length list = result @>
