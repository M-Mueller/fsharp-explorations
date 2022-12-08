#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Directory =
    | Directory of string * Directory list
    | File of string * int


let parseListOutput (lines: string list) : Directory list * string list =
    let endOfOutputIndex =
        lines
        |> Seq.tryFindIndex (fun line -> line.StartsWith("$"))
        |> Option.defaultValue lines.Length

    let output, remaining = List.splitAt endOfOutputIndex lines

    let content =
        output
        |> List.map (fun (line: string) ->
            match line.Split(" ", 2) with
            | [| "dir"; name |] -> Directory(name, [])
            | [| size; name |] -> File(name, int size)
            | _ -> failwithf $"Invalid input: unsupported ls output {line}")


    content, remaining

let parseCommandsIntoPathMap (lines: string list) =
    let mutable lines = lines
    let mutable currentDirectory = []
    let mutable contentMap: Map<string list, Directory list> = Map.empty

    while not (List.isEmpty lines) do
        let command = List.head lines
        lines <- List.tail lines

        if command.StartsWith("$ cd") then
            let path = command.Split(" ", 3)[2]

            if path = ".." then
                currentDirectory <- List.tail currentDirectory
            else
                currentDirectory <- path :: currentDirectory
        else if command = "$ ls" then
            let content, remaining = parseListOutput lines
            lines <- remaining
            contentMap <- contentMap.Add(currentDirectory, content)
        else
            failwithf $"Invalid input: command not supported {command}"

    contentMap

let rec buildDirectoryFromMap (path: string list) (contentMap: Map<string list, Directory list>) : Directory =
    match contentMap.TryFind path with
    | Some contents ->
        let resolvedContent =
            contents
            |> List.map (fun entry ->
                match entry with
                | Directory(name, []) -> buildDirectoryFromMap (name :: path) contentMap
                | File(name, size) -> File(name, size)
                | _ -> failwithf "Invalid state, directory {path} should be empty")

        Directory(List.head path, resolvedContent)

    | None -> failwithf "Path {path} doesn't exist in the map"


let rec totalSize (dir: Directory) : int =
    match dir with
    | Directory(_, content) -> content |> List.map totalSize |> List.sum
    | File(_, size) -> size


let rec totalDirectorySizes (dir: Directory) : int list =
    let size = totalSize dir

    match dir with
    | Directory(name, children) ->
        let childrenSizes = children |> List.map totalDirectorySizes |> List.concat

        size :: childrenSizes
    | File(_, _) -> []



let rec sumOfTotalDirectorySizes (threshold: int) (dir: Directory) =
    dir
    |> totalDirectorySizes
    |> List.filter (fun size -> size <= threshold)
    |> List.sum


let sumOfTotalSizesFromCommands (lines: string seq) =
    lines
    |> Seq.toList
    |> parseCommandsIntoPathMap
    |> buildDirectoryFromMap [ "/" ]
    |> sumOfTotalDirectorySizes 100000


let sizeOfDeletedDirectory (lines: string seq) =
    let root =
        lines |> Seq.toList |> parseCommandsIntoPathMap |> buildDirectoryFromMap [ "/" ]

    let totalSpace = 70000000
    let updateSize = 30000000
    let freeSpace = totalSpace - (totalSize root)

    let requiredAdditionalSpace = updateSize - freeSpace

    root
    |> totalDirectorySizes
    |> List.filter (fun size -> size >= requiredAdditionalSpace)
    |> List.sort
    |> List.head


//////////////////////////
// Tests
//////////////////////////

let runTests =
    test <@ parseListOutput [ "dir a"; "2557 g" ] = ([ Directory("a", []); File("g", 2557) ], []) @>

    test
        <@ parseListOutput [ "dir a"; "2557 g"; "$ cd .." ] = ([ Directory("a", []); File("g", 2557) ], [ "$ cd .." ]) @>

    test <@ parseListOutput [ "$ cd .." ] = ([], [ "$ cd .." ]) @>
    test <@ parseListOutput [] = ([], []) @>

    let commands_simple =
        [ "$ cd /"; "$ ls"; "dir a"; "2557 g"; "$ cd a"; "$ ls"; "123 f"; "$ cd .." ]

    let directoryMap_simple =
        Map [ [ "/" ], [ Directory("a", []); File("g", 2557) ]; [ "a"; "/" ], [ File("f", 123) ] ]

    test <@ parseCommandsIntoPathMap commands_simple = directoryMap_simple @>

    let directory_simple =
        Directory("/", [ Directory("a", [ File("f", 123) ]); File("g", 2557) ])

    test <@ buildDirectoryFromMap [ "/" ] directoryMap_simple = directory_simple @>


    let commands = IO.File.ReadLines("day07.test.in") |> Seq.toList
    let directoryMap = parseCommandsIntoPathMap commands

    let directory_e = Directory("e", [ File("i", 584) ])

    let directory_a =
        Directory("a", [ directory_e; File("f", 29116); File("g", 2557); File("h.lst", 62596) ])

    let directory_d =
        Directory("d", [ File("j", 4060174); File("d.log", 8033020); File("d.ext", 5626152); File("k", 7214296) ])

    let directory_root =
        Directory("/", [ directory_a; File("b.txt", 14848514); File("c.dat", 8504156); directory_d ])

    test <@ buildDirectoryFromMap [ "/" ] directoryMap = directory_root @>

    test <@ totalSize directory_e = 584 @>
    test <@ totalSize directory_a = 94853 @>
    test <@ totalSize directory_d = 24933642 @>
    test <@ totalSize directory_root = 48381165 @>

    test <@ totalDirectorySizes directory_root = [ 48381165; 94853; 584; 24933642 ] @>

    test <@ sumOfTotalDirectorySizes 100000 directory_root = 95437 @>

    test <@ sumOfTotalSizesFromCommands commands = 95437 @>

    test <@ sizeOfDeletedDirectory commands = 24933642 @>

//////////////////////////
// AoC
//////////////////////////

let commands = IO.File.ReadLines("day07.in")

commands
|> sumOfTotalSizesFromCommands
|> printfn "Total size of directories is %d"

commands
|> sizeOfDeletedDirectory
|> printfn "Total size of deleted directory is %d"
