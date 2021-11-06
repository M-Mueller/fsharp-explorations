namespace Equations

module ExpressionTree =
    type ExpressionTree =
        | Addition of ExpressionTree * ExpressionTree
        | Subtraction of ExpressionTree * ExpressionTree
        | Multiplication of ExpressionTree * ExpressionTree
        | Division of ExpressionTree * ExpressionTree
        | Number of int

    /// Apply the given function to the right most leaf of the tree
    let rec mapRight (f:ExpressionTree -> ExpressionTree) tree =
        match tree with
        | Addition (l, r) -> Addition (l, mapRight f r)
        | Subtraction (l, r) -> Subtraction (l, mapRight f r)
        | Multiplication (l, r) -> Multiplication (l, mapRight f r)
        | Division (l, r) -> Division (l, mapRight f r)
        | Number n -> f (Number n)

    let rec evaluate node =
        match node with
        | Addition (left, right) -> (evaluate left) + (evaluate right)
        | Subtraction (left, right) -> (evaluate left) - (evaluate right)
        | Multiplication (left, right) -> (evaluate left) * (evaluate right)
        | Division (left, right) -> (evaluate left) / (evaluate right)
        | Number x -> x

    let fromString (str:string) =
        /// Converts a char into an int by assuming the char is between 48 ('0') and 57 ('9')
        let charToInt (c:char) = (int c) - 48

        /// Returns whether the given char is a digit
        let isDigit (c:char) = c >= '0' && c <= '9'

        /// Returns whether the given char is consider whitespace
        let isWhitespace (c:char) = c = ' ' || c = '\t'

        /// Reads digits from chars and converts them to a number
        /// > parseRemainingDigits 4 ['1', '6', ' ', '+', '2']
        ///   val it : int * list<char> = 416 [' ', '+', '2']
        let rec parseRemainingDigits number (chars:list<char>) =
            match chars with
            | x :: xs ->
                match x with
                | i when isDigit i -> parseRemainingDigits (number * 10 + (charToInt i)) xs
                | _ -> (number, chars)
            | [] -> (number, chars)

        /// Parse a Number from chars
        let rec parseNumber (chars:list<char>) =
            match chars with
            | x :: xs ->
                match x with
                | i when isDigit i -> 
                    let (n, xs) =
                        parseRemainingDigits (charToInt i) xs

                    (Number n, xs)
                | x when isWhitespace x -> parseNumber xs
                | _ -> failwithf "Expected a number but found '%c'" x
            | [] -> failwithf "Expected a number but there are no more characters"

        /// Parse an operation from chars and add it to the tree
        let rec parseOperation tree (chars:list<char>) =
            match chars with
            | x :: xs ->
                match x with
                | '+' ->
                    let (right, xs) =
                        parseNumber xs

                    parseOperation (Addition (tree, right)) xs
                | '-' ->
                    let (right, xs) =
                        parseNumber xs

                    parseOperation (Subtraction (tree, right)) xs
                | '*' ->
                    let (right, xs) =
                        parseNumber xs

                    parseOperation (mapRight (fun n -> Multiplication (n, right)) tree) xs
                | '/' ->
                    let (right, xs) =
                        parseNumber xs

                    parseOperation (mapRight (fun n -> Division (n, right)) tree) xs
                | x when isWhitespace x -> parseOperation tree xs
                | i when isDigit i -> failwithf "Expected an operation but got %c" x
                | _ -> failwithf "Unsupported character '%c'" x
            | [] -> tree

        let (initialTree, chars) =
            parseNumber (Array.toList (str.ToCharArray ()))

        parseOperation initialTree chars
