#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


type Packet =
    | LiteralValue of LiteralValue
    | Operator of Operator

and LiteralValue =
    { version: int
      typeId: int
      value: int64 }

and Operator =
    { version: int
      typeId: OperatorType
      lengthTypeId: int
      subPackets: Packet list }

and OperatorType =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type Binary = Binary of string


let hexToBinary (str: string) : Binary =
    str
    |> Seq.map
        (fun c ->
            match c with
            | '0' -> "0000"
            | '1' -> "0001"
            | '2' -> "0010"
            | '3' -> "0011"
            | '4' -> "0100"
            | '5' -> "0101"
            | '6' -> "0110"
            | '7' -> "0111"
            | '8' -> "1000"
            | '9' -> "1001"
            | 'A' -> "1010"
            | 'B' -> "1011"
            | 'C' -> "1100"
            | 'D' -> "1101"
            | 'E' -> "1110"
            | 'F' -> "1111"
            | _ -> failwithf "Not a hex number: %c" c)
    |> Seq.fold (+) ""
    |> Binary


let binaryToDecimal (Binary binary) = Convert.ToInt32(binary, 2)
let binaryToLargeDecimal (Binary binary) = Convert.ToInt64(binary, 2)


let rec parsePacket (Binary binary) : (Packet * Binary) =
    let parseLiteral version typeId (Binary content) : (Packet * Binary) =
        assert (typeId = 4)

        let rec parseGroups total (remaining: string) =
            let prefix = remaining.[0]
            let number = remaining.[1..4]
            let remaining = remaining.[5..]

            if prefix = '1' then
                parseGroups (total + number) remaining
            else
                (Binary(total + number), remaining)

        let value, remaining = parseGroups "" content

        let literal =
            { version = version
              typeId = typeId
              value = binaryToLargeDecimal value }

        (LiteralValue literal, Binary remaining)


    let parseOperatorType0 version typeId (Binary content) : Packet * Binary =
        let totalLength = binaryToDecimal (Binary content.[0..14])
        let content = content.[15..]

        let rec parseSubPackets (subPackets: Packet list) (remaining: Binary) =
            let packet, (Binary remaining) = parsePacket remaining

            if String.length remaining = 0 then
                subPackets @ [ packet ]
            else
                parseSubPackets (subPackets @ [ packet ]) (Binary remaining)

        (Operator
            { version = version
              typeId = typeId
              lengthTypeId = 0
              subPackets = parseSubPackets [] (Binary content.[0..totalLength - 1]) },
         Binary content.[totalLength..])


    let parseOperatorType1 version typeId (Binary content) : Packet * Binary =
        let numSubPackets = binaryToDecimal (Binary content.[0..10])
        let content = Binary content.[11..]

        let rec parseSubPackets count subPackets remaining =
            if count = 0 then
                (subPackets, remaining)
            else
                let packet, remaining = parsePacket remaining
                parseSubPackets (count - 1) (subPackets @ [ packet ]) remaining

        let subPackets, remaining =
            parseSubPackets numSubPackets [] content

        (Operator
            { version = version
              typeId = typeId
              lengthTypeId = 1
              subPackets = subPackets },
         remaining)


    let parseOperator version typeId (Binary content) : Packet * Binary =
        let lengthTypeId = content.[0]

        if lengthTypeId = '0' then
            parseOperatorType0 version typeId (Binary content.[1..])
        else
            parseOperatorType1 version typeId (Binary content.[1..])


    if String.length binary < 6 then
        failwithf "Remaining binary is too short for a packet"
    else
        let version = binaryToDecimal (Binary binary.[0..2])
        let typeId = binaryToDecimal (Binary binary.[3..5])
        let content = Binary binary.[6..]

        match typeId with
        | 0 -> parseOperator version Sum content
        | 1 -> parseOperator version Product content
        | 2 -> parseOperator version Minimum content
        | 3 -> parseOperator version Maximum content
        | 4 -> parseLiteral version typeId content
        | 5 -> parseOperator version GreaterThan content
        | 6 -> parseOperator version LessThan content
        | 7 -> parseOperator version EqualTo content
        | _ -> failwithf "Unsupported typeId %d" typeId


let rec sumPacketVersions packet =
    match packet with
    | LiteralValue literal -> literal.version
    | Operator operator ->
        operator.version
        + (operator.subPackets
           |> List.map sumPacketVersions
           |> List.sum)


let parseAndSumPacketVersions =
    hexToBinary
    >> parsePacket
    >> fst
    >> sumPacketVersions


let rec evaluatePacket packet =
    match packet with
    | LiteralValue literal -> literal.value
    | Operator operator ->
        let subPackets =
            List.map evaluatePacket operator.subPackets

        match operator.typeId with
        | Sum -> List.sum subPackets
        | Product -> List.fold (*) 1L subPackets
        | Maximum -> List.max subPackets
        | Minimum -> List.min subPackets
        | GreaterThan ->
            if List.length subPackets = 2 then
                if subPackets.[0] > subPackets.[1] then
                    1L
                else
                    0L
            else
                failwithf "GreaterThan requires exactly 2 packets. Got %d" (List.length subPackets)
        | LessThan ->
            if List.length subPackets = 2 then
                if subPackets.[0] < subPackets.[1] then
                    1L
                else
                    0L
            else
                failwithf "LessThan requires exactly 2 packets. Got %d" (List.length subPackets)
        | EqualTo ->
            if List.length subPackets = 2 then
                if subPackets.[0] = subPackets.[1] then
                    1L
                else
                    0L
            else
                failwithf "EqualTo requires exactly 2 packets. Got %d" (List.length subPackets)


let parseAndEvaluate hex =
    hex
    |> hexToBinary
    |> parsePacket
    |> fst
    |> evaluatePacket


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let sample1 = hexToBinary "D2FE28"

    test <@ sample1 = Binary "110100101111111000101000" @>

    let sample1Packet = parsePacket sample1

    test
        <@ fst sample1Packet = LiteralValue
                                   { version = 6
                                     typeId = 4
                                     value = 2021L } @>

    test <@ snd sample1Packet = Binary "000" @>
    test <@ sumPacketVersions (fst sample1Packet) = 6 @>

    let sample2 =
        Binary "00111000000000000110111101000101001010010001001000000000"

    let sample2Packet = parsePacket sample2

    let sample2Expected =
        Operator
            { version = 1
              typeId = LessThan
              lengthTypeId = 0
              subPackets =
                  [ LiteralValue { version = 6; typeId = 4; value = 10L }
                    LiteralValue { version = 2; typeId = 4; value = 20L } ] }

    test <@ fst sample2Packet = sample2Expected @>
    test <@ sumPacketVersions (fst sample2Packet) = 9 @>

    let sample3 =
        Binary "11101110000000001101010000001100100000100011000001100000"

    let sample3Packet = parsePacket sample3

    let sample3Expected =
        Operator
            { version = 7
              typeId = Maximum
              lengthTypeId = 1
              subPackets =
                  [ LiteralValue { version = 2; typeId = 4; value = 1L }
                    LiteralValue { version = 4; typeId = 4; value = 2L }
                    LiteralValue { version = 1; typeId = 4; value = 3L } ] }

    test <@ fst sample3Packet = sample3Expected @>
    test <@ sumPacketVersions (fst sample2Packet) = 9 @>

    test <@ parseAndSumPacketVersions "8A004A801A8002F478" = 16 @>
    test <@ parseAndSumPacketVersions "620080001611562C8802118E34" = 12 @>
    test <@ parseAndSumPacketVersions "C0015000016115A2E0802F182340" = 23 @>
    test <@ parseAndSumPacketVersions "A0016C880162017C3686B18A3D4780" = 31 @>

    test <@ parseAndEvaluate "C200B40A82" = 3L @>
    test <@ parseAndEvaluate "04005AC33890" = 54L @>
    test <@ parseAndEvaluate "880086C3E88112" = 7L @>
    test <@ parseAndEvaluate "CE00C43D881120" = 9L @>
    test <@ parseAndEvaluate "D8005AC2A8F0" = 1L @>
    test <@ parseAndEvaluate "F600BC2D8F" = 0L @>
    test <@ parseAndEvaluate "9C005AC2F8F0" = 0L @>
    test <@ parseAndEvaluate "9C0141080250320F1802104A08" = 1L @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day16.in")
|> Seq.head
|> parseAndSumPacketVersions
|> printfn "The sum of all versions is %d"


IO.File.ReadLines("day16.in")
|> Seq.head
|> parseAndEvaluate
|> printfn "The expression evaluates to %d"
