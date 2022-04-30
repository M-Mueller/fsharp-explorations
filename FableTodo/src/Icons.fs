module Icons

open Feliz
open Feliz.svg

let internal fromPath (path: string) =
    Svg.svg [
        svg.width 24
        svg.height 24
        svg.fill "none"
        svg.stroke "currentColor"
        svg.strokeWidth 2
        svg.children [
            Svg.path [
                svg.strokeLineCap "round"
                svg.strokeLineJoin "round"
                svg.d path
            ]
        ]
    ]

let check = fromPath "M5 13l4 4L19 7"

let edit =
    fromPath
        "M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"

let plus = fromPath "M12 4v16m8-8H4"

let save =
    fromPath "M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"

let trash =
    fromPath
        "M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"

let x = fromPath "M6 18L18 6M6 6l12 12"
