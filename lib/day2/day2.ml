open Core

let filename = "lib/day2/day2.input.txt"
let lines = In_channel.read_lines filename

type color = Red | Green | Blue

type cubeType = {red: int; green : int ; blue : int}

let firstElem lst = 
  match lst with
    | h::_ -> h
    | _ -> failwith "Error in firstDigit"

let replaceSemiColonWithComma s = String.substr_replace_all ~pattern:";" ~with_:"," s
let splitOnComma s = String.split ~on:',' s 
let splitOnSpace s = String.split ~on:' ' s 
let splitOnColon s = String.split ~on:':' s 

let getGameIdFromPart s = match splitOnSpace s with
  | _ :: idText :: _ ->   int_of_string idText
  | _ :: [] -> failwith "Error in gameIdText"
  | _ -> failwith "Error in gameIdText"

let getGameId s = match splitOnColon s with 
  | gameIdText :: _ -> getGameIdFromPart gameIdText
  | _ -> failwith "Error in getGameId"

(* n color for example 3 blue*)
let getColorAndNumberFromPart s = 
  let parts = splitOnSpace s in
  match parts with
  | h::cPart::_ -> 
            let col = match cPart with
            | "red" -> Red
            | "blue" -> Blue
            | "green" -> Green
            | _ -> failwith "Unknown color" in

            let numberOfCubes = int_of_string h in (col,numberOfCubes)
  | _ -> failwith "Err in getColorFromPart"

  let getGamePartList s = match splitOnColon s with 
  | _ :: gamePart :: _ -> List.map ~f:(String.strip) (splitOnComma (replaceSemiColonWithComma gamePart))
  | _ :: [] -> failwith "Error in getGamePartList"
  | [] -> failwith "Error in getGamePartList"

  let getMaxCubes s =
    let rec inner maxRed maxGreen maxBlue lst =
      match lst with 
      | [] -> (maxRed, maxGreen, maxBlue)
      | h :: t -> match getColorAndNumberFromPart h with 
              | (Red, num) -> inner (max maxRed num) maxGreen maxBlue t
              | (Green, num) -> inner maxRed (max maxGreen num) maxBlue t
              | (Blue, num) -> inner maxRed  maxGreen (max maxBlue num) t
    in
      let lst = getGamePartList s in
      inner 0 0 0 lst

let availableRedCubes = 12
let availableGreenCubes = 13
let availableBlueCubes = 14
let isValid maxCubes = 
  let (maxRed, maxGreen, maxBlue) = maxCubes
in maxRed <= availableRedCubes && maxGreen <= availableGreenCubes && maxBlue <= availableBlueCubes

let gameIds = List.map ~f:getGameId lines

let maxCubeList = List.map ~f:getMaxCubes lines
let valids = List.map ~f:isValid maxCubeList

let validGameIds = List.map2_exn ~f:(fun id valid -> if valid then id else 0) gameIds valids

let result1 = List.fold_left ~f:(+) ~init:0 validGameIds 

let powerOfCubes cubes = 
  let (reds,greens,blues) = cubes in
  reds * greens * blues

let result2 = List.fold_left ~f:(+) ~init: 0 (List.map ~f:powerOfCubes maxCubeList)

