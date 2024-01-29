open Core

let filename = "lib/day3/day3.input.txt"
let lines = In_channel.read_lines filename

let lines = List.map ~f:(fun s -> "." ^ s ^ ".") lines

let lineLength = String.length (List.hd_exn lines)

let extraLine = String.make  lineLength '.'

let lines = extraLine :: lines @ [extraLine]


let rec makeField acc lst = 
  match lst with 
  | [] -> acc
  | h :: t -> makeField ( acc ^ h) t



type partNumber = { number : int ; row: int; startCol : int; endCol : int}
type symbol = {symbol : char; row : int; col : int}
type pos = {row : int; col : int}

let coordToIndex (p:pos) =  p.col + (p.row * lineLength)
let indexToCoords idx = (idx / lineLength, idx mod lineLength)

let charToInt c = int_of_char c - int_of_char '0'

let makeNumberList s = 
  let rec makeNumberListInner acc n i s start inNumber = 
    if i = String.length s then acc else
      if Char.is_digit s.[i] then begin
        if inNumber then makeNumberListInner acc (n*10 + charToInt s.[i]) (i+1) s start true
        else makeNumberListInner acc (charToInt s.[i]) (i+1) s i true
      end else 
        let currPos = indexToCoords i in
        let startPos = indexToCoords start in
        let pn = {number = n ; row = fst currPos; startCol = snd startPos; endCol = snd currPos-1} in
        if inNumber then makeNumberListInner (pn::acc) 0 (i+1)  s start false
        else makeNumberListInner acc 0 (i+1) s start false
      in makeNumberListInner [] 0 0 s 0 false
      
let isSymbol c = not (( Char.is_digit c) || (Char.equal c '.'))
      
let makeSymbolList s = 
  let rec makeSymbolListInner acc i s = 
    if i = String.length s then acc
    else
      if isSymbol s.[i] then begin
        let pos = indexToCoords i in
        let symbol = {symbol = s.[i]; row = fst pos; col =snd pos } in
        makeSymbolListInner (symbol::acc) (i+1) s
      end else 
        makeSymbolListInner acc (i+1) s
      in
      makeSymbolListInner [] 0 s
          
let posOfInts r c = 
  {row = r; col = c}
            
let makePnNeighbours pn =
  let length = pn.endCol - pn.startCol + 3 in
  let rowAbove = pn.row - 1 in
  let rowBelow = pn.row + 1 in
  let startCol = pn.startCol -1 in
  let neighboursAbove = List.init length ~f:(fun col  -> posOfInts rowAbove (col + startCol)) in
  let neighboursBelow = List.init length ~f:(fun col -> posOfInts rowBelow (col + startCol)) in
  neighboursAbove @ neighboursBelow @ [posOfInts pn.row startCol; posOfInts pn.row (startCol+length-1)]
              
let makePnPositions pn =
  let length = pn.endCol - pn.startCol + 1 in
  List.init length ~f:(fun col  -> posOfInts pn.row (col + pn.startCol)) 
              

let makeSymbolNeighbours (sym:symbol)  =
  let neighboursAbove = List.init 3 ~f:(fun col -> posOfInts (sym.row-1) (col + sym.col-1)) in
  let neighboursBelow = List.init 3 ~f:(fun col -> posOfInts (sym.row+1) (col + sym.col-1)) in
  neighboursAbove @ neighboursBelow @ [posOfInts sym.row (sym.col-1); posOfInts  sym.row (sym.col+1)]
              
let positionHasSymbol pos grid = 
  let index = coordToIndex pos in
  isSymbol grid.[index]

let posIsEqual p1 p2 =
  p1.row = p2.row && p1.col = p2.col
                
let neighboursHasSymbol neighbours grid = 
  List.exists ~f:(fun pos -> positionHasSymbol pos grid) neighbours
                  
let neighboursHasNumber neighbours number =
  let pnNeighbours = makePnPositions number in
  List.exists ~f:(fun sPos -> List.exists ~f:(fun pnPos -> posIsEqual sPos pnPos) pnNeighbours) neighbours

let grid = makeField "" lines

let numbers = makeNumberList grid

let isValidPartNumber partNumber grid =
  let neighbours = makePnNeighbours partNumber in
  neighboursHasSymbol neighbours grid

let validPartNumbers = List.filter ~f:(fun pn -> isValidPartNumber pn grid) numbers

let resultP1 = List.fold ~init:0 ~f:(fun sum pn -> sum + pn.number ) validPartNumbers

let isGear sym = 
  Char.equal sym.symbol '*'

let gears = List.filter ~f:(isGear) (makeSymbolList grid)


let findNeighbouringPartNumbers (gear:symbol) (numbers:partNumber list) =
  let gearNeighbours = makeSymbolNeighbours gear in 
  List.filter ~f:(fun number -> neighboursHasNumber gearNeighbours number) numbers

let ratio lst = 
  match lst with 
  | a::b::_ -> a.number * b.number
  | _ -> failwith "Must be two elements in list"

let gearRatios gears numbers = 
  let gearNeighbours = List.map ~f:(fun gear -> findNeighbouringPartNumbers gear numbers) gears in
  let validGears = List.filter ~f:(fun neighbouringNums -> List.length neighbouringNums = 2) gearNeighbours in
  List.fold ~init:0 ~f:(+) (List.map ~f:(ratio) validGears)

let resultP2 = gearRatios gears numbers