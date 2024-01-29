open Core

let filename = "lib/day1/day1.input.txt"
let day1Input = In_channel.read_all filename

let lines = String.split_lines day1Input

let lineLists = List.map ~f:( String.to_list ) lines

let charToIntVal c = int_of_char c - int_of_char '0'

let digitsAsChars = List.map ~f:( List.filter ~f:(Char.is_digit))  lineLists
let digitsAsInts = List.map ~f:(List.map ~f:(charToIntVal)) digitsAsChars

let firstElem lst = 
  match lst with
    | h::_ -> h
    | _ -> failwith "Error in firstDigit"

let rec lastElem lst = 
  match lst with 
  | [] -> failwith "Empty list"
  | [e] -> e
  | _::t -> lastElem t


let firsts = List.map ~f:firstElem digitsAsInts
let lasts = List.map ~f:lastElem digitsAsInts

let lNumbers =  List.map2_exn ~f:(fun a b -> a * 10 + b) firsts lasts

let result = List.fold_left ~init:0 ~f:(+)  lNumbers