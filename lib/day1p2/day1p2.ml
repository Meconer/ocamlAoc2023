open Core

(* let filename = "lib/day1p2/day1.example2.txt" *)
let filename = "lib/day1/day1.input.txt"
let day1Input = In_channel.read_all filename

let lines = String.split_lines day1Input

let startsWith s1 s2 = 
  if ( String.length s2 > String.length s1) then false
  else let start = String.sub ~pos:0 ~len:(String.length s2) s1
in
 if String.compare start s2 = 0 then true 
else false

let removeFirstChar s = 
  String.sub s ~pos:1 ~len:(String.length s - 1)

let firstElem lst = 
  match lst with
  | [] -> failwith "Empty list"
  | h::_ -> h

let toLookFor = [
  ( "one" , 1) ;
  ( "1", 1);
  ( "two", 2); 
  ( "2" , 2 );
  ( "three" , 3 );
  ( "3" , 3 );
  ( "four" , 4 );
  ( "4" , 4 );
  ( "five" , 5 );
  ( "5" , 5 );
  ( "six" , 6 );
  ( "6" , 6 );
  ( "seven" , 7 );
  ( "7" , 7 );
  ( "eight" , 8 );
  ( "8" , 8 );
  ( "nine" , 9 );
  ( "9" , 9 ) ] 

let rec firstNumber s = 
  let matches = List.filter ~f:(fun (text,_) -> startsWith s text) toLookFor in
  if List.is_empty matches then firstNumber (removeFirstChar s)
  else let (_,num) = firstElem matches in num

let lastNumber s = 
  
  let rec helper s =
  let matches = List.filter ~f:(fun (text,_) -> startsWith s (String.rev text)) toLookFor in
  if List.is_empty matches then helper (removeFirstChar s)
  else let (_,num) = firstElem matches in num

in helper (String.rev s)

let charToIntVal c = int_of_char c - int_of_char '0'

let firstElem lst = 
  match lst with
    | h::_ -> h
    | _ -> failwith "Error in firstDigit"


let firsts = List.map ~f:firstNumber lines
let lasts = List.map ~f:lastNumber lines

let lNumbers =  List.map2_exn ~f:(fun a b -> a * 10 + b) firsts lasts

let result = List.fold_left ~init:0 ~f:(+)  lNumbers