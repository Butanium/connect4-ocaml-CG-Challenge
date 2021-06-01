(* Drop chips in the columns. *)
(* Connect at least 4 of your chips in any direction to win. *)
type node = {turn: int; sons: (node * extNode) list;actions: int list} and
extNode = R | F of node*extNode;;
let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr in
let mapiInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f i x) arr in
(* myid: 0 or 1 (Player 0 plays first) *)
(* oppid: if your dex is 0, this will be 1, and vice versa *)
let myId, oppId = Scanf.sscanf (input_line stdin) " %d  %d" (fun myid oppid -> (myid, oppid))  in
let initArray = Array.create_matrix 7 9 (-1)  in
let mctArray = Array.create_matrix 7 9 (-1)  in
let playoutArray = Array.create_matrix 7 9 (-1)  in
let resetPlayout() = for i = 0 to 6 do mapiInPlace (fun j _ -> mctArray.(i).(j)) playoutArray.(i)
done in
let initMct() = for i = 0 to 6 do mapiInPlace (fun j _ -> initArray.(i).(j)) mctArray.(i)
done in
let getActions turn board =
    let res = ref @@ if turn = 1 then [-2] else [] in
    for i = 0 to 9 do
        if board.(6).(i) = -1 then res := i :: !res
    done;
    !res
in
let rootNode = ref (turn=0; sons=[]; actions=getActions 0 initArray},R) in
let conv = function | 0 -> '0' | 1 -> '1' | _ -> '.' in
let printGame arr =
        let rec aux i = if i=7 then () else (aux (i+1);
            prerr_endline @@ Array.fold_left (fun acc x -> acc ^ Printf.sprintf " %c |" (conv x))"" arr.(i))
        in aux 0
in
(*let trans = function*)
(*    | '0' -> 0*)
(*    | '1' -> 1*)
(*    | _ -> -1*)
(*in*)
(*let updateRow i str =*)
(*    for j = 0 to 8 do*)
(*        initArray.(i).(j) <- trans str.[j]*)
(*    done;*)
(*in*)
let replace arr =
    let rec aux arr i = if i>=9 then failwith "can't replace" else
        let x =  arr.(0).(i) in
            if x = 0 then arr.(0).(i) <- 1 else aux arr (i+1)
    in aux arr 0
in
let exist i j = i>=0 && j>=0 && i < 7 && j < 9
in
let add arr player j =
    let rec aux i =
        if arr.(i).(j) = -1 then (arr.(i).(j) <- player; i) else aux (i+1)
    in try (aux 0) with Invalid_argument _ ->(prerr_endline "error at : "; printGame arr;
        failwith @@ Printf.sprintf "invalid action %d" j)
in

let tests = [(0,1);(1,1);(1,-1);(1,0)]
in
let inv (x,y) = -x,-y
in
let check arr i j player =
    let rec aux ((dirl, dirc) as dir) l c depth rev =
        if exist l c && depth<4 then (
            if arr.(l).(c) = player then aux dir (l+dirl) (c+dirc) (depth+1) rev
            else if rev then aux (inv dir) (i-dirl) (j-dirc) (depth-1) false else false
        )
        else (assert(depth<=4); depth=4)
    in List.fold_left (fun acc dir -> acc || aux dir i j 1 true) false tests
in
let performAction arr player = function
    | -2 -> replace arr; false
    | j -> let i = add arr player j in check arr i j player;
in

(* game loop *)
while true do
    let turnindex = int_of_string (input_line stdin) in (* starts from 0; As the game progresses, first player gets [0,2,4,...] and second player gets [1,3,5,...] *)
    for i = 0 to 6 do
        let _ = input_line stdin in (* one row of the board (from top to bottom) *)
        ()
     done;

    let numvalidactions = int_of_string (input_line stdin) in (* number of unfilled columns in the board *)
    for i = 0 to numvalidactions - 1 do
        let _ =  (input_line stdin) in (* a valid column index into which a chip can be dropped *)
        ()
    done;

    let oppAction = int_of_string (input_line stdin) in (* opponent's previous chosen column index (will be -1 for first player in the first turn) *)
    performAction oppId;
    advance oppAction
    if turn>0 then
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* Output a column index to drop the chip in. Append message to show in the viewer. *)
    print_endline "0";
    ();
done;
