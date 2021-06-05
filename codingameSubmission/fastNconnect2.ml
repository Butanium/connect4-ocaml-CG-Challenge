(* Drop chips in the columns. *)
(* Connect at least 4 of your chips in any direction to win. *)
exception End_game;;
let stringList a = let r = ref "[" in
List.iter (fun x -> r := !r^", "^string_of_int x) a; !r^" ]";;
let f = float_of_int;;
let newId = ref 1;;
type baseNode = {id:int; mutable win:float; mutable visit:float; turn: int; mutable sons: mctNode list; mutable possAct: int list} and
actNode = R | N of {action:int; father:mctNode} and
mctNode = {base: baseNode; actNode: actNode};;
let nodeString n = let b = n.base in
    Printf.sprintf "id : %d | win : %.2f | visit : %.2f | turn : %d |\nsonLength : %d | possActLength : %d | R : \n%s\n"
    b.id b.win b.visit b.turn (List.length b.sons) (List.length b.possAct) (match n.actNode with
        | R -> "ROOT" | N {action;father={base={id;turn;_};_}} -> Printf.sprintf "N action : %d, fatherId : %d | fatherTurn : %d" action id turn)
in
let fatherString = function
    | {actNode=N {father;_};_} -> nodeString father
    | _ -> "ROOT"
in
let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr in
let mapiInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f i x) arr in
(* myid: 0 or 1 (Player 0 plays first) *)
(* oppid: if your dex is 0, this will be 1, and vice versa *)
let myId, oppId = Scanf.sscanf (input_line stdin) " %d  %d" (fun myid oppid -> (myid, oppid))  in
let gameArray = Array.make_matrix 7 9 (-1)  in
let mctArray = Array.make_matrix 7 9 (-1)  in
let playoutArray = Array.make_matrix 7 9 (-1)  in
let resetPlayout() =
    for i = 0 to 6 do
        mapiInPlace (fun j _ -> mctArray.(i).(j)) playoutArray.(i)
    done
in
let resetMct() =
    for i = 0 to 6 do
        mapiInPlace (fun j _ -> gameArray.(i).(j)) mctArray.(i)
    done
in
(**[getActions turn board]*)
let getActions turn board =
    let res = ref @@ if turn = 1 then [-2] else [] in
    for i = 0 to 8 do
        try (if board.(6).(i) = -1 then res := i :: !res) with Invalid_argument _ -> failwith "getActions failed"
    done;
    !res
in
let getRandomAction turn board =
    try (
    let arr = Array.make 10 (-2) in
    let index = ref 0 in
    if turn = 1 then index := 1;
    for i = 0 to 8 do
        if board.(6).(i) = -1 then (
            arr.(!index) <- i;
            incr index
        )
    done;
    if !index=0 then raise End_game;
    arr.(Random.int !index)
    ) with Invalid_argument x -> failwith @@ "getRandomAction failed "^x
in

let toChar = function | 0 -> '0' | 1 -> '1' | -1 -> '.' | e -> failwith @@ "unexpected int action : "^string_of_int e in
let printGame arr =
        let rec aux i = if i=7 then () else (aux (i+1);
            prerr_endline @@ Array.fold_left (fun acc x -> acc ^ Printf.sprintf " %c |" (toChar x))"" arr.(i))
        in aux 0
in
let toCell = function
    | '0' -> 0
    | '1' -> 1
    | _ -> -1
in
let convertRow str =
    let r = Array.make 9 (-1) in
    for j = 0 to 8 do
        try (
        r.(j) <- toCell str.[j]
        ) with Invalid_argument _ -> failwith "found bug"
    done;
    r
in
let replace arr =
    let rec aux arr i = if i>=9 then (prerr_endline "failed replace on :"; printGame arr; raise @@ Invalid_argument "can't replace") else
        let x =  arr.(0).(i) in
            if x > -1 then arr.(0).(i) <- 1-x else aux arr (i+1)
    in aux arr 0
in
let exist i j = i>=0 && j>=0 && i < 7 && j < 9
in
let add arr player j =
    let rec aux i =
        if arr.(i).(j) = -1 then (arr.(i).(j) <- player; i) else aux (i+1)
    in try (aux 0) with Invalid_argument _ ->(prerr_endline "error at ADD : "; printGame arr;
        raise @@ Invalid_argument (Printf.sprintf "invalid action %d" j))
in

let tests = [(0,1);(1,1);(1,-1);(1,0)]
in
let inv (x,y) = -x,-y
in
let check arr i j player =
    let rec aux ((dirl, dirc) as dir) l c depth rev =
        if exist l c && depth<4 && arr.(l).(c) = player then (
           aux dir (l+dirl) (c+dirc) (depth+1) rev
        )
        else if rev && depth<4 then aux (inv dir) (i-dirl) (j-dirc) (depth) false
        else (assert(depth<=4); depth=4)
    in List.fold_left (fun acc dir -> acc || aux dir i j 0 true) false tests
in

(**[performAction arr player action]*)
let performAction arr player = function
    | -2 -> begin
                try (replace arr; false) with  _ -> failwith "replace failed"
            end
    | (j:int) -> try (let i = add arr player j in check arr i j player;) with Invalid_argument e -> raise @@ Invalid_argument  ("add/check failed"^e)
in
let performCheckAction arr player = function
    | -2 -> false
    | j -> (let i = add arr player j in
           let b = check arr i j player in
           arr.(i).(j) <- -1; b)
in

let playout initTurn =
    try (
    let turn = ref initTurn in
    resetPlayout();
    let win() = if initTurn mod 2 = !turn mod 2 then 0. else 1.
    in
    let rec wh() = try (
        let player = !turn mod 2 in
        let actions = getActions !turn playoutArray in
        let rec auxW player = function
            | x :: xs ->  if performCheckAction playoutArray player x then x else auxW player xs
            | _ -> -66
        in
        if auxW player actions != -66 then win()
        else (
        let r = auxW (1 - player) actions in if r != -66 then (
            (* à remplacer si remove *)
            assert(not @@ performAction playoutArray player r);
            incr turn;
            wh()
        )
        else (
            let t = getRandomAction !turn playoutArray in
            assert(not @@ performAction playoutArray player t);
            incr turn;
            wh()
        )
    )
    ) with End_game -> 0.5
    in wh();) with Invalid_argument e -> raise @@ Invalid_argument ("playout failed"^e)
in
let getActionFromNode = function
    | {actNode= N {action;_};_} -> action
    | _ -> raise @@ Invalid_argument "can't get action from ROOT"
in
let rec retropagation node score =
    node.base.win <- node.base.win +. score;
    node.base.visit <- node.base.visit +. 1.;
    match node.actNode with
        | R -> ()
        | N {action; father} -> retropagation father (1. -. score)
in
let expend node =
    match node.base.possAct with
        | x :: xs -> node.base.possAct <- xs;
            let b = (try (performAction mctArray (node.base.turn mod 2) x) with Invalid_argument e ->
                                    prerr_endline @@Printf.sprintf "errExpendNode : %s\nfather : %s" (nodeString node) (fatherString node);
                                    prerr_endline @@ Printf.sprintf "actions : %s" @@ stringList (x::xs);
                                    raise (Invalid_argument ("exepend failed, " ^ e )))
            in
            let possAct =  getActions (node.base.turn+1) mctArray in
            let newN = {base={id= !newId;win=0.; visit=0.; turn=node.base.turn+1;sons=[];
                possAct}; actNode = N {action=x; father=node}} in
            incr newId;
            node.base.sons <- newN :: node.base.sons;
            if b then (
                retropagation newN 1.
            ) else retropagation newN @@ playout newN.base.turn
        | _ ->  raise End_game
in
let performActionNode arr = function
    | {actNode=N {action;_};base={turn; _}} as n -> begin try (performAction arr (1 - turn mod 2) action)
        with Invalid_argument e -> raise @@ Invalid_argument ("performNode failed at \n"^nodeString n ^ e)
        end
    | _ -> false
in
let ucb1 node = let nf = (match node.actNode with | N {father={base={visit;_};_}} -> visit) in
     node.base.win /. node.base.visit +. (2. *. log nf /. node.base.visit)**0.5
in
let rec select node =
    if performActionNode mctArray node then retropagation node 1. else (
        match node.base.possAct, node.base.sons with
            | [], [] -> retropagation node 0.5
            | [], x :: xs -> let _, n = List.fold_left
                (fun ((accS, accN) as acc) n -> let s = ucb1 n in if s > accS then s, n else acc) (ucb1 x, x) xs
                in select n
            | _ -> expend node
    )
in
let debugNode n =
    prerr_endline @@ Printf.sprintf "win : %.2f | visit : %.2f | rate : %.3f | action : %d"
        n.base.win n.base.visit  (n.base.win /. n.base.visit) (try(getActionFromNode n) with Invalid_argument _ -> -66)
in
let debugTree node =
    let rec aux node =
        prerr_endline @@ nodeString node;
        match node.base.sons with
        | x :: xs -> aux x
        | [] -> ()
    in
    prerr_endline "___start MCTS leaf___ ";aux node; prerr_endline "________";
in
let mctSearch turn stopTime =
    newId := 1;
    let debug = ref 0. in
    let mcTree = {base = {id=0; win=0.; visit=0.; turn; sons=[]; possAct=getActions turn gameArray}; actNode=R}
    in

    while Sys.time () < stopTime do
        resetMct();
        select mcTree;
        debug := 1. +. !debug
    done;
    prerr_endline @@ Printf.sprintf "win rate : %.3f" @@ 1. -. mcTree.base.win /. mcTree.base.visit;
    begin
    match mcTree.base.sons with
        | x :: xs as l ->
            List.iter debugNode l;
            let _, n = List.fold_left
                (fun ((accS, accN) as acc) n -> let s = n.base.win /. n.base.visit in if s > accS then s, n else acc)
                (x.base.win/. x.base.visit, x) xs
            in
            prerr_endline "\nchosen node :";
            debugNode n;
            prerr_endline @@ Printf.sprintf "\nPLAYOUTS : %.0f" !debug;
            let action = getActionFromNode n in
            begin
            try (
            performAction gameArray myId action;
            ) with Invalid_argument e -> raise @@ Invalid_argument ("perform chosen node failed"^e)
            end;
            action, !debug

        | _ -> failwith "no sons for root"
    end;
in




(* game loop *)
while true do
    let t = Sys.time () in
    let turnindex = int_of_string (input_line stdin) in (* starts from 0; As the game progresses, first player gets [0,2,4,...] and second player gets [1,3,5,...] *)

    for i = 0 to 6 do
        let _ = input_line stdin in ()(* one row of the board (from top to bottom) *)
     done;

    let numvalidactions = int_of_string (input_line stdin) in (* number of unfilled columns in the board *)
    for i = 0 to numvalidactions - 1 do
        let _ =  (input_line stdin) in (* a valid column index into which a chip can be dropped *)
        ()
    done;

    let oppAction = int_of_string (input_line stdin) in (* opponent's previous chosen column index (will be -1 for first player in the first turn) *)

    if turnindex > 0 then (
        let _ = performAction gameArray oppId oppAction
        in
        prerr_endline @@"turn : "^string_of_int turnindex;
    );
    prerr_endline "starting mcts...";


    let a, d = mctSearch turnindex (t +. 0.1) in
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* Output a column index to drop the chip in. Append message to show in the viewer. *)
    print_endline @@ string_of_int a ^ Printf.sprintf " %.0f playouts !" d ;
    ();
done;;
