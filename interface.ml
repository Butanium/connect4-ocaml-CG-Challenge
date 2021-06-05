module AI = struct
    (* Drop chips in the columns. *)
    (* Connect at least 4 of your chips in any direction to win. *)

    exception End_game
    let myId = ref 0
    let stringList a = let r = ref "[" in
    List.iter (fun x -> r := !r^", "^string_of_int x) a; !r^" ]"
    let f = float_of_int
    let newId = ref 1
    type baseNode = {id:int; mutable win:float; mutable visit:float; turn: int; mutable sons: mctNode list; mutable possAct: int list} and
    actNode = R | N of {action:int; father:mctNode} and
    mctNode = {base: baseNode; actNode: actNode}


        (**[getActions turn board]*)
    let getActions turn board =
        let res = ref @@ if turn = 1 then [-2] else [] in
        for i = 0 to 8 do
            try (if board.(6).(i) = -1 then res := i :: !res) with Invalid_argument _ -> failwith "getActions failed"
        done;
        !res
    type mctsearcher = {newId : int ref; mcTree : mctNode ref; gameArray : int array array; mctArray : int array array; playoutArray : int array array}
    let createAI() = let gameArray = Array.make_matrix 7 9 (-1) in
        {newId = ref 1; gameArray; mctArray = Array.make_matrix 7 9 (-1);
        playoutArray = Array.make_matrix 7 9 (-1);
        mcTree = ref {base = {id=0; win=0. ; visit=0. ; turn=0; sons=[]; possAct=getActions 0 gameArray}; actNode=R}}

    let nodeString n = let b = n.base in
        Printf.sprintf "id : %d | win : %.2f | visit : %.2f | turn : %d |\nsonLength : %d | possActLength : %d | R : \n%s\n"
        b.id b.win b.visit b.turn (List.length b.sons) (List.length b.possAct) (match n.actNode with
            | R -> "ROOT" | N {action;father={base={id;turn;_};_}} -> Printf.sprintf "N action : %d, fatherId : %d | fatherTurn : %d" action id turn)

    let fatherString = function
        | {actNode=N {father;_};_} -> nodeString father
        | _ -> "ROOT"

    let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr
    let mapiInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f i x) arr
    (* myid: 0 or 1 (Player 0 plays first) *)
    (* oppid: if your dex is 0, this will be 1, and vice versa *)
    let resetPlayout ai =
        for i = 0 to 6 do
            mapiInPlace (fun j _ -> ai.mctArray.(i).(j)) ai.playoutArray.(i)
        done

    let resetMct ai =
        for i = 0 to 6 do
            mapiInPlace (fun j _ -> ai.gameArray.(i).(j)) ai.mctArray.(i)
        done

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


    let toChar = function | 0 -> '0' | 1 -> '1' | -1 -> '.' | e -> failwith @@ "unexpected int action : "^string_of_int e
    let printGame arr =
            let rec aux i = if i=7 then () else (aux (i+1);
                prerr_endline @@ Array.fold_left (fun acc x -> acc ^ Printf.sprintf " %c |" (toChar x))"" arr.(i))
            in aux 0

    let toCell = function
        | '0' -> 0
        | '1' -> 1
        | _ -> -1

    let convertRow str =
        let r = Array.make 9 (-1) in
        for j = 0 to 8 do
            try (
            r.(j) <- toCell str.[j]
            ) with Invalid_argument _ -> failwith "found bug"
        done;
        r

    let replace arr =
        let rec aux arr i = if i>=9 then (prerr_endline "failed replace on :"; printGame arr; raise @@ Invalid_argument "can't replace") else
            let x =  arr.(0).(i) in
                if x > -1 then arr.(0).(i) <- 1-x else aux arr (i+1)
        in aux arr 0

    let exist i j = i>=0 && j>=0 && i < 7 && j < 9

    let add arr player j =
        let rec aux i =
            if arr.(i).(j) = -1 then (arr.(i).(j) <- player; i) else aux (i+1)
        in try (aux 0) with Invalid_argument _ ->(prerr_endline "error at ADD : "; printGame arr;
            raise @@ Invalid_argument (Printf.sprintf "invalid action %d" j))


    let tests = [(0,1);(1,1);(1,-1);(1,0)]

    let inv (x,y) = -x,-y

    let check arr i j player =
        let rec aux ((dirl, dirc) as dir) l c depth rev =
            if exist l c && depth<4 && arr.(l).(c) = player then (
               aux dir (l+dirl) (c+dirc) (depth+1) rev
            )
            else if rev && depth<4 then aux (inv dir) (i-dirl) (j-dirc) (depth) false
            else (assert(depth<=4); depth=4)
        in List.fold_left (fun acc dir -> acc || aux dir i j 0 true) false tests


    (**[performAction arr player action]*)
    let performAction arr player = function
        | -2 -> begin
                    try (replace arr; false) with  _ -> failwith "replace failed"
                end
        | (j:int) -> try (let i = add arr player j in check arr i j player;) with Invalid_argument e -> raise @@ Invalid_argument  ("add/check failed"^e)

    let performCheckAction arr player = function
        | -2 -> false
        | j -> (let i = add arr player j in
               let b = check arr i j player in
               arr.(i).(j) <- -1; b)

    let playouWinDebug = ref true
    let playout initTurn ai=
        try (
        let turn = ref initTurn in
        resetPlayout ai;
        let debugList = ref [] in

        let win() = if !playouWinDebug && !turn mod 2 = !myId then (prerr_endline "win on :";
            printGame ai.playoutArray;
            prerr_endline @@ "\n" ^ stringList (List.rev !debugList);
            playouWinDebug := false
            );
            if initTurn mod 2 = !turn mod 2 then 0. else 1.
        in
        let rec wh() = try (
            let player = !turn mod 2 in
            let actions = getActions !turn ai.playoutArray in
            let rec auxW player = function
                | x :: xs ->  if performCheckAction ai.playoutArray player x then x else auxW player xs
                | _ -> -66
            in
            let r = auxW player actions in if r!= -66 then(
                debugList := r :: !debugList;
                let _ = performAction ai.playoutArray player r in win()
            )
            else (
            let r = auxW (1 - player) actions in if r != -66 then (
                (* à remplacer si remove *)
                assert(not @@ performAction ai.playoutArray player r);
                incr turn;
                debugList := r :: !debugList;
                wh()
            )
            else (
                let t = getRandomAction !turn ai.playoutArray in
                assert(not @@ performAction ai.playoutArray player t);
                incr turn;
                debugList := t :: !debugList;
                wh()
            )
        )
        ) with End_game -> if initTurn mod 2 = !myId then 0. else 1.
        in wh();) with Invalid_argument e -> raise @@ Invalid_argument ("playout failed"^e)

    let getActionFromNode = function
        | {actNode= N {action;_};_} -> action
        | _ -> raise @@ Invalid_argument "can't get action from ROOT"

    let rec retropagation node score =
        node.base.win <- node.base.win +. score;
        node.base.visit <- node.base.visit +. 1.;
        match node.actNode with
            | R -> ()
            | N {action; father} -> retropagation father (1. -. score)

    let expend node ai =
        match node.base.possAct with
            | x :: xs -> node.base.possAct <- xs;
                let b = (try (performAction ai.mctArray (node.base.turn mod 2) x) with Invalid_argument e ->
                                        prerr_endline @@Printf.sprintf "errExpendNode : %s\nfather : %s" (nodeString node) (fatherString node);
                                        prerr_endline @@ Printf.sprintf "actions : %s" @@ stringList (x::xs);
                                        raise (Invalid_argument ("exepend failed, " ^ e )))
                in
                let possAct =  getActions (node.base.turn+1) ai.mctArray in
                let newN = {base={id= !newId;win=0.; visit=0.; turn=node.base.turn+1;sons=[];
                    possAct}; actNode = N {action=x; father=node}} in
                incr newId;
                node.base.sons <- newN :: node.base.sons;
                if b then (
                    retropagation newN 1.
                ) else retropagation newN @@ playout newN.base.turn ai
            | _ ->  raise End_game

    let performActionNode arr = function
        | {actNode=N {action;_};base={turn; _}} as n -> begin try (performAction arr (1 - turn mod 2) action)
            with Invalid_argument e -> raise @@ Invalid_argument ("performNode failed at \n"^nodeString n ^ e)
            end
        | _ -> false

    let ucb1 node = let nf = (match node.actNode with | N {father={base={visit;_};_}} -> visit) in
         node.base.win /. node.base.visit +. (2. *. log nf /. node.base.visit)**0.5

    let rec select node ai =
        if performActionNode ai.mctArray node then retropagation node 1. else (
            match node.base.possAct, node.base.sons with
                | [], [] -> retropagation node 0.5
                | [], x :: xs -> let _, n = List.fold_left
                    (fun ((accS, accN) as acc) n -> let s = ucb1 n in if s > accS then s, n else acc) (ucb1 x, x) xs
                    in select n ai
                | _ -> expend node ai
        )

    let debugNode n =
        prerr_endline @@ Printf.sprintf "win : %.2f | visit : %.2f | rate : %.3f | action : %d"
            n.base.win n.base.visit  (n.base.win /. n.base.visit) (try(getActionFromNode n) with Invalid_argument _ -> -66)

    let debugTree node =
        let rec aux node =
            prerr_endline @@ nodeString node;
            match node.base.sons with
            | x :: xs -> aux x
            | [] -> ()
        in
            (prerr_endline "___start MCTS leaf___ ";aux node; prerr_endline "________")

    let mctSearch turn stopTime ai =
        let debug = ref 0. in
        playouWinDebug := true;

        while Sys.time () < stopTime do
            resetMct ai;
            select !(ai.mcTree) ai;
            debug := 1. +. !debug
        done;
        debugTree !(ai.mcTree);
        print_endline @@ Printf.sprintf "win rate : %.3f" @@ 1. -. !(ai.mcTree).base.win /. !(ai.mcTree).base.visit;
        (
        match !(ai.mcTree).base.sons with
            | x :: xs as l ->
                List.iter debugNode l;
                let w, n = List.fold_left
                    (fun ((accS, accN) as acc) n -> let s = n.base.win /. n.base.visit in if s > accS then s, n else acc)
                    (x.base.win/. x.base.visit, x) xs
                in
                prerr_endline "\nchosen node :";
                debugNode n;
                prerr_endline @@ Printf.sprintf "\nPLAYOUTS : %.0f" !debug;
                let action = getActionFromNode n in
                begin
                try (
                let _ = performAction ai.gameArray !myId action in ()
                ) with Invalid_argument e -> raise @@ Invalid_argument ("perform chosen node failed"^e)
                end;
                ai.mcTree := {base=n.base; actNode = R};
                action, !debug, w

            | _ -> failwith "no sons for root"
            )





    (**[eval maxTime turnindex oppAction] *)
    let eval maxTime turnindex oppAction ai =
        let myid = turnindex mod 2 in
        let oppId = 1 - myid in
        myId := myid;
        let t = Sys.time () in

        if turnindex > 0 then (
            let _ = performAction ai.gameArray oppId oppAction
            in

            ai.mcTree := (try (let [x] = List.filter (fun x -> getActionFromNode x = oppAction) !(ai.mcTree).base.sons
            in {base=x.base; actNode=R}) with Match_failure _ -> newId := 1;
             {base = {id=0; win=0. ; visit=0. ; turn=turnindex; sons=[]; possAct=getActions 0 ai.gameArray}; actNode=R});

        );
        prerr_endline "starting mcts...";
        let a, d, w = mctSearch turnindex (t +. maxTime) ai in
        (* Write an action using print_endline *)
        (* To debug: prerr_endline "Debug message"; *)


        (* Output a column index to drop the chip in. Append message to show in the viewer. *)
        print_endline @@ Printf.sprintf "%.1f %% win rate, %.0f playouts !" (w*.100.) d ;
        a, w
    let init ai =
        for i = 0 to 6 do
            mapInPlace (fun _ -> -1) ai.gameArray.(i)
        done
end;;

#load "graphics.cma";;
exception Invalid_action of int;;
open Graphics;;
type player = Human | MCTS of AI.mctsearcher*float | Adaptative_mcts of AI.mctsearcher;;
let create_mcts_player t = MCTS (AI.createAI(),t);;
let create_adaptative_mcts () = Adaptative_mcts (AI.createAI());;
let arrow color = 
    let r = Array.make_matrix 60 40 transp in
    for x = 15 to 25 do
       for y = 0 to 35 do 
           r.(y).(x) <- color
        done
    done;
    for x = 0 to 39 do
        for y = 0 to 25-abs(x-20)-10 do
            r.(35+y).(x) <- color
        done
    done;
        
    make_image r;;
let draw_arrow x y color = 
    draw_image (arrow color) (x-20) (y-30);;


let gameArray = Array.make_matrix 7 9 (-1);;

    
let toChar = function | 0 -> '0' | 1 -> '1' | -1 -> '.' | e -> failwith @@ "unexpected int action : "^string_of_int e
    ;;
let printGame arr =
            let rec aux i = if i=7 then () else (aux (i+1);
                prerr_endline @@ Array.fold_left (fun acc x -> acc ^ Printf.sprintf " %c |" (toChar x))"" arr.(i))
                in aux 0;;
let initGame() = close_graph(); open_graph "1300x650";
Array.iter (fun arr -> Array.iteri (fun j _ -> arr.(j) <- -1) arr) gameArray;
set_line_width 5;
set_color black;
for k = 0 to 9 do 
    let x = (50+k*(1200/9)) in
    moveto x 10;
    lineto x @@ 10 + 7*(650/8);
    let y = 10 + k*(650/8) in
       if k<8 then (
    moveto 50 y;
    lineto 1250 y);
done
;;
initGame();;
let getColor player = if player = 1 then red else rgb 255 176 0;;
let updateGame arr = 
    Array.iteri (fun i a -> Array.iteri (fun j x -> if x > -1 then (
        set_color @@ getColor x;
        fill_circle (50 + (2*j+1)*1200/18) 
            (10 + (2*i+1)*650/16) 30
        ) ;
    ) a) arr;;
let replace arr =
    let rec aux arr i = if i>=9 then (prerr_endline "failed replace on :"; printGame arr; raise @@ Invalid_argument "can't replace") else
        let x =  arr.(0).(i) in
            if x > -1 then arr.(0).(i) <- 1-x else aux arr (i+1)
            in aux arr 0;;

let exist i j = i>=0 && j>=0 && i < 7 && j < 9;;

let add arr player j =
    let rec aux i =
        if arr.(i).(j) = -1 then (arr.(i).(j) <- player; i) else aux (i+1)
    in try (aux 0) with Invalid_argument _ ->(prerr_endline "error at ADD : "; printGame arr;
    raise @@ Invalid_action j)
    let inv (x,y) = -x,-y
    ;;
    let tests = [(0,1);(1,1);(1,-1);(1,0)];;
let check arr i j player =
    let rec aux ((dirl, dirc) as dir) l c depth rev =

        if exist l c && depth<4 && arr.(l).(c) = player then (
            (aux dir (l+dirl) (c+dirc) (depth+1) rev
            )
        )else if rev then aux (inv dir) (i-dirl) (j-dirc) (depth) 
            false 
        else (assert(depth<=4); depth=4)
        in List.fold_left (fun acc dir -> acc ||  aux dir i j 0 true) false tests;;

let performAction arr player = function
    | -2 -> begin
                try (replace arr; false) with  _ -> failwith "replace failed"
            end
    | (j:int) -> try (let i = add arr player j in check arr i j player;) with Invalid_argument e -> raise @@ Invalid_argument  ("add/check failed"^e)
    ;;
let players = [|Human;Human|];;

let debugPos() = let x, _ = mouse_pos() in 
    let r =                  min (max 0 @@ (x-50)/(1200/9)) 8 in 
        set_color white;
        fill_rect 600 595 1000 1000;
    moveto 600 595;
    set_color black;
    draw_string @@ string_of_int r ^ "pos : " ^ string_of_int x;;
let rec getHuman() = 
    moveto 20 595;
    set_text_size 50;
    set_color black;
    draw_string "press space to play";
    while read_key() <> ' ' do () done;
    while key_pressed() do () done;

    set_color white;
    fill_rect 20 595 600 650;
    
    let rec aux prec = 

        let x, _ = mouse_pos() in 
        let column = min (max 0 @@ (x-50)/(1200/9)) 8  in
        if column != prec then         
            draw_arrow (prec*(1200/9)+1200/9/2+50) 630 white;
            if button_down() then (
            draw_arrow (prec*(1200/9)+1200/9/2+50) 630 white;
             column
             )
        else (
            let x = min (max 0 @@ (x-50)/(1200/9)) 8 in 
                if column != prec then
                draw_arrow (x*(1200/9)+1200/9/2+50) 630 (rgb 20 20 255); aux column
            )
    in aux 0;;
let getMCTS t turn oppAction ai = 
    let a, w = AI.eval t turn oppAction ai in
    moveto 1000 595;
    set_text_size 25;
    set_color white;
    let s = Printf.sprintf "winrate : %.1f %%" (w*.100.) in 
    let w,h = text_size s in 
    fill_rect 1000 595 w h;
    set_color black;
    draw_string s;
    a
    ;;


let rec play turn lastMove players start_time last_turn_duration = 
 
    let player = players.(turn mod 2) in 
    let opponent = players.(1 - turn mod 2) in 
    let action =  
        match player with
        | Human -> if turn > 0 && opponent = Human then (let _ = wait_next_event [Button_up] in ()); getHuman() 
        | MCTS (ai,ti) -> getMCTS ti turn lastMove ai
        | Adaptative_mcts ai -> getMCTS last_turn_duration turn lastMove ai
        | _ -> failwith "not impl" 
    in
    try (
    if performAction gameArray (turn mod 2) action
    then (
        updateGame gameArray;
        set_text_size 100;
        moveto 50 300;
        let s = Printf.sprintf "player %d win" (turn mod 2) in
        let w,h = text_size s in  
        set_color white;
        fill_rect 50 300 w h;
        set_color @@ getColor (turn mod 2);
        draw_string s;
    ) else (
        updateGame gameArray;   
        play (turn + 1) action players (Sys.time()) (Sys.time() -. start_time)
    )
    ) with Invalid_action a ->( 
        let s = Printf.sprintf "Invalid action : %d" a in 
            
        set_color white;
        let w, h = text_size s in 
            moveto (1200-w) 595;
            fill_rect (1200-w) 595 w h;
        set_color @@ rgb 199 49 56;
        draw_string s;
        play turn lastMove players start_time (last_turn_duration +. start_time -. Sys.time())
        ) ;;
let initPlayer = function 
    | MCTS (ai,_) -> AI.init ai
    | _ -> ();;
let rec start_game players = 
    initGame();
    Array.iter initPlayer players;
    play 0 (-1) players (Sys.time()) 2.;
    moveto 10 595;
    set_text_size 50;
    set_color white;
    let s = "press r to replay or anything to close" in 
    let w,h = text_size s in 
    fill_rect 10 595 w h;
    set_color black;
    draw_string s;
    match read_key() with 
    | 'r' -> let temp = players.(0) in players.(0) <- players.(1);
            players.(1) <- temp; start_game players
    | _ -> close_graph()
    ;;

(* [FR] create_mcts_player t créer un bot qui aura t secondes pour choisir son coup (2 secondes minimum conseillé)
 
Human créer un joueur qui devra choisir quel coup jouer à chaque tour

create_adaptative_mcts() créé un bot qui se donne autant de temps que son adversaire au dernier tour
 pour réfléchir à son prochain coup. 2s pour le premier tour*)
 
 
 (* [EN] create_mcts_player t create a bot which will have to chose its action in t seconds
 (2 seconds min if you want to have a pretty good AI)
 
 Human create a player which will chose what move to do every turn
 
 create_adaptative_mcts() create a bot which will have to play in the same amount of time
 as it's opponent took in the previous turn (2s for first turn) *)
 
 

 start_game [|Human; Human|] (* local multiplayer*);;
start_game [|Human; create_adaptative_mcts()|] (* Human VS adaptative AI*);;
 start_game [|Human; create_mcts_player 2.|] (* Human VS AI *);;
start_game [|create_mcts_player 2.;create_mcts_player 10.|] (* fast AI versus slow AI *) ;;




