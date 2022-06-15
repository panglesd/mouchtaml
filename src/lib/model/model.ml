type color = Heart | Spades | Diamonds | Clubs

type number =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Height
  | Nine
  | Ten
  | Jack
  | Queen
  | King

let string_of_color (color : color) =
  match color with
  | Heart -> "♥"
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let string_of_number (number : number) =
  match number with
  | Ace -> "A"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Height -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"

type card = number * color

let string_of_card ((n, c) : card) =
  Printf.sprintf "%s of %s" (string_of_number n) (string_of_color c)

let is_ace card = match card with Ace, _ -> true | _ -> false

type stack = card list

let string_of_stack s = s |> List.map string_of_card |> String.concat " "

type line = stack * stack * stack

let string_of_center c = match c with [] -> "  " | c :: _ -> string_of_card c

let string_of_line (s1, s2, s3) =
  Printf.sprintf "%s  %s  %s" (string_of_stack s1) (string_of_center s2)
    (string_of_stack @@ List.rev s3)

type state = {
  moustache : stack * stack;
  board : line * line * line * line;
  stage : int;
}

let map4 f (a, b, c, d) = (f a, f b, f c, f d)
let iter4 f n = ignore @@ map4 f n
let list_of_4 (a, b, c, d) = [ a; b; c; d ]

let fold_left_map_4 f acc (a, b, c, d) =
  let acc, a = f acc a in
  let acc, b = f acc b in
  let acc, c = f acc c in
  let acc, d = f acc d in
  (acc, (a, b, c, d))

let print_state state =
  let m1, m2 = state.moustache in
  let lines = state.board in
  Printf.printf "%s    %s\n" (string_of_stack m1) (string_of_stack m2);
  iter4 (fun line -> Printf.printf "%s\n" (string_of_line line)) lines

let int_of_number number =
  match number with
  | Ace -> 0
  | Two -> 1
  | Three -> 2
  | Four -> 3
  | Five -> 4
  | Six -> 5
  | Seven -> 6
  | Height -> 7
  | Nine -> 8
  | Ten -> 9
  | Jack -> 10
  | Queen -> 11
  | King -> 12

let compatible ((n1, c1) : card) ((n2, c2) : card) =
  c1 = c2 && Int.abs (int_of_number n1 - int_of_number n2) = 1

let compatible_to_mount ((n1, c1) : card) ((n2, c2) : card) =
  c1 = c2 && int_of_number n1 - int_of_number n2 = 1

type line_coordinate = One | Two | Three | Four

let mapi_4 f (a, b, c, d) = (f One a, f Two b, f Three c, f Four d)

let get_line_at_coordinate state (v_coord : line_coordinate) =
  match (state.board, v_coord) with
  | (a, _, _, _), One -> a
  | (_, b, _, _), Two -> b
  | (_, _, c, _), Three -> c
  | (_, _, _, d), Four -> d

type horizontal_coordinate = Left | Right

let invert h = match h with Left -> Right | Right -> Left
let get_center_from_line (_, c, _) = c

let get_center_at_coordinate state v_coord =
  let l = get_line_at_coordinate state v_coord in
  get_center_from_line l

type moustache_coordinate = horizontal_coordinate

let get_moustache_at_coordinate state h_coord =
  match h_coord with
  | Left -> fst state.moustache
  | Right -> snd state.moustache

type board_coordinate = line_coordinate * horizontal_coordinate

let get_stack_at_board_coordinate state (v_coord, h_coord) =
  let l1, _, l2 = get_line_at_coordinate state v_coord in
  match h_coord with Left -> l1 | Right -> l2

type stack_coordinate =
  | Moustache of horizontal_coordinate
  | Board of board_coordinate

let get_stack_at_stack_coordinate state coordinate =
  match coordinate with
  | Moustache c -> get_moustache_at_coordinate state c
  | Board c -> get_stack_at_board_coordinate state c

type coordinate = Stack of stack_coordinate | Center of line_coordinate

let get_stack_at_coordinate state coordinate =
  match coordinate with
  | Stack coordinate -> get_stack_at_stack_coordinate state coordinate
  | Center v_coord -> get_center_at_coordinate state v_coord

let get_moustaches state = state.moustache

let get_lines state =
  match state.board with
  | a, b, c, d -> ((One, a), (Two, b), (Three, c), (Four, d))

type move = stack_coordinate * coordinate

let check_move ((s_coord, coord) : move) state =
  let s_init = get_stack_at_stack_coordinate state s_coord in
  match s_init with
  | [] -> false
  | card_init :: _ -> (
      match coord with
      | Stack (Moustache c) -> (
          match get_moustache_at_coordinate state c with
          | [] -> true
          | card_to :: _ -> compatible card_init card_to)
      | Stack (Board c) -> (
          match get_stack_at_board_coordinate state c with
          | [] -> false
          | card_to :: _ -> compatible card_init card_to)
      | Center c -> (
          match get_center_at_coordinate state c with
          | [] -> is_ace card_init
          | card_to :: _ -> compatible_to_mount card_init card_to))

let set (a, b, c, d) (h : line_coordinate) x =
  match h with
  | One -> (x, b, c, d)
  | Two -> (a, x, c, d)
  | Three -> (a, b, x, d)
  | Four -> (a, b, c, x)

let pop_card (s_coord : stack_coordinate) state =
  match s_coord with
  | Moustache Left ->
      let m1, m2 = state.moustache in
      let c = List.hd m1 and m1 = List.tl m1 in
      (c, { state with moustache = (m1, m2) })
  | Moustache Right ->
      let m1, m2 = state.moustache in
      let c = List.hd m2 and m2 = List.tl m2 in
      (c, { state with moustache = (m1, m2) })
  | Board (i, Left) ->
      let l1, center, l2 = get_line_at_coordinate state i in
      let c = List.hd l1 and l1 = List.tl l1 in
      let board = set state.board i (l1, center, l2) in
      (c, { state with board })
  | Board (i, Right) ->
      let l1, center, l2 = get_line_at_coordinate state i in
      let c = List.hd l2 and l2 = List.tl l2 in
      let board = set state.board i (l1, center, l2) in
      (c, { state with board })

let push_card (coord : coordinate) (card, state) =
  (* Printf.printf "Pushing %s\n" (string_of_card card); *)
  match coord with
  | Stack (Moustache Left) ->
      let m1, m2 = state.moustache in
      let m1 = card :: m1 in
      { state with moustache = (m1, m2) }
  | Stack (Moustache Right) ->
      let m1, m2 = state.moustache in
      let m2 = card :: m2 in
      { state with moustache = (m1, m2) }
  | Stack (Board (i, Left)) ->
      let l1, center, l2 = get_line_at_coordinate state i in
      let board = set state.board i (card :: l1, center, l2) in
      { state with board }
  | Stack (Board (i, Right)) ->
      let l1, center, l2 = get_line_at_coordinate state i in
      let board = set state.board i (l1, center, card :: l2) in
      { state with board }
  | Center i ->
      let l1, center, l2 = get_line_at_coordinate state i in
      let board = set state.board i (l1, card :: center, l2) in
      { state with board }

let do_move_opt ((s_coord, coord) as m : move) state =
  if check_move m state then Some (state |> pop_card s_coord |> push_card coord)
  else None

let do_move m state =
  match do_move_opt m state with
  | Some s -> s
  | None -> failwith "move impossible"

let do_move_i_opt m i state =
  let moves = List.init i (fun _ -> m) in
  List.fold_left
    (fun state move -> Option.bind state (do_move_opt move))
    (Some state) moves

let do_move_i m i state =
  match do_move_i_opt m i state with
  | Some s -> s
  | None -> failwith "move impossible"

let check_move_i m i state = Option.is_some @@ do_move_i_opt m i state
let _ = ()

let next_stack_coordinate (stack_coordinate : stack_coordinate)
    ~include_moustache =
  match stack_coordinate with
  | Moustache h -> Board (One, h)
  | Board (Four, h) when include_moustache -> Moustache (invert h)
  | Board (Four, h) -> Board (One, invert h)
  | Board (One, h) -> Board (Two, h)
  | Board (Two, h) -> Board (Three, h)
  | Board (Three, h) -> Board (Four, h)

let first_coordinate ~include_moustache =
  if include_moustache then Moustache Left else Board (One, Left)

let list_product l1 l2 =
  List.concat_map (fun e -> List.map (fun f -> (e, f)) l2) l1

let deck =
  let all_numbers =
    [
      Ace;
      Two;
      Three;
      Four;
      Five;
      Six;
      Seven;
      Height;
      Nine;
      Ten;
      Jack;
      Queen;
      King;
    ]
  and all_colors = [ Spades; Heart; Diamonds; Clubs ] in
  List.map
    (fun color -> List.map (fun number -> (number, color)) all_numbers)
    all_colors
  |> List.concat

let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let create_state () =
  Random.self_init ();
  let state =
    {
      moustache = ([], []);
      board = (([], [], []), ([], [], []), ([], [], []), ([], [], []));
      stage = 1;
    }
  in
  let deck = shuffle deck in
  let has_empty_center (_, c, _) = List.length c = 0 in
  let _, state =
    List.fold_left
      (fun (coord, state) card ->
        match (is_ace card, coord) with
        | true, Board (v, _)
          when has_empty_center @@ get_line_at_coordinate state v ->
            (coord, push_card (Center v) (card, state))
        | _ ->
            ( next_stack_coordinate coord ~include_moustache:true,
              push_card (Stack coord) (card, state) ))
      (first_coordinate ~include_moustache:true, state)
      deck
  in
  (* print_state state; *)
  state

let create_state_filled n =
  Random.self_init ();

  let all_numbers =
    [
      Ace;
      Two;
      Three;
      Four;
      Five;
      Six;
      Seven;
      Height;
      Nine;
      Ten;
      Jack;
      Queen;
      King;
    ]
  and all_colors = [ Spades; Heart; Diamonds; Clubs ]
  and all_colors4 = (Spades, Heart, Diamonds, Clubs) in
  let first_numbers, last_numbers =
    let rec get l i =
      match l with
      | [] -> ([], [])
      | l when i = 0 -> ([], l)
      | a :: q ->
          let x, y = get q (i - 1) in
          (a :: x, y)
    in
    get all_numbers n
  in
  let deck = list_product last_numbers all_colors in
  let c1, c2, c3, c4 =
    map4 (fun c -> List.map (fun n -> (n, c)) first_numbers) all_colors4
  in
  let state =
    {
      moustache = ([], []);
      board =
        ( ([], List.rev c1, []),
          ([], List.rev c2, []),
          ([], List.rev c3, []),
          ([], List.rev c4, []) );
      stage = 1;
    }
  in
  let deck = shuffle deck in
  let has_empty_center (_, c, _) = List.length c = 0 in
  let _, state =
    List.fold_left
      (fun (coord, state) card ->
        match (is_ace card, coord) with
        | true, Board (v, _)
          when has_empty_center @@ get_line_at_coordinate state v ->
            (coord, push_card (Center v) (card, state))
        | _ ->
            ( next_stack_coordinate coord ~include_moustache:true,
              push_card (Stack coord) (card, state) ))
      (first_coordinate ~include_moustache:true, state)
      deck
  in
  print_state state;
  state

let collect_deck state =
  fold_left_map_4
    (fun acc (l1, c, l2) -> (l1 @ l2 @ acc, ([], c, [])))
    [] state.board

let next_stage state =
  let collect_deck, board = collect_deck state in
  let collect_deck = shuffle collect_deck in
  let state = { state with board; stage = state.stage + 1 } in
  let _, state =
    List.fold_left
      (fun (coord, state) card ->
        ( next_stack_coordinate coord ~include_moustache:false,
          push_card (Stack coord) (card, state) ))
      (first_coordinate ~include_moustache:false, state)
      collect_deck
  in
  state

let list_line_coordinate = [ One; Two; Three; Four ]
let list_horizontal_coordinate = [ Left; Right ]

let list_stack_coordinate =
  List.map (fun c -> Moustache c) list_horizontal_coordinate
  @ List.map
      (fun c -> Board c)
      (list_product list_line_coordinate list_horizontal_coordinate)

let list_coordinate =
  List.map (fun c -> Center c) list_line_coordinate
  @ List.map (fun c -> Stack c) list_stack_coordinate

let list_move = list_product list_stack_coordinate list_coordinate

let list_mounting_move =
  list_product list_stack_coordinate
    (List.map (fun c -> Center c) list_line_coordinate)

let list_next_state state =
  List.filter_map (fun move -> do_move_opt move state) list_move

let is_a_win state =
  let collected_deck, _ = collect_deck state in
  let m1, m2 = get_moustaches state in
  match m1 @ m2 @ collected_deck with [] -> true | _ -> false

(* type state_value = Winning | Losing | Proba of float | In_progress *)

type game = Winning | Losing

let solved = Hashtbl.create 10

let rec mount_all state =
  match
    List.find_map (fun move -> do_move_opt move state) list_mounting_move
  with
  | Some state -> mount_all state
  | None -> state

let solve state =
  let proccessed = Hashtbl.create 10 in
  let rec solve state =
    let state = mount_all state in
    if Hashtbl.mem proccessed state then None
    else (
      Hashtbl.add proccessed state ();
      match Hashtbl.find_opt solved state with
      | None ->
          let res = process state in
          Option.iter (Hashtbl.add solved state) res;
          res
      | Some Winning -> Some Winning
      | Some Losing -> Some Losing)
  and process state =
    if is_a_win state then (
      Hashtbl.add solved state Winning;
      Some Winning)
    else
      let list_next_state = list_next_state state in
      match
        List.fold_left
          (fun acc state ->
            match acc with
            | Some Winning -> Some Winning
            | None -> (
                match solve state with
                | Some Winning -> Some Winning
                | _ -> None)
            | Some Losing -> solve state)
          (Some Losing) list_next_state
      with
      | Some f as res ->
          Hashtbl.add solved state f;
          res
      | _ as res -> res
  in
  let res = solve state in
  Printf.printf "Size of the hashtable: %d\n"
    (Hashtbl.stats solved).num_bindings;
  match res with
  | Some Winning -> true
  | Some Losing -> false
  | None ->
      Hashtbl.add solved state Losing;
      false
