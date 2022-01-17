open Js_of_ocaml
open Tyxml_lwd
(* open Lwdom *)

let js = Js.string

let state = Lwd.var @@ Model.create_state ()

(* let holded_card = ref None *)

let holded_coord = ref Model.(Moustache Left)

let string_of_color (color : Model.color) =
  match color with
  | Model.Heart -> "Hearts"
  | Model.Spades -> "Spades"
  | Model.Diamonds -> "Diamonds"
  | Model.Clubs -> "Clubs"

let string_of_number (number : Model.number) =
  match number with
  | Model.Ace -> "A"
  | Model.Two -> "2"
  | Model.Three -> "3"
  | Model.Four -> "4"
  | Model.Five -> "5"
  | Model.Six -> "6"
  | Model.Seven -> "7"
  | Model.Height -> "8"
  | Model.Nine -> "9"
  | Model.Ten -> "10"
  | Model.Jack -> "J"
  | Model.Queen -> "Q"
  | Model.King -> "K"

let img_of_card ?coord ((n, c) as _card : Model.card) =
  let src, alt =
    let cs = string_of_color c and ns = string_of_number n in
    ( Lwd.pure @@ "../assets/cards/" ^ cs ^ "/" ^ ns ^ ".png",
      Lwd.pure @@ ns ^ " of " ^ cs )
  in
  Html.img
    ~a:
      [
        Html.a_class @@ Lwd.pure [ "card" ];
        Html.a_draggable @@ Lwd.pure true;
        Html.a_ondragstart @@ Lwd.pure
        @@ Some
             (fun _ ->
               (* holded_card := Some card; *)
               (match coord with
               | Some coord -> holded_coord := coord
               | None -> ());
               true);
      ]
    ~src ~alt ()

let div_of_stack ~coord stack =
  let horiz_coord =
    match coord with Model.Moustache c -> c | Board (_, c) -> c
  in
  let l = List.map (fun c -> img_of_card ~coord c) stack in
  let x = Lwd.var false in
  let a = Lwd.get x in
  let class_ =
    let open Lwd_infix in
    let$ a = a in
    (if a then [ "dragged_over" ] else [])
    @
    match horiz_coord with
    | Model.Left -> [ "stack"; "left" ]
    | Model.Right -> [ "stack"; "right" ]
  in
  Html.div
    ~a:
      [
        Html.a_class @@ class_;
        Html.a_ondragover @@ Lwd.pure
        @@ Some
             (fun e ->
               ignore e##.clientX;
               if
                 Model.check_move
                   (!holded_coord, Model.Stack coord)
                   (Lwd.peek state)
               then Lwd.set x true;
               false);
        (* Html.a_ondragover @@ Lwd.pure *)
        (* @@ Some *)
        (*      (fun _ -> *)
        (*        (\* Lwd.set x true; *\) *)
        (*        false); *)
        Html.a_ondragleave @@ Lwd.pure
        @@ Some
             (fun _ ->
               Lwd.set x false;
               false);
        Html.a_ondrop @@ Lwd.pure
        @@ Some
             (fun _ ->
               if
                 Model.check_move
                   (!holded_coord, Model.Stack coord)
                   (Lwd.peek state)
               then
                 Lwd.set state
                   (Model.do_move
                      (!holded_coord, Model.Stack coord)
                      (Lwd.peek state));
               false);
      ]
    l

let div_of_moustaches (moustache_l, moustache_r) =
  Html.div
    ~a:[ Html.a_class @@ Lwd.pure [ "moustaches" ] ]
    [
      div_of_stack ~coord:Model.(Moustache Left) moustache_l;
      div_of_stack ~coord:Model.(Moustache Right) moustache_r;
    ]

let div_of_line ~coord ((left, center, right) : Model.line) =
  let left = div_of_stack ~coord:(Board (coord, Left)) left
  and right = div_of_stack ~coord:(Board (coord, Right)) right in
  let x = Lwd.var false in
  let a = Lwd.get x in
  let class_ =
    let open Lwd_infix in
    let$ a = a in
    (if a then [ "dragged_over" ] else []) @ [ "center" ]
  in
  let a =
    [
      Html.a_class class_;
      Html.a_ondragover @@ Lwd.pure
      @@ Some
           (fun e ->
             ignore e##.clientX;
             if
               Model.check_move
                 (!holded_coord, Model.Center coord)
                 (Lwd.peek state)
             then Lwd.set x true;
             false);
      Html.a_ondrop @@ Lwd.pure
      @@ Some
           (fun _ ->
             if
               Model.check_move
                 (!holded_coord, Model.Center coord)
                 (Lwd.peek state)
             then
               Lwd.set state
                 (Model.do_move
                    (!holded_coord, Model.Center coord)
                    (Lwd.peek state));
             false);
    ]
  in
  let center =
    match center with
    | [] -> Html.div ~a []
    | c :: _ -> Html.div ~a [ img_of_card c ]
  in
  Html.div ~a:[ Html.a_class @@ Lwd.pure [ "line" ] ] [ left; center; right ]

let div_of_board state =
  let lines = Model.get_lines state in
  let l =
    Model.list_of_4 @@ Model.map4 (fun (coord, l) -> div_of_line ~coord l) lines
  in
  Html.div ~a:[ Html.a_class @@ Lwd.pure [ "board" ] ] l

(* match card with *)
(*   | (_, _) -> _ *)

let button () =
  Html.button
    ~a:
      [
        Html.a_onclick @@ Lwd.pure
        @@ Some
             (fun _ ->
               Lwd.set state (Model.next_stage (Lwd.peek state));
               false);
      ]
    [ Html.txt @@ Lwd.pure "Next stage" ]

let onload _ =
  let main =
    Js.Opt.get
      (Dom_html.window##.document##getElementById (js "main"))
      (fun () -> assert false)
  in
  let open Lwd_infix in
  let moustaches =
    let$* state = Lwd.get state in
    let moustaches = Model.get_moustaches state in
    div_of_moustaches moustaches
  in
  let board =
    let$* state = Lwd.get state in
    div_of_board state
  in
  let doc = Html.div [ moustaches; board; button () ] in
  (*let root = Lwd.observe (Lwdom.to_fragment doc) in*)
  let (_ : Lwdom.Scheduler.job) = Lwdom.Scheduler.append_to_dom doc main in
  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload