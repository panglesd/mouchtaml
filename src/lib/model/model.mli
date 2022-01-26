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

type card = number * color

val is_ace : card -> bool

type stack = card list

type line = stack * stack * stack

type state = {
  moustache : stack * stack;
  board : line * line * line * line;
  stage : int;
}
(* type state *)

val list_of_4 : 'a * 'a * 'a * 'a -> 'a list

type line_coordinate

val get_line_at_coordinate : state -> line_coordinate -> line

type horizontal_coordinate = Left | Right

val invert : horizontal_coordinate -> horizontal_coordinate

val get_center_from_line : line -> stack

val get_center_at_coordinate : state -> line_coordinate -> stack

type moustache_coordinate

val get_moustache_at_coordinate : state -> moustache_coordinate -> stack

type board_coordinate = line_coordinate * horizontal_coordinate

val get_stack_at_board_coordinate : state -> board_coordinate -> stack

type stack_coordinate =
  | Moustache of horizontal_coordinate
  | Board of board_coordinate

val get_stack_at_stack_coordinate : state -> stack_coordinate -> stack

type coordinate = Stack of stack_coordinate | Center of line_coordinate

val get_stack_at_coordinate : state -> coordinate -> stack

val get_moustaches : state -> stack * stack

val get_lines :
  state ->
  (line_coordinate * line)
  * (line_coordinate * line)
  * (line_coordinate * line)
  * (line_coordinate * line)

type move = stack_coordinate * coordinate

val check_move : move -> state -> bool

val pop_card : stack_coordinate -> state -> card * state

val push_card : coordinate -> card * state -> state

val do_move : move -> state -> state

val next_stack_coordinate :
  stack_coordinate -> include_moustache:bool -> stack_coordinate

val first_coordinate : include_moustache:bool -> stack_coordinate

val deck : card list

val shuffle : card list -> card list

val create_state : unit -> state

val create_state_filled : int -> state

val next_stage : state -> state

val mapi_4 :
  (line_coordinate -> 'a -> 'b) -> 'a * 'a * 'a * 'a -> 'b * 'b * 'b * 'b

val map4 : ('a -> 'b) -> 'a * 'a * 'a * 'a -> 'b * 'b * 'b * 'b

val list_next_state : state -> state list

val solve : state -> bool
